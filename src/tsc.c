#include "tsc.h"

#include "spirv.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Basics {{{

// Rounds to the next multiple of four
#define ROUND_TO_4(x) (((x) + 3) & ~0x03)

#define NEW(compiler, type) bumpZeroAlloc(&(compiler)->alloc, sizeof(type))
#define NEW_ARRAY(compiler, type, count) bumpZeroAlloc(&(compiler)->alloc, sizeof(type) * (count))

static inline bool isLetter(char c)
{
    return (('z' >= c) && (c >= 'a')) || (('Z' >= c) && (c >= 'A')) || c == '_';
}

static inline bool isNumeric(char c)
{
    return ('0' <= c) && ('9' >= c);
}

static inline bool isAlphanum(char c)
{
    return isLetter(c) || isNumeric(c);
}

#define arrFree(a) ((a) ? free(arr__sbraw(a)), 0 : 0)
#define arrPush(a, v) (arr__sbmaybegrow(a, 1), (a)[arr__sbn(a)++] = (v))
#define arrPop(a) (--arr__sbn(a))
#define arrLength(a) ((a) ? arr__sbn(a) : 0)
#define arrAdd(a, n) (arr__sbmaybegrow(a, n), arr__sbn(a) += (n), &(a)[arr__sbn(a) - (n)])
#define arrLast(a) (&((a)[arr__sbn(a) - 1]))

#define arr__sbraw(a) ((uint32_t *)(void *)(a)-2)
#define arr__sbm(a) arr__sbraw(a)[0]
#define arr__sbn(a) arr__sbraw(a)[1]

#define arr__sbneedgrow(a, n) ((a) == 0 || arr__sbn(a) + (n) >= arr__sbm(a))
#define arr__sbmaybegrow(a, n) (arr__sbneedgrow(a, (n)) ? arr__sbgrow(a, n) : 0)
#define arr__sbgrow(a, n) (*((void **)&(a)) = arr__sbgrowf((a), (n), sizeof(*(a))))

static void *arr__sbgrowf(void *arr, uint32_t increment, uint32_t itemsize)
{
    uint32_t dbl_cur = arr ? 2 * arr__sbm(arr) : 0;
    uint32_t min_needed = arrLength(arr) + increment;
    uint32_t m = dbl_cur > min_needed ? dbl_cur : min_needed;
    uint32_t *p =
        (uint32_t *)realloc(arr ? arr__sbraw(arr) : 0, itemsize * m + sizeof(uint32_t) * 2);
    if (p)
    {
        if (!arr) p[1] = 0;
        p[0] = m;
        return p + 2;
    }
    else
    {
        return (void *)(2 * sizeof(uint32_t)); // try to force a NULL pointer exception later
    }
}

static inline void fnvHashReset(uint64_t *hash)
{
    *hash = 14695981039346656037ULL;
}

static inline void fnvHashUpdate(uint64_t *hash, uint8_t *bytes, size_t count)
{
    for (uint64_t i = 0; i < count; ++i)
    {
        *hash = ((*hash) * 1099511628211) ^ bytes[i];
    }
}

static inline uint64_t hashStr(const char *string)
{
    uint64_t hash;
    fnvHashReset(&hash);
    fnvHashUpdate(&hash, (uint8_t *)(string), strlen(string));
    return hash;
}

// Hashmap {{{
#define DEFAULT_HASHMAP_SIZE 32

static inline bool stringEquals(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

typedef struct HashMap
{
    const char **keys;
    uint64_t *hashes;
    uint64_t *indices;
    uint64_t size;

    /*array*/ void **values;
} HashMap;

static void hashGrow(HashMap *map);

static void hashInit(HashMap *map, uint64_t size)
{
    memset(map, 0, sizeof(*map));

    map->size = size;
    if (map->size == 0)
    {
        map->size = DEFAULT_HASHMAP_SIZE;
    }

    // Round up to nearnest power of two
    map->size -= 1;
    map->size |= map->size >> 1;
    map->size |= map->size >> 2;
    map->size |= map->size >> 4;
    map->size |= map->size >> 8;
    map->size |= map->size >> 16;
    map->size |= map->size >> 32;
    map->size += 1;

    // Init memory
    map->keys = malloc(sizeof(*map->keys) * map->size);
    map->hashes = malloc(sizeof(*map->hashes) * map->size);
    map->indices = malloc(sizeof(*map->indices) * map->size);

    memset(map->keys, 0, sizeof(*map->keys) * map->size);
    memset(map->hashes, 0, sizeof(*map->hashes) * map->size);
}

static uint64_t hashSetInternal(HashMap *map, const char *key, uint64_t index)
{
    uint64_t hash = hashStr(key);
    uint64_t i = hash & (map->size - 1);
    uint64_t iters = 0;
    while ((map->hashes[i] != hash || !stringEquals(map->keys[i], key)) && map->hashes[i] != 0 &&
           iters < map->size)
    {
        i = (i + 1) & (map->size - 1);
        iters++;
    }

    if (iters >= map->size)
    {
        hashGrow(map);
        return hashSetInternal(map, key, index);
    }

    map->keys[i] = key;
    map->hashes[i] = hash;
    map->indices[i] = index;

    return index;
}

static inline void *hashSet(HashMap *map, const char *key, void *value)
{
    size_t index = arrLength(map->values);
    arrPush(map->values, value);
    hashSetInternal(map, key, index);
    return map->values[index];
}

static bool hashGet(HashMap *map, const char *key, void **result)
{
    uint64_t hash = hashStr(key);
    uint64_t i = hash & (map->size - 1);
    uint64_t iters = 0;
    while ((map->hashes[i] != hash || !stringEquals(map->keys[i], key)) && map->hashes[i] != 0 &&
           iters < map->size)
    {
        i = (i + 1) & (map->size - 1);
        iters++;
    }
    if (iters >= map->size)
    {
        return false;
    }

    if (map->hashes[i] != 0)
    {
        if (result) *result = map->values[map->indices[i]];
        return true;
    }

    return false;
}

static void hashGrow(HashMap *map)
{
    uint64_t old_size = map->size;
    const char **old_keys = map->keys;
    uint64_t *old_hashes = map->hashes;
    uint64_t *old_indices = map->indices;

    map->size = old_size * 2;
    map->hashes = malloc(sizeof(*map->hashes) * map->size);
    map->indices = malloc(sizeof(*map->indices) * map->size);
    map->keys = malloc(sizeof(*map->keys) * map->size);
    memset(map->hashes, 0, sizeof(*map->hashes) * map->size);
    memset(map->keys, 0, sizeof(*map->keys) * map->size);

    for (uint64_t i = 0; i < old_size; i++)
    {
        if (old_hashes[i] != 0)
        {
            hashSetInternal(map, old_keys[i], old_indices[i]);
        }
    }

    free(old_hashes);
    free(old_indices);
    free(old_keys);
}

static void hashDestroy(HashMap *map)
{
    free(map->hashes);
    free(map->indices);
    free(map->keys);
    arrFree(map->values);
}
// }}}

// Bump allocator {{{
typedef struct BumpBlock
{
    unsigned char *data;
    size_t size;
    size_t pos;
    struct BumpBlock *next;
} BumpBlock;

static void blockInit(BumpBlock *block, size_t size)
{
    block->data = malloc(size);
    block->size = size;
    block->pos = 0;
    block->next = NULL;
}

static void blockDestroy(BumpBlock *block)
{
    if (block->next != NULL)
    {
        blockDestroy(block->next);
        free(block->next);
        block->next = NULL;
    }

    free(block->data);
}

static void *blockAlloc(BumpBlock *block, size_t size)
{
    assert((block->size - block->pos) >= size);
    void *data = block->data + block->pos;
    block->pos += size;
    return data;
}

typedef struct BumpAlloc
{
    size_t block_size;
    size_t last_block_size;
    BumpBlock base_block;
    BumpBlock *last_block;
} BumpAlloc;

static void bumpInit(BumpAlloc *alloc, size_t block_size)
{
    alloc->block_size = block_size;
    alloc->last_block_size = alloc->block_size;
    blockInit(&alloc->base_block, block_size);
    alloc->last_block = &alloc->base_block;
}

static void *bumpAlloc(BumpAlloc *alloc, size_t size)
{
    if (size == 0)
    {
        return NULL;
    }

    size_t space = alloc->last_block->size - alloc->last_block->pos;
    if (space < size)
    {
        // Append new block
        alloc->last_block->next = malloc(sizeof(BumpBlock));
        alloc->last_block_size *= 2;
        alloc->last_block_size += size;
        blockInit(alloc->last_block->next, alloc->last_block_size);
        alloc->last_block = alloc->last_block->next;
    }

    return blockAlloc(alloc->last_block, size);
}

static void *bumpZeroAlloc(BumpAlloc *alloc, size_t size)
{
    void *data = bumpAlloc(alloc, size);
    memset(data, 0, size);
    return data;
}

static char *bumpStrndup(BumpAlloc *alloc, const char *str, size_t length)
{
    char *ptr = bumpAlloc(alloc, length + 1);
    memcpy(ptr, str, length);
    ptr[length] = '\0';
    return ptr;
}

static void bumpDestroy(BumpAlloc *alloc)
{
    blockDestroy(&alloc->base_block);
}
// }}}

// String builder {{{
typedef struct StringBuilder
{
    char *buf;
    char *scratch;
    size_t len;
    size_t cap;
} StringBuilder;

static void sbInit(StringBuilder *sb)
{
    sb->len = 0;
    sb->cap = 1 << 16; // 64k
    sb->buf = malloc(sb->cap);
    sb->scratch = malloc(sb->cap);
}

static void sbDestroy(StringBuilder *sb)
{
    free(sb->buf);
    free(sb->scratch);
}

static void sbReset(StringBuilder *sb)
{
    sb->len = 0;
}

static void sbGrow(StringBuilder *sb)
{
    sb->cap *= 2;
    sb->buf = realloc(sb->buf, sb->cap);
    sb->scratch = realloc(sb->scratch, sb->cap);
}

static void sbAppend(StringBuilder *sb, const char *str)
{
    size_t len = strlen(str);
    while (len + sb->len >= sb->cap)
    {
        sbGrow(sb);
    }
    strncpy(&sb->buf[sb->len], str, len);
    sb->len += len;
}

static void sbAppendChar(StringBuilder *sb, char c)
{
    while (1 + sb->len >= sb->cap)
    {
        sbGrow(sb);
    }
    sb->buf[sb->len] = c;
    sb->len += 1;
}

static void sbSprintf(StringBuilder *sb, const char *fmt, ...)
{
    va_list vl;
    va_start(vl, fmt);
    vsnprintf(sb->scratch, sb->cap, fmt, vl);
    va_end(vl);
    sbAppend(sb, sb->scratch);
}

static char *sbBuildMalloc(StringBuilder *sb)
{
    char *result = malloc(sb->len + 1);
    strncpy(result, sb->buf, sb->len);
    result[sb->len] = '\0';
    return result;
}

static char *sbBuild(StringBuilder *sb, BumpAlloc *bump)
{
    char *result = bumpAlloc(bump, sb->len + 1);
    strncpy(result, sb->buf, sb->len);
    result[sb->len] = '\0';
    return result;
}
// }}}

// }}}

// Types {{{

typedef struct File File;
typedef struct Module Module;

typedef struct Scope Scope;

typedef struct AstExpr AstExpr;
typedef struct AstStmt AstStmt;
typedef struct AstDecl AstDecl;

typedef struct Location
{
    File *file;
    uint32_t pos;
    uint32_t length;
    uint32_t line;
    uint32_t col;
} Location;

typedef struct Error
{
    Location loc;
    const char *message;
} Error;

//
// Token
//

typedef enum TokenKind {
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACK,
    TOKEN_RBRACK,
    TOKEN_LCURLY,
    TOKEN_RCURLY,
    TOKEN_ATTR_LBRACK,
    TOKEN_ATTR_RBRACK,

    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_COLON_COLON,

    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_MOD,

    TOKEN_BITOR,
    TOKEN_BITXOR,
    TOKEN_BITAND,
    TOKEN_BITNOT,

    TOKEN_LSHIFT,
    TOKEN_RSHIFT,

    TOKEN_PERIOD,
    TOKEN_COMMA,

    TOKEN_NOT,    // !
    TOKEN_ASSIGN, // =

    TOKEN_EQUAL,     // ==
    TOKEN_NOTEQ,     // !=
    TOKEN_LESS,      // <
    TOKEN_LESSEQ,    // <=
    TOKEN_GREATER,   // >
    TOKEN_GREATEREQ, // >=

    TOKEN_ADDEQ, // +=
    TOKEN_SUBEQ, // -=
    TOKEN_MULEQ, // *=
    TOKEN_DIVEQ, // /=
    TOKEN_MODEQ, // %=

    TOKEN_BITANDEQ, // &=
    TOKEN_BITOREQ,  // |=
    TOKEN_BITXOREQ, // ^=

    TOKEN_LSHIFTEQ, // <<=
    TOKEN_RSHIFTEQ, // >>=

    TOKEN_AND, // &&
    TOKEN_OR,  // ||

    TOKEN_IDENT,
    TOKEN_DOT,
    TOKEN_IN,
    TOKEN_OUT,
    TOKEN_IMPORT,
    TOKEN_STRUCT,
    TOKEN_FOR,
    TOKEN_WHILE,
    TOKEN_SWITCH,
    TOKEN_CASE,
    TOKEN_DEFAULT,
    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_RETURN,
    TOKEN_CONST,
    TOKEN_CONSTANT_BUFFER,
    TOKEN_SAMPLER_STATE,
    TOKEN_TEXTURE_1D,
    TOKEN_TEXTURE_2D,
    TOKEN_TEXTURE_3D,
    TOKEN_TEXTURE_CUBE,
    TOKEN_SAMPLED_TEXTURE_1D,
    TOKEN_SAMPLED_TEXTURE_2D,
    TOKEN_SAMPLED_TEXTURE_3D,
    TOKEN_SAMPLED_TEXTURE_CUBE,

    TOKEN_INT_LIT,
    TOKEN_FLOAT_LIT,
    TOKEN_STRING_LIT,

    TOKEN_BOOL,

    TOKEN_UINT,
    TOKEN_INT,

    TOKEN_FLOAT2,
    TOKEN_FLOAT3,
    TOKEN_FLOAT4,

    TOKEN_FLOAT2X2,
    TOKEN_FLOAT3X3,
    TOKEN_FLOAT4X4,

    TOKEN_FLOAT,

    TOKEN_VOID,

    TOKEN_FALSE,
    TOKEN_TRUE,
} TokenKind;

typedef struct Token
{
    TokenKind kind;
    Location loc;
    union
    {
        char *str;
        double double_;
        int64_t int_;
    };
} Token;

//
// Type
//

typedef enum TypeKind {
    TYPE_VOID,
    TYPE_TYPE,

    TYPE_BOOL,
    TYPE_FLOAT,
    TYPE_INT,

    TYPE_VECTOR,
    TYPE_MATRIX,

    TYPE_POINTER,
    TYPE_FUNC,
    TYPE_STRUCT,

    TYPE_SAMPLER,
    TYPE_IMAGE,
    TYPE_SAMPLED_IMAGE,
} TypeKind;

typedef struct Type
{
    TypeKind kind;
    char *string;
    uint32_t id;
    union
    {
        struct
        {
            uint32_t bits;
        } float_;
        struct
        {
            uint32_t bits;
            bool is_signed;
        } int_;
        struct
        {
            SpvStorageClass storage_class;
            struct Type *sub;
        } ptr;
        struct
        {
            uint32_t size;
            struct Type *elem_type;
        } vector;
        struct
        {
            uint32_t col_count;
            struct Type *col_type;
        } matrix;
        struct
        {
            struct Type *return_type;

            struct Type **params;
            uint32_t param_count;
        } func;
        struct
        {
            char *name;

            AstDecl **field_decls;
            struct Type **fields;
            uint32_t field_count;
        } struct_;
        struct
        {
            struct Type *sampled_type;
            SpvDim dim;
            uint32_t depth;
            uint32_t arrayed;
            uint32_t multisampled;
            uint32_t sampled;
            SpvImageFormat format;
        } image;
        struct
        {
            struct Type *image_type;
        } sampled_image;
    };
} Type;

//
// IR
//

typedef struct IRModule IRModule;
typedef struct IRInst IRInst;

typedef struct IRDecoration
{
    SpvDecoration kind;
    union
    {
        uint32_t value;
    };
} IRDecoration;

typedef enum IRInstKind {
    IR_INST_ENTRY_POINT,
    IR_INST_FUNCTION,
    IR_INST_BLOCK,
    IR_INST_FUNC_PARAM,
    IR_INST_VARIABLE,
    IR_INST_CONSTANT,
    IR_INST_RETURN,
    IR_INST_STORE,
    IR_INST_LOAD,
    IR_INST_ACCESS_CHAIN,
    IR_INST_FUNC_CALL,

    IR_INST_BUILTIN_CALL,
    IR_INST_CAST,
    IR_INST_COMPOSITE_CONSTRUCT,
    IR_INST_COMPOSITE_EXTRACT,
    IR_INST_VECTOR_SHUFFLE,

    IR_INST_UNARY,
    IR_INST_BINARY,
} IRInstKind;

typedef enum IRBuiltinInstKind {
    IR_BUILTIN_DOT,
} IRBuiltinInstKind;

struct IRInst
{
    IRInstKind kind;
    uint32_t id;
    Type *type;
    /*array*/ IRDecoration *decorations;

    union
    {
        struct
        {
            IRInst *func;
            char *name;
            SpvExecutionModel execution_model;
            IRInst **globals;
            uint32_t global_count;
        } entry_point;

        struct
        {
            /*array*/ IRInst **params;
            /*array*/ IRInst **blocks;
        } func;

        struct
        {
            /*array*/ IRInst **insts;
        } block;

        struct
        {
            SpvStorageClass storage_class;
            IRInst *initializer;
        } var;

        struct
        {
            void *value;
            size_t value_size_bytes;
        } constant;

        struct
        {
            IRInst *value;
        } return_;

        struct
        {
            IRInst *pointer;
            IRInst *value;
        } store;

        struct
        {
            IRInst *pointer;
        } load;

        struct
        {
            IRInst *base;
            IRInst **indices;
            uint32_t index_count;
        } access_chain;

        struct
        {
            IRInst *func;
            IRInst **params;
            uint32_t param_count;
        } func_call;

        struct
        {
            IRBuiltinInstKind kind;
            IRInst **params;
            uint32_t param_count;
        } builtin_call;

        struct
        {
            SpvOp op;
            Type *dst_type;
            IRInst *value;
            bool redundant;
        } cast;

        struct
        {
            IRInst **fields;
            uint32_t field_count;
        } composite_construct;

        struct
        {
            IRInst *value;
            uint32_t *indices;
            uint32_t index_count;
        } composite_extract;

        struct
        {
            IRInst *vector_a;
            IRInst *vector_b;
            uint32_t *indices;
            uint32_t index_count;
        } vector_shuffle;

        struct
        {
            IRInst *right;
            SpvOp op;
        } unary;

        struct
        {
            IRInst *left;
            IRInst *right;
            SpvOp op;
        } binary;
    };
};

struct IRModule
{
    TscCompiler *compiler;
    Module *mod;

    /*array*/ IRInst **entry_points;
    /*array*/ IRInst **constants;
    /*array*/ IRInst **functions;
    /*array*/ IRInst **globals;

    IRInst *current_block;

    uint32_t id_bound;
    /*array*/ uint32_t *stream;
};

//
// AST
//

typedef struct AstAttribute
{
    char *name;
    /*array*/ AstExpr **values;
} AstAttribute;

typedef enum AstVarKind {
    VAR_FUNCTION = 0,
    VAR_FUNCTION_PARAM,
    VAR_INPUT,
    VAR_OUTPUT,
    VAR_GLOBAL,
} AstVarKind;

typedef enum AstUnaryOp {
    UNOP_NEG,
    UNOP_NOT,
} AstUnaryOp;

typedef enum AstBinaryOp {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_MOD,

    BINOP_EQ,
    BINOP_NOTEQ,
    BINOP_LESS,
    BINOP_LESSEQ,
    BINOP_GREATER,
    BINOP_GREATEREQ,
} AstBinaryOp;

typedef enum AstStmtKind {
    STMT_DECL,
    STMT_EXPR,
    STMT_VAR_ASSIGN,
    STMT_RETURN,
} AstStmtKind;

typedef enum AstDeclKind {
    DECL_FUNC,
    DECL_VAR,
    DECL_CONST,

    DECL_STRUCT,
    DECL_STRUCT_FIELD,
} AstDeclKind;

typedef enum AstExprKind {
    EXPR_PRIMARY,
    EXPR_IDENT,
    EXPR_ACCESS,
    EXPR_SAMPLER_TYPE,
    EXPR_TEXTURE_TYPE,
    EXPR_SAMPLED_TEXTURE_TYPE,
    EXPR_FUNC_CALL,
    EXPR_BUILTIN_CALL,
    EXPR_UNARY,
    EXPR_BINARY,
} AstExprKind;

struct AstStmt
{
    AstStmtKind kind;
    Location loc;

    union
    {
        AstDecl *decl;
        AstExpr *expr;
        struct
        {
            AstExpr *assigned_expr;
            AstExpr *value_expr;
        } var_assign;
        struct
        {
            AstExpr *value;
        } return_;
    };
};

struct AstDecl
{
    AstDeclKind kind;
    Location loc;
    char *name;
    Type *type;
    Type *as_type;
    IRInst *value;
    Scope *scope;
    /*array*/ AstAttribute *attributes;
    /*array*/ IRDecoration *decorations;
    int64_t *resolved_int;

    union
    {
        struct
        {
            SpvExecutionModel *execution_model;

            /*array*/ AstStmt **stmts;
            /*array*/ AstDecl **parameters;
            AstExpr *return_type;

            // To be filled later:
            /*array*/ AstDecl **var_decls;
            /*array*/ AstDecl **func_params;
            /*array*/ AstDecl **inputs;
            /*array*/ AstDecl **outputs;
        } func;

        struct
        {
            AstExpr *type_expr;
            AstExpr *value_expr;
            SpvStorageClass storage_class;
            AstVarKind kind;
        } var;

        struct
        {
            AstExpr *type_expr;
            AstExpr *value_expr;
        } constant;

        struct
        {
            /*array*/ AstDecl **fields;
        } struct_;

        struct
        {
            AstExpr *type_expr;
            uint32_t index;
        } struct_field;

        struct
        {
            uint32_t set_index;
            /*array*/ AstDecl **params;
        } parameter_block;
    };
};

struct AstExpr
{
    AstExprKind kind;
    Location loc;
    IRInst *value;
    Type *type;
    Type *as_type;
    bool assignable;
    Scope *inhabited_scope;
    Scope *scope;
    int64_t *resolved_int;

    union
    {
        struct
        {
            Token *token;
        } primary;

        struct
        {
            char *name;

            uint32_t *shuffle_indices;
            uint32_t shuffle_index_count;
            AstDecl *decl; // The declaration this identifier refers to
        } ident;

        struct
        {
            AstExpr *base;
            /*array*/ AstExpr **chain;
        } access;

        struct
        {
            AstExpr *func_expr;
            /*array*/ AstExpr **params;
        } func_call;

        struct
        {
            IRBuiltinInstKind kind;
            /*array*/ AstExpr **params;
        } builtin_call;

        struct
        {
            AstExpr *sampled_type_expr;
            SpvDim dim;
        } texture;

        struct
        {
            AstUnaryOp op;
            AstExpr *right;
        } unary;

        struct
        {
            AstBinaryOp op;
            AstExpr *left;
            AstExpr *right;
        } binary;
    };
};

//
// Scope
//

struct Scope
{
    struct Scope *parent;
    AstDecl *owner;
    HashMap map; // string -> *AstDecl
};

//
// Compiler
//

typedef struct TscCompiler
{
    BumpAlloc alloc;
    StringBuilder sb;

    HashMap keyword_table;
    HashMap files; // Maps absolute paths to files

    /*array*/ File **file_queue;
    /*array*/ Error *errors;
} TscCompiler;

//
// File
//

struct File
{
    char *text;
    size_t text_size;

    char *path;
    char *dir;

    /*array*/ Token *tokens;
    /*array*/ AstDecl **decls;
};

struct Module
{
    TscCompiler *compiler;
    Scope *scope;
    /*array*/ File **files;

    HashMap type_cache;
    uint32_t set_bound;
};

//
// Lexer
//

typedef struct Lexer
{
    TscCompiler *compiler;
    File *file;

    Token token;

    size_t pos;
    uint32_t line;
    uint32_t col;
} Lexer;

typedef struct Parser
{
    TscCompiler *compiler;
    File *file;

    size_t pos;
} Parser;

typedef struct Analyzer
{
    TscCompiler *compiler;
    Module *module;

    AstDecl *scope_func;
    Scope **scope_stack;
} Analyzer;

// }}}

// Compiler functions {{{
static void addErr(TscCompiler *compiler, Location *loc, const char *msg)
{
    Error err = {0};
    err.loc = *loc;
    err.message = msg;
    arrPush(compiler->errors, err);
}

TscCompiler *tscCompilerCreate()
{
    TscCompiler *compiler = malloc(sizeof(TscCompiler));
    memset(compiler, 0, sizeof(*compiler));

    bumpInit(&compiler->alloc, 1 << 14);
    sbInit(&compiler->sb);

    hashInit(&compiler->keyword_table, 32);

    hashSet(&compiler->keyword_table, "const", (void *)TOKEN_CONST);
    hashSet(&compiler->keyword_table, "return", (void *)TOKEN_RETURN);
    hashSet(&compiler->keyword_table, "switch", (void *)TOKEN_SWITCH);
    hashSet(&compiler->keyword_table, "case", (void *)TOKEN_CASE);
    hashSet(&compiler->keyword_table, "default", (void *)TOKEN_DEFAULT);
    hashSet(&compiler->keyword_table, "break", (void *)TOKEN_BREAK);
    hashSet(&compiler->keyword_table, "continue", (void *)TOKEN_CONTINUE);
    hashSet(&compiler->keyword_table, "for", (void *)TOKEN_FOR);
    hashSet(&compiler->keyword_table, "while", (void *)TOKEN_WHILE);
    hashSet(&compiler->keyword_table, "if", (void *)TOKEN_IF);
    hashSet(&compiler->keyword_table, "else", (void *)TOKEN_ELSE);
    hashSet(&compiler->keyword_table, "import", (void *)TOKEN_IMPORT);
    hashSet(&compiler->keyword_table, "struct", (void *)TOKEN_STRUCT);
    hashSet(&compiler->keyword_table, "in", (void *)TOKEN_IN);
    hashSet(&compiler->keyword_table, "out", (void *)TOKEN_OUT);
    hashSet(&compiler->keyword_table, "dot", (void *)TOKEN_DOT);
    hashSet(&compiler->keyword_table, "ConstantBuffer", (void *)TOKEN_CONSTANT_BUFFER);
    hashSet(&compiler->keyword_table, "SamplerState", (void *)TOKEN_SAMPLER_STATE);
    hashSet(&compiler->keyword_table, "Texture1D", (void *)TOKEN_TEXTURE_1D);
    hashSet(&compiler->keyword_table, "Texture2D", (void *)TOKEN_TEXTURE_2D);
    hashSet(&compiler->keyword_table, "Texture3D", (void *)TOKEN_TEXTURE_3D);
    hashSet(&compiler->keyword_table, "TextureCube", (void *)TOKEN_TEXTURE_CUBE);
    hashSet(&compiler->keyword_table, "SampledTexture1D", (void *)TOKEN_SAMPLED_TEXTURE_1D);
    hashSet(&compiler->keyword_table, "SampledTexture2D", (void *)TOKEN_SAMPLED_TEXTURE_2D);
    hashSet(&compiler->keyword_table, "SampledTexture3D", (void *)TOKEN_SAMPLED_TEXTURE_3D);
    hashSet(&compiler->keyword_table, "SampledTextureCube", (void *)TOKEN_SAMPLED_TEXTURE_CUBE);

    hashSet(&compiler->keyword_table, "uint", (void *)TOKEN_UINT);
    hashSet(&compiler->keyword_table, "int", (void *)TOKEN_INT);
    hashSet(&compiler->keyword_table, "bool", (void *)TOKEN_BOOL);
    hashSet(&compiler->keyword_table, "true", (void *)TOKEN_TRUE);
    hashSet(&compiler->keyword_table, "false", (void *)TOKEN_FALSE);
    hashSet(&compiler->keyword_table, "void", (void *)TOKEN_VOID);
    hashSet(&compiler->keyword_table, "float", (void *)TOKEN_FLOAT);
    hashSet(&compiler->keyword_table, "float2", (void *)TOKEN_FLOAT2);
    hashSet(&compiler->keyword_table, "float3", (void *)TOKEN_FLOAT3);
    hashSet(&compiler->keyword_table, "float4", (void *)TOKEN_FLOAT4);
    hashSet(&compiler->keyword_table, "float2x2", (void *)TOKEN_FLOAT2X2);
    hashSet(&compiler->keyword_table, "float3x3", (void *)TOKEN_FLOAT3X3);
    hashSet(&compiler->keyword_table, "float4x4", (void *)TOKEN_FLOAT4X4);

    return compiler;
}

void tscCompilerDestroy(TscCompiler *compiler)
{
    hashDestroy(&compiler->keyword_table);
    bumpDestroy(&compiler->alloc);
    sbDestroy(&compiler->sb);
    free(compiler);
}
// }}}

// Scope functions {{{
static void scopeInit(Scope *scope, Scope *parent, AstDecl *owner)
{
    memset(scope, 0, sizeof(*scope));
    hashInit(&scope->map, 0);
    scope->parent = parent;
    scope->owner = owner;
}

static void scopeDestroy(Scope *scope)
{
    hashDestroy(&scope->map);
}

static AstDecl *scopeGetLocal(Scope *scope, const char *name)
{
    AstDecl *symbol = NULL;
    bool found = hashGet(&scope->map, name, (void **)&symbol);
    if (found) return symbol;
    return NULL;
}

static AstDecl *scopeGetGlobal(Scope *scope, const char *name)
{
    if (scope->parent)
    {
        AstDecl *sym = scopeGetGlobal(scope->parent, name);
        if (sym) return sym;
    }

    return scopeGetLocal(scope, name);
}

static bool scopeAdd(Scope *scope, const char *name, AstDecl *decl)
{
    assert(scope);

    if (scopeGetLocal(scope, name)) return false;

    hashSet(&scope->map, name, decl);

    return true;
}

static void scopeClone(Scope *new_scope, Scope *old_scope, AstDecl *new_owner)
{
    memcpy(new_scope, old_scope, sizeof(Scope));
    new_scope->owner = new_owner;
}
// }}}

// Module functions {{{
static void moduleInit(Module *m, TscCompiler *compiler)
{
    memset(m, 0, sizeof(*m));
    m->compiler = compiler;

    hashInit(&m->type_cache, 0);

    m->scope = NEW(compiler, Scope);
    scopeInit(m->scope, NULL, NULL);
}

static void moduleDestroy(Module *m)
{
    hashDestroy(&m->type_cache);
    arrFree(m->files);
}
// }}}

// Type functions {{{
static char *typeToString(TscCompiler *compiler, Type *type)
{
    if (type->string) return type->string;

    char *prefix = NULL;
    char *storage_class = NULL;
    char *sub = NULL;
    char *postfix = NULL;

    switch (type->kind)
    {
    case TYPE_VOID: prefix = "void"; break;

    case TYPE_TYPE: prefix = "type"; break;

    case TYPE_BOOL: prefix = "bool"; break;

    case TYPE_FLOAT:
        sbReset(&compiler->sb);
        sbSprintf(&compiler->sb, "float%u", type->float_.bits);
        prefix = sbBuild(&compiler->sb, &compiler->alloc);
        break;

    case TYPE_INT:
        sbReset(&compiler->sb);

        if (type->int_.is_signed)
            sbAppend(&compiler->sb, "int");
        else
            sbAppend(&compiler->sb, "uint");

        sbSprintf(&compiler->sb, "%u", type->int_.bits);
        prefix = sbBuild(&compiler->sb, &compiler->alloc);
        break;

    case TYPE_VECTOR:
        sbReset(&compiler->sb);
        sbSprintf(&compiler->sb, "vec%u", type->vector.size);
        prefix = sbBuild(&compiler->sb, &compiler->alloc);

        sub = typeToString(compiler, type->vector.elem_type);
        break;
    case TYPE_MATRIX:
        sbReset(&compiler->sb);
        sbSprintf(&compiler->sb, "mat%u", type->matrix.col_count);
        prefix = sbBuild(&compiler->sb, &compiler->alloc);

        sub = typeToString(compiler, type->matrix.col_type);
        break;

    case TYPE_POINTER: {
        prefix = "ptr";

        switch (type->ptr.storage_class)
        {
        case SpvStorageClassInput: storage_class = "input"; break;
        case SpvStorageClassOutput: storage_class = "output"; break;
        case SpvStorageClassUniformConstant: storage_class = "constant"; break;
        case SpvStorageClassUniform: storage_class = "uniform"; break;
        case SpvStorageClassStorageBuffer: storage_class = "storage"; break;
        case SpvStorageClassFunction: storage_class = "function"; break;

        default: assert(0); break;
        }

        assert(type->ptr.sub);
        sub = typeToString(compiler, type->ptr.sub);
        break;
    }

    case TYPE_FUNC: {
        prefix = "func";

        char *return_type = typeToString(compiler, type->func.return_type);
        char **params = bumpZeroAlloc(&compiler->alloc, sizeof(char *) * type->func.param_count);
        for (uint32_t i = 0; i < type->func.param_count; ++i)
        {
            params[i] = typeToString(compiler, type->func.params[i]);
        }

        sbReset(&compiler->sb);
        sbAppend(&compiler->sb, return_type);
        sbAppend(&compiler->sb, "$");
        sbSprintf(&compiler->sb, "%u", type->func.param_count);
        for (uint32_t i = 0; i < type->func.param_count; ++i)
        {
            if (i != 0) sbAppend(&compiler->sb, "_");
            sbAppend(&compiler->sb, params[i]);
        }
        sub = sbBuild(&compiler->sb, &compiler->alloc);

        break;
    }

    case TYPE_STRUCT: {
        sbReset(&compiler->sb);
        sbSprintf(&compiler->sb, "struct%u%s", strlen(type->struct_.name), type->struct_.name);
        prefix = sbBuild(&compiler->sb, &compiler->alloc);
        break;
    }

    case TYPE_SAMPLER: {
        prefix = "sampler";
        break;
    }

    case TYPE_IMAGE: {
        prefix = "image";

        sub = typeToString(compiler, type->image.sampled_type);

        switch (type->image.dim)
        {
        case SpvDimCube: postfix = "Cube"; break;
        case SpvDim1D: postfix = "1D"; break;
        case SpvDim2D: postfix = "2D"; break;
        case SpvDim3D: postfix = "3D"; break;
        default: assert(0); break;
        }

        break;
    }

    case TYPE_SAMPLED_IMAGE: {
        prefix = "sampledImage";
        sub = typeToString(compiler, type->sampled_image.image_type);
        break;
    }
    }

    sbReset(&compiler->sb);

    assert(prefix);
    sbAppend(&compiler->sb, prefix);

    if (storage_class)
    {
        sbAppend(&compiler->sb, "_");
        sbAppend(&compiler->sb, storage_class);
    }

    if (sub)
    {
        sbAppend(&compiler->sb, "_");
        sbAppend(&compiler->sb, sub);
    }

    if (postfix)
    {
        sbAppend(&compiler->sb, "_");
        sbAppend(&compiler->sb, postfix);
    }

    type->string = sbBuild(&compiler->sb, &compiler->alloc);
    return type->string;
}

static Type *getCachedType(Module *m, Type *type)
{
    char *type_string = typeToString(m->compiler, type);
    assert(type_string);
    assert(strlen(type_string) > 0);

    Type *found_type = NULL;
    if (hashGet(&m->type_cache, type_string, (void **)&found_type))
    {
        assert(found_type);
        return found_type;
    }

    hashSet(&m->type_cache, type_string, type);

    return type;
}

static Type *newBasicType(Module *m, TypeKind kind)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = kind;
    return getCachedType(m, ty);
}

static Type *newPointerType(Module *m, SpvStorageClass storage_class, Type *sub)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = TYPE_POINTER;
    ty->ptr.storage_class = storage_class;
    ty->ptr.sub = sub;
    return getCachedType(m, ty);
}

static Type *newVectorType(Module *m, Type *elem_type, uint32_t size)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = TYPE_VECTOR;
    ty->vector.elem_type = elem_type;
    ty->vector.size = size;
    return getCachedType(m, ty);
}

static Type *newMatrixType(Module *m, Type *col_type, uint32_t col_count)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = TYPE_MATRIX;
    ty->matrix.col_type = col_type;
    ty->matrix.col_count = col_count;
    return getCachedType(m, ty);
}

static Type *newFloatType(Module *m, uint32_t bits)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = TYPE_FLOAT;
    ty->float_.bits = bits;
    return getCachedType(m, ty);
}

static Type *newIntType(Module *m, uint32_t bits, bool is_signed)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = TYPE_INT;
    ty->int_.bits = bits;
    ty->int_.is_signed = is_signed;
    return getCachedType(m, ty);
}

static Type *newFuncType(Module *m, Type *return_type, Type **params, uint32_t param_count)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = TYPE_FUNC;
    ty->func.return_type = return_type;
    if (param_count > 0)
    {
        ty->func.param_count = param_count;
        ty->func.params = NEW_ARRAY(m->compiler, Type *, param_count);
        memcpy(ty->func.params, params, sizeof(Type *) * param_count);
    }
    return getCachedType(m, ty);
}

static Type *
newStructType(Module *m, char *name, Type **fields, AstDecl **field_decls, uint32_t field_count)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = TYPE_STRUCT;
    ty->struct_.name = name;

    if (field_count > 0)
    {
        ty->struct_.field_count = field_count;

        ty->struct_.fields = NEW_ARRAY(m->compiler, Type *, field_count);
        memcpy(ty->struct_.fields, fields, sizeof(Type *) * field_count);

        ty->struct_.field_decls = NEW_ARRAY(m->compiler, AstDecl *, field_count);
        memcpy(ty->struct_.field_decls, field_decls, sizeof(AstDecl *) * field_count);
    }

    return getCachedType(m, ty);
}

static Type *newImageType(Module *m, Type *sampled_type, SpvDim dim)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = TYPE_IMAGE;
    ty->image.sampled_type = sampled_type;
    ty->image.dim = dim;
    ty->image.depth = 0;
    ty->image.arrayed = 0;
    ty->image.multisampled = 0;
    ty->image.sampled = 1;
    ty->image.format = SpvImageFormatUnknown;
    return getCachedType(m, ty);
}

static Type *newSampledImageType(Module *m, Type *image_type)
{
    Type *ty = NEW(m->compiler, Type);
    ty->kind = TYPE_SAMPLED_IMAGE;
    ty->sampled_image.image_type = image_type;
    return getCachedType(m, ty);
}

static bool isTypeCastable(Type *src_type, Type *dst_type, SpvOp *op)
{
    if (src_type == dst_type)
    {
        *op = SpvOpNop;
        return true;
    }
    else if (src_type->kind == TYPE_INT)
    {
        if (src_type->int_.is_signed)
        {
            if (dst_type->kind == TYPE_INT)
            {
                if (dst_type->int_.is_signed)
                {
                    *op = SpvOpSConvert;
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else if (dst_type->kind == TYPE_FLOAT)
            {
                *op = SpvOpConvertSToF;
                return true;
            }
            else
            {
                return false;
            }
        }
        else
        {
            if (dst_type->kind == TYPE_INT)
            {
                if (dst_type->int_.is_signed)
                {
                    return false;
                }
                else
                {
                    *op = SpvOpUConvert;
                    return true;
                }
            }
            else if (dst_type->kind == TYPE_FLOAT)
            {
                *op = SpvOpConvertUToF;
                return true;
            }
            else
            {
                return false;
            }
        }
    }
    else if (src_type->kind == TYPE_FLOAT)
    {
        if (dst_type->kind == TYPE_INT)
        {
            if (dst_type->int_.is_signed)
            {
                *op = SpvOpConvertFToS;
                return true;
            }
            else
            {
                *op = SpvOpConvertFToU;
                return true;
            }
        }
        else if (dst_type->kind == TYPE_FLOAT)
        {
            *op = SpvOpFConvert;
            return true;
        }
        else
        {
            return false;
        }
    }
    else
    {
        return false;
    }

    assert(0);
}

static Type *getScalarType(Type *type)
{
    switch (type->kind)
    {
    case TYPE_BOOL:
    case TYPE_FLOAT:
    case TYPE_INT: {
        return type;
    }

    case TYPE_VECTOR: {
        return type->vector.elem_type;
    }

    default: break;
    }

    return NULL;
}
// }}}

// Lexer functions {{{
static inline bool lexerIsAtEnd(Lexer *l)
{
    return (l->pos >= l->file->text_size) || (l->file->text[l->pos] == '\0');
}

static inline char lexerNext(Lexer *l, size_t count)
{
    char c = l->file->text[l->pos];
    l->pos += count;
    l->col += count;
    return c;
}

static inline char lexerPeek(Lexer *l, size_t offset)
{
    return l->file->text[l->pos + offset];
}

static inline void lexerAddSimpleToken(Lexer *l, TokenKind kind, size_t length)
{
    lexerNext(l, length);
    l->token.kind = kind;
    l->token.loc.length = length;
}

static void lexerLex(Lexer *l, TscCompiler *compiler, File *file)
{
    l->file = file;
    l->compiler = compiler;
    l->pos = 0;
    l->col = 1;
    l->line = 1;

    while (!lexerIsAtEnd(l))
    {
        memset(&l->token, 0, sizeof(Token));
        l->token.loc.pos = l->pos;
        l->token.loc.length = 0;
        l->token.loc.line = l->line;
        l->token.loc.col = l->col;

        switch (lexerPeek(l, 0))
        {
        case '\t':
        case ' ': {
            lexerNext(l, 1);
            break;
        }

        case '\n': {
            l->col = 0;
            l->line++;
            lexerNext(l, 1);
            break;
        }

        case '(': {
            lexerAddSimpleToken(l, TOKEN_LPAREN, 1);
            break;
        }
        case ')': {
            lexerAddSimpleToken(l, TOKEN_RPAREN, 1);
            break;
        }
        case '[': {
            if (lexerPeek(l, 1) == '[')
                lexerAddSimpleToken(l, TOKEN_ATTR_LBRACK, 2);
            else
                lexerAddSimpleToken(l, TOKEN_LBRACK, 1);
            break;
        }
        case ']': {
            if (lexerPeek(l, 1) == ']')
                lexerAddSimpleToken(l, TOKEN_ATTR_RBRACK, 2);
            else
                lexerAddSimpleToken(l, TOKEN_RBRACK, 1);
            break;
        }
        case '{': {
            lexerAddSimpleToken(l, TOKEN_LCURLY, 1);
            break;
        }
        case '}': {
            lexerAddSimpleToken(l, TOKEN_RCURLY, 1);
            break;
        }

        case '.': {
            lexerAddSimpleToken(l, TOKEN_PERIOD, 1);
            break;
        }

        case ',': {
            lexerAddSimpleToken(l, TOKEN_COMMA, 1);
            break;
        }

        case '+': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_ADDEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_ADD, 1);
            break;
        }
        case '-': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_SUBEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_SUB, 1);
            break;
        }
        case '*': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_MULEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_MUL, 1);
            break;
        }
        case '/': {
            if (lexerPeek(l, 1) == '/')
            {
                lexerNext(l, 2);

                while (lexerPeek(l, 0) != '\n' && !lexerIsAtEnd(l))
                {
                    lexerNext(l, 1);
                }
            }
            else if (lexerPeek(l, 1) == '*')
            {
                // Multiline comment
                lexerNext(l, 2);

                while ((lexerPeek(l, 0) != '*' || lexerPeek(l, 1) != '/') && !lexerIsAtEnd(l))
                {
                    if (lexerPeek(l, 0) == '\n')
                    {
                        ++l->line;
                        l->col = 0;
                    }
                    lexerNext(l, 1);
                }

                if (!lexerIsAtEnd(l))
                {
                    lexerNext(l, 2);
                }
                else
                {
                    Location err_loc = l->token.loc;
                    err_loc.length = 1;
                    addErr(l->compiler, &err_loc, "unclosed comment");
                }
            }
            else if (lexerPeek(l, 1) == '=')
            {
                lexerAddSimpleToken(l, TOKEN_DIVEQ, 2);
            }
            else
            {
                lexerAddSimpleToken(l, TOKEN_DIV, 1);
            }
            break;
        }
        case '%': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_MODEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_MOD, 1);
            break;
        }

        case '&': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_BITANDEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_BITAND, 1);
            break;
        }
        case '|': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_BITOREQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_BITOR, 1);
            break;
        }
        case '^': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_BITXOREQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_BITXOR, 1);
            break;
        }
        case '~': {
            lexerAddSimpleToken(l, TOKEN_BITNOT, 1);
            break;
        }

        case ':': {
            if (lexerPeek(l, 1) == ':')
                lexerAddSimpleToken(l, TOKEN_COLON_COLON, 2);
            else
                lexerAddSimpleToken(l, TOKEN_COLON, 1);
            break;
        }
        case ';': {
            lexerAddSimpleToken(l, TOKEN_SEMICOLON, 1);
            break;
        }

        case '!': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_NOTEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_NOT, 1);
            break;
        }

        case '=': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_EQUAL, 2);
            else
                lexerAddSimpleToken(l, TOKEN_ASSIGN, 1);
            break;
        }

        case '>': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_GREATEREQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_GREATER, 1);
            break;
        }
        case '<': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_LESSEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_LESS, 1);
            break;
        }

        case '"': {
            lexerNext(l, 1);

            sbReset(&compiler->sb);

            while (!lexerIsAtEnd(l) && lexerPeek(l, 0) != '"')
            {
                if (lexerPeek(l, 0) == '\\')
                {
                    lexerNext(l, 1);
                    switch (lexerPeek(l, 0))
                    {
                    case 'a':
                        sbAppendChar(&compiler->sb, '\a');
                        lexerNext(l, 1);
                        break;
                    case 'b':
                        sbAppendChar(&compiler->sb, '\b');
                        lexerNext(l, 1);
                        break;
                    case 'f':
                        sbAppendChar(&compiler->sb, '\f');
                        lexerNext(l, 1);
                        break;
                    case 'n':
                        sbAppendChar(&compiler->sb, '\n');
                        lexerNext(l, 1);
                        break;
                    case 'r':
                        sbAppendChar(&compiler->sb, '\r');
                        lexerNext(l, 1);
                        break;
                    case 't':
                        sbAppendChar(&compiler->sb, '\t');
                        lexerNext(l, 1);
                        break;
                    case 'v':
                        sbAppendChar(&compiler->sb, '\v');
                        lexerNext(l, 1);
                        break;
                    case '0':
                        sbAppendChar(&compiler->sb, '\0');
                        lexerNext(l, 1);
                        break;
                    case '?':
                        sbAppendChar(&compiler->sb, '\?');
                        lexerNext(l, 1);
                        break;
                    case '\'':
                        sbAppendChar(&compiler->sb, '\'');
                        lexerNext(l, 1);
                        break;
                    case '\"':
                        sbAppendChar(&compiler->sb, '\"');
                        lexerNext(l, 1);
                        break;
                    case '\\':
                        sbAppendChar(&compiler->sb, '\\');
                        lexerNext(l, 1);
                        break;
                    default:
                        sbAppendChar(&compiler->sb, '\\');
                        sbAppendChar(&compiler->sb, lexerPeek(l, 0));
                        lexerNext(l, 1);
                        break;
                    }
                }
                else
                {
                    sbAppendChar(&compiler->sb, lexerPeek(l, 0));
                    lexerNext(l, 1);
                }
            }

            if (lexerIsAtEnd(l) || lexerPeek(l, 0) != '"')
            {
                Location err_loc = l->token.loc;
                err_loc.length = 1;
                addErr(l->compiler, &err_loc, "unclosed string");
                break;
            }

            lexerNext(l, 1);

            l->token.kind = TOKEN_STRING_LIT;
            l->token.str = sbBuild(&compiler->sb, &compiler->alloc);
            l->token.loc.length = l->pos - l->token.loc.pos;
            break;
        }

        default: {
            if (isLetter(lexerPeek(l, 0)))
            {
                while (!lexerIsAtEnd(l) && isAlphanum(lexerPeek(l, 0)))
                {
                    lexerNext(l, 1);
                }

                size_t ident_length = l->pos - l->token.loc.pos;
                char *ident_start = &l->file->text[l->token.loc.pos];

                char *ident = NEW_ARRAY(compiler, char, ident_length + 1);
                memcpy(ident, ident_start, ident_length);
                ident[ident_length] = '\0';

                l->token.loc.length = ident_length;
                l->token.str = ident;

                void *result = NULL;
                bool found_keyword = hashGet(&compiler->keyword_table, ident, &result);
                if (found_keyword)
                {
                    l->token.kind = (TokenKind)result;
                }
                else
                {
                    l->token.kind = TOKEN_IDENT;
                }
            }
            else if (isNumeric(lexerPeek(l, 0)))
            {
                char *dot_ptr = NULL;

                if (lexerPeek(l, 0) == '0' && lexerPeek(l, 1) == 'x')
                {
                    // Hexadecimal
                    l->token.kind = TOKEN_INT_LIT;
                    lexerNext(l, 2);
                    l->token.loc.length += 2;

                    char *hex_start = &l->file->text[l->pos];

                    while (isNumeric(lexerPeek(l, 0)) ||
                           (lexerPeek(l, 0) >= 'a' && lexerPeek(l, 0) <= 'f') ||
                           (lexerPeek(l, 0) >= 'A' && lexerPeek(l, 0) <= 'F'))
                    {
                        l->token.loc.length++;
                        l->col++;
                        lexerNext(l, 1);
                    }

                    char *str = NEW_ARRAY(compiler, char, l->token.loc.length - 2 + 1);
                    memcpy(str, hex_start, l->token.loc.length - 2);

                    l->token.int_ = strtol(str, NULL, 16);
                }
                else
                {
                    char *number_start = &l->file->text[l->pos];

                    while (isNumeric(lexerPeek(l, 0)) || lexerPeek(l, 0) == '.')
                    {
                        if (lexerPeek(l, 0) == '.')
                        {
                            if (!isNumeric(lexerPeek(l, 1))) break;
                            assert(!dot_ptr);
                            dot_ptr = &l->file->text[l->pos];
                        }

                        l->token.loc.length++;
                        l->col++;
                        lexerNext(l, 1);
                    }

                    char *str = NEW_ARRAY(compiler, char, l->token.loc.length + 1);
                    memcpy(str, number_start, l->token.loc.length);

                    if (dot_ptr)
                    {
                        l->token.kind = TOKEN_FLOAT_LIT;
                        l->token.double_ = strtod(str, NULL);
                    }
                    else
                    {
                        l->token.kind = TOKEN_INT_LIT;
                        l->token.int_ = strtol(str, NULL, 10);
                    }
                }
            }
            else
            {
                Location err_loc = l->token.loc;
                err_loc.length = 1;
                addErr(l->compiler, &err_loc, "unknown token");
                lexerNext(l, 1);
            }

            break;
        }
        }

        if (l->token.loc.length > 0)
        {
            arrPush(l->file->tokens, l->token);
        }
    }

    if (arrLength(l->file->tokens) == 0)
    {
        Location err_loc = {0};
        err_loc.file = l->file;
        addErr(l->compiler, &err_loc, "no tokens found for file");
    }
}
// }}}

// Parser functions {{{
static AstExpr *parseExpr(Parser *p);

static inline bool parserIsAtEnd(Parser *p)
{
    return p->pos >= arrLength(p->file->tokens);
}

static inline Token *parserPeek(Parser *p, size_t offset)
{
    if (p->pos + offset >= arrLength(p->file->tokens))
    {
        return &p->file->tokens[arrLength(p->file->tokens) - 1];
    }
    return &p->file->tokens[p->pos + offset];
}

static inline Token *parserNext(Parser *p, size_t count)
{
    if (parserIsAtEnd(p)) return NULL;

    Token *tok = &p->file->tokens[p->pos];
    p->pos += count;
    return tok;
}

static inline Token *parserConsume(Parser *p, TokenKind kind)
{
    Token *tok = parserPeek(p, 0);
    if (tok->kind != kind)
    {
        addErr(p->compiler, &tok->loc, "unexpected token");
        return NULL;
    }
    parserNext(p, 1);
    return tok;
}

static inline Location parserBeginLoc(Parser *p)
{
    return parserPeek(p, 0)->loc;
}

static inline void parserEndLoc(Parser *p, Location *loc)
{
    loc->length = (parserPeek(p, 0)->loc.pos + parserPeek(p, 0)->loc.length) - loc->pos;
}

static AstExpr *parseIdentExpr(Parser *p)
{
    TscCompiler *compiler = p->compiler;

    Location loc = parserBeginLoc(p);

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_IDENT: {
        AstExpr *expr = NEW(compiler, AstExpr);
        expr->kind = EXPR_IDENT;
        expr->ident.name = parserNext(p, 1)->str;

        parserEndLoc(p, &loc);
        expr->loc = loc;

        return expr;
    }

    default: {
        addErr(p->compiler, &parserNext(p, 1)->loc, "expecting identifier expression");
        break;
    }
    }

    return NULL;
}

static AstExpr *parsePrimaryExpr(Parser *p)
{
    TscCompiler *compiler = p->compiler;

    Location loc = parserBeginLoc(p);

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_IDENT: {
        return parseIdentExpr(p);
    }

    case TOKEN_INT_LIT:
    case TOKEN_FLOAT_LIT:
    case TOKEN_STRING_LIT:
    case TOKEN_BOOL:
    case TOKEN_VOID:
    case TOKEN_TRUE:
    case TOKEN_FALSE:
    case TOKEN_INT:
    case TOKEN_UINT:
    case TOKEN_FLOAT:
    case TOKEN_FLOAT2:
    case TOKEN_FLOAT3:
    case TOKEN_FLOAT4:
    case TOKEN_FLOAT2X2:
    case TOKEN_FLOAT3X3:
    case TOKEN_FLOAT4X4: {
        AstExpr *expr = NEW(compiler, AstExpr);
        expr->kind = EXPR_PRIMARY;
        expr->primary.token = parserNext(p, 1);

        parserEndLoc(p, &loc);
        expr->loc = loc;

        return expr;
    }

    default: {
        addErr(p->compiler, &parserNext(p, 1)->loc, "expecting primary expression");
        break;
    }
    }

    return NULL;
}

static AstExpr *parseAccessFuncCall(Parser *p)
{
    TscCompiler *compiler = p->compiler;
    Location loc = parserBeginLoc(p);

    AstExpr *expr = NULL;

    bool is_builtin = true;
    IRBuiltinInstKind builtin_kind = {0};

    // Check if it's a builtin function
    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_DOT: builtin_kind = IR_BUILTIN_DOT; break;

    default: is_builtin = false; break;
    }

    if (is_builtin)
    {
        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BUILTIN_CALL;
        expr->builtin_call.kind = builtin_kind;

        parserNext(p, 1); // skip name token

        if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

        while (!parserIsAtEnd(p) && parserPeek(p, 0)->kind != TOKEN_RPAREN)
        {
            AstExpr *param = parseExpr(p);
            if (!param) return NULL;
            arrPush(expr->builtin_call.params, param);

            if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
            {
                if (!parserConsume(p, TOKEN_COMMA)) return NULL;
            }
        }

        if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }
    else
    {
        expr = parsePrimaryExpr(p);
        if (!expr) return NULL;
    }

    // Not a builtin function, must be an access or function call

    while (!parserIsAtEnd(p) &&
           (parserPeek(p, 0)->kind == TOKEN_LPAREN || parserPeek(p, 0)->kind == TOKEN_PERIOD))
    {
        if (parserPeek(p, 0)->kind == TOKEN_LPAREN)
        {
            // Function call expression
            parserNext(p, 1);

            AstExpr *func_call = NEW(p->compiler, AstExpr);
            func_call->kind = EXPR_FUNC_CALL;
            func_call->func_call.func_expr = expr;

            while (!parserIsAtEnd(p) && parserPeek(p, 0)->kind != TOKEN_RPAREN)
            {
                AstExpr *param = parseExpr(p);
                if (!param) return NULL;
                arrPush(func_call->func_call.params, param);

                if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                }
            }

            if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

            parserEndLoc(p, &loc);
            func_call->loc = loc;

            expr = func_call;
        }
        else if (parserPeek(p, 0)->kind == TOKEN_PERIOD)
        {
            // Access expression

            AstExpr *base_expr = expr;
            expr = NEW(compiler, AstExpr);
            expr->kind = EXPR_ACCESS;
            expr->access.base = base_expr;

            while (parserPeek(p, 0)->kind == TOKEN_PERIOD)
            {
                parserNext(p, 1);

                AstExpr *ident = parseIdentExpr(p);
                if (!ident) return NULL;

                arrPush(expr->access.chain, ident);
            }

            parserEndLoc(p, &loc);
            expr->loc = loc;
        }
        else
        {
            assert(0);
        }
    }

    return expr;
}

static AstExpr *parseUnaryExpr(Parser *p)
{
    if (parserPeek(p, 0)->kind == TOKEN_SUB || parserPeek(p, 0)->kind == TOKEN_NOT)
    {
        Token *op_tok = parserNext(p, 1);

        AstUnaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_SUB: op = UNOP_NEG; break;
        case TOKEN_NOT: op = UNOP_NOT; break;
        default: assert(0); break;
        }

        AstExpr *right = parseUnaryExpr(p);

        AstExpr *expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_UNARY;
        expr->unary.right = right;
        expr->unary.op = op;

        return expr;
    }

    return parseAccessFuncCall(p);
}

static AstExpr *parseMuliplication(Parser *p)
{
    AstExpr *expr = parseUnaryExpr(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) &&
           (parserPeek(p, 0)->kind == TOKEN_MUL || parserPeek(p, 0)->kind == TOKEN_DIV))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_MUL: op = BINOP_MUL; break;
        case TOKEN_DIV: op = BINOP_DIV; break;
        default: assert(0); break;
        }

        AstExpr *left = expr;
        AstExpr *right = parseUnaryExpr(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;
    }

    return expr;
}

static AstExpr *parseAddition(Parser *p)
{
    AstExpr *expr = parseMuliplication(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) &&
           (parserPeek(p, 0)->kind == TOKEN_ADD || parserPeek(p, 0)->kind == TOKEN_SUB))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_ADD: op = BINOP_ADD; break;
        case TOKEN_SUB: op = BINOP_SUB; break;
        default: assert(0); break;
        }

        AstExpr *left = expr;
        AstExpr *right = parseMuliplication(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;
    }

    return expr;
}

static AstExpr *parseComparison(Parser *p)
{
    AstExpr *expr = parseAddition(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) &&
           (parserPeek(p, 0)->kind == TOKEN_EQUAL || parserPeek(p, 0)->kind == TOKEN_NOTEQ ||
            parserPeek(p, 0)->kind == TOKEN_LESS || parserPeek(p, 0)->kind == TOKEN_LESSEQ ||
            parserPeek(p, 0)->kind == TOKEN_GREATER || parserPeek(p, 0)->kind == TOKEN_GREATEREQ))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_EQUAL: op = BINOP_EQ; break;
        case TOKEN_NOTEQ: op = BINOP_NOTEQ; break;
        case TOKEN_LESS: op = BINOP_LESS; break;
        case TOKEN_LESSEQ: op = BINOP_LESSEQ; break;
        case TOKEN_GREATER: op = BINOP_GREATER; break;
        case TOKEN_GREATEREQ: op = BINOP_GREATEREQ; break;
        default: assert(0); break;
        }

        AstExpr *left = expr;
        AstExpr *right = parseAddition(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;
    }

    return expr;
}

static AstExpr *parseExpr(Parser *p)
{
    return parseComparison(p);
}

static AstStmt *parseStmt(Parser *p)
{
    TscCompiler *compiler = p->compiler;

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_RETURN: {
        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_RETURN;

        if (parserPeek(p, 0)->kind != TOKEN_SEMICOLON)
        {
            AstExpr *return_expr = parseExpr(p);
            if (!return_expr) return NULL;

            stmt->return_.value = return_expr;
        }

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return stmt;
    }

    default: {
        AstExpr *expr = parseExpr(p);
        if (!expr) return NULL;

        if (parserPeek(p, 0)->kind == TOKEN_SEMICOLON)
        {
            // Expression statement
            parserNext(p, 1);
            AstStmt *stmt = NEW(compiler, AstStmt);
            stmt->kind = STMT_EXPR;
            stmt->expr = expr;

            return stmt;
        }
        else if (parserPeek(p, 0)->kind == TOKEN_ASSIGN)
        {
            // Variable assignment
            parserNext(p, 1);

            AstExpr *value_expr = parseExpr(p);
            if (!value_expr) return NULL;

            AstStmt *stmt = NEW(compiler, AstStmt);
            stmt->kind = STMT_VAR_ASSIGN;
            stmt->var_assign.assigned_expr = expr;
            stmt->var_assign.value_expr = value_expr;

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            return stmt;
        }
        else if (parserPeek(p, 0)->kind == TOKEN_IDENT)
        {
            // Variable declaration
            Token *name_tok = parserNext(p, 1);

            AstDecl *decl = NEW(compiler, AstDecl);
            decl->kind = DECL_VAR;
            decl->name = name_tok->str;
            decl->var.type_expr = expr;
            decl->var.storage_class = SpvStorageClassFunction;
            decl->var.kind = VAR_FUNCTION;

            if (parserPeek(p, 0)->kind == TOKEN_ASSIGN)
            {
                parserNext(p, 1);

                AstExpr *value_expr = parseExpr(p);
                if (!value_expr) return NULL;

                decl->var.value_expr = value_expr;
            }

            AstStmt *stmt = NEW(compiler, AstStmt);
            stmt->kind = STMT_DECL;
            stmt->decl = decl;

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            return stmt;
        }
        else
        {
            addErr(
                compiler, &parserNext(p, 1)->loc, "unexpected token, expecting ';' or identifier");
        }

        break;
    }
    }

    return NULL;
}

static AstDecl *parseTopLevel(Parser *p)
{
    TscCompiler *compiler = p->compiler;

    /*array*/ AstAttribute *attributes = NULL;

    if (parserPeek(p, 0)->kind == TOKEN_ATTR_LBRACK)
    {
        parserNext(p, 1);

        Token *namespace = NULL;
        Token *attr_name = NULL;

        namespace = parserConsume(p, TOKEN_IDENT);
        if (!namespace) return NULL;

        if (parserPeek(p, 0)->kind == TOKEN_COLON_COLON)
        {
            parserNext(p, 1);
            attr_name = parserConsume(p, TOKEN_IDENT);
            if (!attr_name) return NULL;
        }
        else
        {
            attr_name = namespace;
            namespace = NULL;
        }

        assert(attr_name);

        sbReset(&compiler->sb);
        if (namespace)
        {
            sbAppend(&compiler->sb, namespace->str);
            sbAppend(&compiler->sb, "::");
        }

        sbAppend(&compiler->sb, attr_name->str);

        AstAttribute attr = {0};
        attr.name = sbBuild(&compiler->sb, &compiler->alloc);

        if (parserPeek(p, 0)->kind == TOKEN_LPAREN)
        {
            parserNext(p, 1);

            while (parserPeek(p, 0)->kind != TOKEN_RPAREN)
            {
                AstExpr *value = parseExpr(p);
                if (!value) return NULL;

                arrPush(attr.values, value);

                if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                }
            }

            if (!parserConsume(p, TOKEN_RPAREN)) return NULL;
        }

        arrPush(attributes, attr);

        if (!parserConsume(p, TOKEN_ATTR_RBRACK)) return NULL;
    }

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_CONST: {
        parserNext(p, 1);
        AstDecl *decl = NEW(compiler, AstDecl);
        decl->kind = DECL_CONST;
        decl->attributes = attributes;

        AstExpr *type_expr = parseUnaryExpr(p);
        if (!type_expr) return NULL;

        Token *name_tok = parserConsume(p, TOKEN_IDENT);
        if (!name_tok) return NULL;

        decl->name = name_tok->str;
        decl->constant.type_expr = type_expr;

        if (!parserConsume(p, TOKEN_ASSIGN)) return NULL;

        AstExpr *value_expr = parseExpr(p);
        if (!value_expr) return NULL;
        decl->constant.value_expr = value_expr;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return decl;
    }

    case TOKEN_STRUCT: {
        parserNext(p, 1);
        AstDecl *decl = NEW(compiler, AstDecl);
        decl->kind = DECL_STRUCT;
        decl->attributes = attributes;

        Token *name_tok = parserConsume(p, TOKEN_IDENT);
        if (!name_tok) return NULL;
        decl->name = name_tok->str;

        if (!parserConsume(p, TOKEN_LCURLY)) return NULL;

        while (parserPeek(p, 0)->kind != TOKEN_RCURLY)
        {
            AstExpr *type_expr = parseUnaryExpr(p);
            if (!type_expr) return NULL;

            Token *name_tok = parserConsume(p, TOKEN_IDENT);
            if (!name_tok) return NULL;

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            AstDecl *field_decl = NEW(compiler, AstDecl);
            field_decl->kind = DECL_STRUCT_FIELD;
            field_decl->name = name_tok->str;
            field_decl->struct_field.type_expr = type_expr;

            arrPush(decl->struct_.fields, field_decl);
        }

        if (!parserConsume(p, TOKEN_RCURLY)) return NULL;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return decl;
    }

    case TOKEN_CONSTANT_BUFFER:
    case TOKEN_SAMPLED_TEXTURE_1D:
    case TOKEN_SAMPLED_TEXTURE_2D:
    case TOKEN_SAMPLED_TEXTURE_3D:
    case TOKEN_SAMPLED_TEXTURE_CUBE:
    case TOKEN_TEXTURE_1D:
    case TOKEN_TEXTURE_2D:
    case TOKEN_TEXTURE_3D:
    case TOKEN_TEXTURE_CUBE:
    case TOKEN_SAMPLER_STATE: {
        AstExpr *type_expr = NULL;
        SpvStorageClass storage_class = {0};

        switch (parserPeek(p, 0)->kind)
        {
        case TOKEN_CONSTANT_BUFFER: {
            parserNext(p, 1);

            storage_class = SpvStorageClassUniform;

            if (!parserConsume(p, TOKEN_LESS)) return NULL;

            type_expr = parseUnaryExpr(p);
            if (!type_expr) return NULL;

            if (!parserConsume(p, TOKEN_GREATER)) return NULL;

            break;
        }

        case TOKEN_SAMPLED_TEXTURE_1D:
        case TOKEN_SAMPLED_TEXTURE_2D:
        case TOKEN_SAMPLED_TEXTURE_3D:
        case TOKEN_SAMPLED_TEXTURE_CUBE:
        case TOKEN_TEXTURE_1D:
        case TOKEN_TEXTURE_2D:
        case TOKEN_TEXTURE_3D:
        case TOKEN_TEXTURE_CUBE: {
            Token *texture_kind_tok = parserNext(p, 1);

            storage_class = SpvStorageClassUniformConstant;

            type_expr = NEW(compiler, AstExpr);
            type_expr->loc = parserPeek(p, 0)->loc;

            switch (texture_kind_tok->kind)
            {
            case TOKEN_TEXTURE_1D:
                type_expr->kind = EXPR_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDim1D;
                break;
            case TOKEN_TEXTURE_2D:
                type_expr->kind = EXPR_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDim2D;
                break;
            case TOKEN_TEXTURE_3D:
                type_expr->kind = EXPR_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDim3D;
                break;
            case TOKEN_TEXTURE_CUBE:
                type_expr->kind = EXPR_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDimCube;
                break;
            case TOKEN_SAMPLED_TEXTURE_1D:
                type_expr->kind = EXPR_SAMPLED_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDim1D;
                break;
            case TOKEN_SAMPLED_TEXTURE_2D:
                type_expr->kind = EXPR_SAMPLED_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDim2D;
                break;
            case TOKEN_SAMPLED_TEXTURE_3D:
                type_expr->kind = EXPR_SAMPLED_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDim3D;
                break;
            case TOKEN_SAMPLED_TEXTURE_CUBE:
                type_expr->kind = EXPR_SAMPLED_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDimCube;
                break;

            default: assert(0); break;
            }

            if (!parserConsume(p, TOKEN_LESS)) return NULL;

            type_expr->texture.sampled_type_expr = parseUnaryExpr(p);
            if (!type_expr->texture.sampled_type_expr) return NULL;

            if (!parserConsume(p, TOKEN_GREATER)) return NULL;

            break;
        }

        case TOKEN_SAMPLER_STATE: {
            type_expr = NEW(compiler, AstExpr);
            type_expr->kind = EXPR_SAMPLER_TYPE;
            type_expr->loc = parserPeek(p, 0)->loc;

            storage_class = SpvStorageClassUniformConstant;

            parserNext(p, 1);

            break;
        }

        default: assert(0); break;
        }

        Token *name_tok = parserConsume(p, TOKEN_IDENT);
        if (!name_tok) return NULL;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        AstDecl *decl = NEW(compiler, AstDecl);
        decl->kind = DECL_VAR;
        decl->name = name_tok->str;
        decl->var.type_expr = type_expr;
        decl->var.storage_class = storage_class;
        decl->var.kind = VAR_GLOBAL;
        decl->attributes = attributes;

        return decl;
    }

    default: {
        AstExpr *type_expr = parseUnaryExpr(p);
        if (!type_expr) return NULL;

        Token *name_tok = parserConsume(p, TOKEN_IDENT);
        if (!name_tok) return NULL;

        if (parserPeek(p, 0)->kind == TOKEN_LPAREN)
        {
            // Function declaration

            AstDecl *decl = NEW(compiler, AstDecl);
            decl->kind = DECL_FUNC;
            decl->name = name_tok->str;
            decl->func.return_type = type_expr;
            decl->attributes = attributes;

            if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

            while (parserPeek(p, 0)->kind != TOKEN_RPAREN)
            {
                SpvStorageClass storage_class = SpvStorageClassFunction;
                AstVarKind var_kind = VAR_FUNCTION_PARAM;

                if (parserPeek(p, 0)->kind == TOKEN_IN)
                {
                    parserNext(p, 1);
                    storage_class = SpvStorageClassInput;
                    var_kind = VAR_INPUT;
                }
                else if (parserPeek(p, 0)->kind == TOKEN_OUT)
                {
                    parserNext(p, 1);
                    storage_class = SpvStorageClassOutput;
                    var_kind = VAR_OUTPUT;
                }

                AstExpr *type_expr = parseUnaryExpr(p);
                if (!type_expr) return NULL;

                Token *param_name_tok = parserConsume(p, TOKEN_IDENT);
                if (!param_name_tok) return NULL;

                AstDecl *param_decl = NEW(compiler, AstDecl);
                param_decl->kind = DECL_VAR;
                param_decl->name = param_name_tok->str;
                param_decl->var.type_expr = type_expr;
                param_decl->var.storage_class = storage_class;
                param_decl->var.kind = var_kind;

                arrPush(decl->func.parameters, param_decl);

                if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                }
            }

            if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

            if (!parserConsume(p, TOKEN_LCURLY)) return NULL;

            while (parserPeek(p, 0)->kind != TOKEN_RCURLY)
            {
                AstStmt *stmt = parseStmt(p);
                if (stmt)
                {
                    arrPush(decl->func.stmts, stmt);
                }
            }

            if (!parserConsume(p, TOKEN_RCURLY)) return NULL;

            return decl;
        }
        else
        {
            addErr(compiler, &parserPeek(p, 0)->loc, "expecting top level declaration");
            parserNext(p, 1);
        }

        break;
    }
    }

    return NULL;
}

static void parserParse(Parser *p, TscCompiler *compiler, File *file)
{
    p->pos = 0;
    p->compiler = compiler;
    p->file = file;

    while (!parserIsAtEnd(p))
    {
        AstDecl *decl = parseTopLevel(p);
        if (decl)
        {
            arrPush(p->file->decls, decl);
        }
    }
}
// }}}

// Analyzer functions {{{
static void analyzerAnalyzeExpr(Analyzer *a, AstExpr *expr, Type *expected_type);
static void analyzerAnalyzeStmt(Analyzer *a, AstStmt *stmt);
static void analyzerAnalyzeDecl(Analyzer *a, AstDecl *decl);

static void analyzerPushScope(Analyzer *a, Scope *scope)
{
    assert(scope);
    arrPush(a->scope_stack, scope);
    if (scope->owner && scope->owner->kind == DECL_FUNC)
    {
        a->scope_func = scope->owner;
    }
}

static void analyzerPopScope(Analyzer *a, Scope *scope)
{
    assert(scope);
    assert(arrLength(a->scope_stack) > 0);

    Scope *last_scope = a->scope_stack[arrLength(a->scope_stack) - 1];
    assert(last_scope == scope);

    arrPop(a->scope_stack);

    a->scope_func = NULL;
    for (uint32_t i = 0; i < arrLength(a->scope_stack); ++i)
    {
        if (a->scope_stack[i]->owner && a->scope_stack[i]->owner->kind == DECL_FUNC)
        {
            a->scope_func = a->scope_stack[i]->owner;
        }
    }
}

static Scope *analyzerCurrentScope(Analyzer *a)
{
    if (arrLength(a->scope_stack) > 0)
    {
        return a->scope_stack[arrLength(a->scope_stack) - 1];
    }
    return NULL;
}

static void analyzerTryRegisterDecl(Analyzer *a, AstDecl *decl)
{
    if (!decl->name) return;
    if (strcmp(decl->name, "_") == 0) return; // We don't register underscore names

    Scope *scope = analyzerCurrentScope(a);

    if (scopeGetLocal(scope, decl->name))
    {
        addErr(a->compiler, &decl->loc, "duplicate declaration");
    }
    else
    {
        scopeAdd(scope, decl->name, decl);
    }
}

static void analyzerAnalyzeExpr(Analyzer *a, AstExpr *expr, Type *expected_type)
{
    TscCompiler *compiler = a->compiler;
    Scope *scope = analyzerCurrentScope(a);
    Module *m = a->module;

    expr->inhabited_scope = scope;

    switch (expr->kind)
    {
    case EXPR_PRIMARY: {
        switch (expr->primary.token->kind)
        {
        case TOKEN_VOID: {
            expr->as_type = newBasicType(a->module, TYPE_VOID);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_BOOL: {
            expr->as_type = newBasicType(a->module, TYPE_BOOL);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FLOAT: {
            expr->as_type = newFloatType(a->module, 32);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_INT: {
            expr->as_type = newIntType(a->module, 32, true);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_UINT: {
            expr->as_type = newIntType(a->module, 32, false);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FLOAT2: {
            Type *float_type = newFloatType(a->module, 32);
            expr->as_type = newVectorType(a->module, float_type, 2);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FLOAT3: {
            Type *float_type = newFloatType(a->module, 32);
            expr->as_type = newVectorType(a->module, float_type, 3);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FLOAT4: {
            Type *float_type = newFloatType(a->module, 32);
            expr->as_type = newVectorType(a->module, float_type, 4);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FLOAT2X2: {
            Type *float_type = newFloatType(a->module, 32);
            Type *col_type = newVectorType(a->module, float_type, 2);
            expr->as_type = newMatrixType(a->module, col_type, 2);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FLOAT3X3: {
            Type *float_type = newFloatType(a->module, 32);
            Type *col_type = newVectorType(a->module, float_type, 3);
            expr->as_type = newMatrixType(a->module, col_type, 3);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FLOAT4X4: {
            Type *float_type = newFloatType(a->module, 32);
            Type *col_type = newVectorType(a->module, float_type, 4);
            expr->as_type = newMatrixType(a->module, col_type, 4);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FALSE:
        case TOKEN_TRUE: {
            expr->type = newBasicType(a->module, TYPE_BOOL);
            break;
        }

        case TOKEN_FLOAT_LIT: {
            if (expected_type && expected_type->kind == TYPE_FLOAT)
            {
                expr->type = expected_type;
            }
            else
            {
                expr->type = newFloatType(a->module, 32);
            }
            break;
        }

        case TOKEN_INT_LIT: {
            if (expected_type &&
                (expected_type->kind == TYPE_INT || expected_type->kind == TYPE_FLOAT))
            {
                expr->type = expected_type;
            }
            else
            {
                expr->type = newIntType(a->module, 32, true);
            }

            expr->resolved_int = NEW(compiler, int64_t);
            *expr->resolved_int = expr->primary.token->int_;

            break;
        }

        default: assert(0); break;
        }
        break;
    }

    case EXPR_IDENT: {
        AstDecl *decl = scopeGetGlobal(scope, expr->ident.name);
        if (!decl)
        {
            addErr(compiler, &expr->loc, "unknown identifier");
            break;
        }

        if (decl->kind == DECL_VAR)
        {
            if (decl->var.kind != VAR_FUNCTION_PARAM)
            {
                expr->assignable = true;
            }
        }

        expr->ident.decl = decl;

        expr->type = decl->type;
        expr->as_type = decl->as_type;
        expr->scope = decl->scope;
        expr->resolved_int = decl->resolved_int;
        break;
    }

    case EXPR_ACCESS: {
        AstExpr *left = expr->access.base;
        analyzerAnalyzeExpr(a, left, NULL);

        for (uint32_t i = 0; i < arrLength(expr->access.chain); ++i)
        {
            AstExpr *right = expr->access.chain[i];

            if (!left->type)
            {
                assert(arrLength(compiler->errors) > 0);
                break;
            }

            if (left->type->kind == TYPE_STRUCT)
            {
                assert(left->scope);

                analyzerPushScope(a, left->scope);
                analyzerAnalyzeExpr(a, right, NULL);
                analyzerPopScope(a, left->scope);
            }
            else if (left->type->kind == TYPE_VECTOR)
            {
                assert(right->kind == EXPR_IDENT);

                char *selector = right->ident.name;
                uint32_t new_vec_dim = strlen(selector);
                if (new_vec_dim > 4)
                {
                    addErr(compiler, &right->loc, "vector shuffle must select at most 4 elements");
                    break;
                }

                uint32_t *positions = NEW_ARRAY(compiler, uint32_t, new_vec_dim);

                for (uint32_t j = 0; j < new_vec_dim; ++j)
                {
                    bool valid = true;
                    switch (selector[j])
                    {
                    case 'r':
                    case 'x': positions[j] = 0; break;
                    case 'g':
                    case 'y': positions[j] = 1; break;
                    case 'b':
                    case 'z': positions[j] = 2; break;
                    case 'a':
                    case 'w': positions[j] = 3; break;

                    default:
                        addErr(compiler, &right->loc, "invalid vector shuffle");
                        valid = false;
                        break;
                    }

                    if (positions[j] >= left->type->vector.size)
                    {
                        addErr(compiler, &right->loc, "invalid vector shuffle");
                        break;
                        valid = false;
                    }

                    if (!valid) break;
                }

                right->ident.shuffle_indices = positions;
                right->ident.shuffle_index_count = new_vec_dim;

                if (new_vec_dim == 1)
                {
                    right->type = left->type->vector.elem_type;
                }
                else
                {
                    right->type = newVectorType(m, left->type->vector.elem_type, new_vec_dim);
                }
            }
            else
            {
                addErr(compiler, &left->loc, "expression is not accessible");
                break;
            }

            if (i == (arrLength(expr->access.chain) - 1))
            {
                expr->type = right->type;
                expr->as_type = right->as_type;
                expr->scope = right->scope;
                expr->resolved_int = right->resolved_int;
            }

            left = right;
        }

        expr->assignable = expr->access.base->assignable;

        break;
    }

    case EXPR_FUNC_CALL: {
        analyzerAnalyzeExpr(a, expr->func_call.func_expr, NULL);
        Type *func_type = expr->func_call.func_expr->type;
        if (!func_type)
        {
            break;
        }

        if (func_type->kind == TYPE_TYPE)
        {
            // Type constructor

            Type *constructed_type = expr->func_call.func_expr->as_type;
            assert(constructed_type);

            expr->type = constructed_type;

            uint32_t param_count = arrLength(expr->func_call.params);
            AstExpr **params = expr->func_call.params;

            if (constructed_type->kind == TYPE_VECTOR)
            {
                if (param_count == constructed_type->vector.size)
                {
                    for (uint32_t i = 0; i < param_count; ++i)
                    {
                        analyzerAnalyzeExpr(a, params[i], constructed_type->vector.elem_type);
                    }
                }
                else if (param_count == 1)
                {
                    analyzerAnalyzeExpr(a, params[0], constructed_type->vector.elem_type);
                }
                else
                {
                    addErr(compiler, &expr->loc, "invalid parameter count for constructor");
                    break;
                }
            }
            else if (param_count == 1)
            {
                analyzerAnalyzeExpr(a, params[0], NULL);
                if (!params[0]->type) break;

                SpvOp op;
                if (!isTypeCastable(params[0]->type, constructed_type, &op))
                {
                    addErr(compiler, &params[0]->loc, "value is not castable to this type");
                    break;
                }
            }
            else
            {
                addErr(compiler, &expr->loc, "invalid constructor");
                break;
            }
        }
        else if (func_type->kind == TYPE_FUNC)
        {
            // Actual function call

            if (func_type->func.param_count != arrLength(expr->func_call.params))
            {
                addErr(compiler, &expr->loc, "wrong amount of parameters for function call");
                break;
            }

            for (uint32_t i = 0; i < func_type->func.param_count; ++i)
            {
                AstExpr *param = expr->func_call.params[i];
                analyzerAnalyzeExpr(a, param, func_type->func.params[i]);
            }

            expr->type = func_type->func.return_type;
        }
        else
        {
            addErr(
                compiler,
                &expr->func_call.func_expr->loc,
                "expression does not represent a function");
        }

        break;
    }

    case EXPR_BUILTIN_CALL: {
        uint32_t param_count = arrLength(expr->builtin_call.params);
        AstExpr **params = expr->builtin_call.params;

        bool got_param_types = true;

        for (uint32_t i = 0; i < param_count; ++i)
        {
            AstExpr *param = params[i];
            analyzerAnalyzeExpr(a, param, NULL);
            if (!param->type)
            {
                got_param_types = false;
                continue;
            }
        }

        if (!got_param_types) break;

        switch (expr->builtin_call.kind)
        {
        case IR_BUILTIN_DOT: {
            if (param_count != 2)
            {
                addErr(compiler, &expr->loc, "dot needs 2 parameters");
                break;
            }

            AstExpr *a = params[0];
            AstExpr *b = params[1];

            if ((a->type->kind != TYPE_VECTOR) || (b->type->kind != TYPE_VECTOR))
            {
                addErr(compiler, &expr->loc, "dot operates on vectors");
                break;
            }

            if (a->type != b->type)
            {
                addErr(compiler, &expr->loc, "dot cannot operate on different types");
                break;
            }

            expr->type = a->type->vector.elem_type;
            break;
        }
        }

        break;
    }

    case EXPR_SAMPLER_TYPE: {
        expr->type = newBasicType(m, TYPE_TYPE);
        expr->as_type = newBasicType(m, TYPE_SAMPLER);
        break;
    }

    case EXPR_TEXTURE_TYPE: {
        Type *type_type = newBasicType(m, TYPE_TYPE);
        analyzerAnalyzeExpr(a, expr->texture.sampled_type_expr, type_type);
        if (!expr->texture.sampled_type_expr->type)
        {
            break;
        }

        Type *sampled_type = expr->texture.sampled_type_expr->as_type;
        assert(sampled_type);

        if ((sampled_type->kind != TYPE_FLOAT) && (sampled_type->kind != TYPE_INT))
        {
            addErr(compiler, &expr->loc, "invalid sampled type for texture");
        }

        expr->type = type_type;
        expr->as_type = newImageType(m, sampled_type, expr->texture.dim);
        break;
    }

    case EXPR_SAMPLED_TEXTURE_TYPE: {
        Type *type_type = newBasicType(m, TYPE_TYPE);
        analyzerAnalyzeExpr(a, expr->texture.sampled_type_expr, type_type);
        if (!expr->texture.sampled_type_expr->type)
        {
            break;
        }

        Type *sampled_type = expr->texture.sampled_type_expr->as_type;
        assert(sampled_type);

        if ((sampled_type->kind != TYPE_FLOAT) && (sampled_type->kind != TYPE_INT))
        {
            addErr(compiler, &expr->loc, "invalid sampled type for texture");
        }

        expr->type = type_type;
        Type *image_type = newImageType(m, sampled_type, expr->texture.dim);
        expr->as_type = newSampledImageType(m, image_type);
        break;
    }

    case EXPR_UNARY: {
        switch (expr->unary.op)
        {
        case UNOP_NEG: {
            analyzerAnalyzeExpr(a, expr->unary.right, NULL);
            if (!expr->unary.right->type) break;
            Type *right_type = expr->unary.right->type;
            Type *scalar_type = getScalarType(right_type);

            if (!scalar_type || (scalar_type->kind != TYPE_INT && scalar_type->kind != TYPE_FLOAT))
            {
                addErr(
                    compiler,
                    &expr->unary.right->loc,
                    "\'negation\' expression does not work on this type");
                break;
            }

            expr->type = right_type;

            break;
        }

        case UNOP_NOT: {
            analyzerAnalyzeExpr(a, expr->unary.right, NULL);
            if (!expr->unary.right->type) break;
            Type *right_type = expr->unary.right->type;
            Type *scalar_type = getScalarType(right_type);

            if (!scalar_type || (scalar_type->kind != TYPE_INT && scalar_type->kind != TYPE_BOOL))
            {
                addErr(
                    compiler,
                    &expr->unary.right->loc,
                    "\'not\' expression does not work on this type");
                break;
            }

            expr->type = right_type;

            break;
        }
        }

        break;
    }

    case EXPR_BINARY: {
        assert(0);
        break;
    }
    }

    if (expected_type)
    {
        if (!expr->type)
        {
            addErr(compiler, &expr->loc, "could not resolve type for expression");
        }
        else if (expr->type != expected_type)
        {
            addErr(compiler, &expr->loc, "unmatched types");
        }
    }
}

static void analyzerAnalyzeStmt(Analyzer *a, AstStmt *stmt)
{
    TscCompiler *compiler = a->compiler;

    switch (stmt->kind)
    {
    case STMT_DECL: {
        analyzerTryRegisterDecl(a, stmt->decl);
        analyzerAnalyzeDecl(a, stmt->decl);
        break;
    }

    case STMT_EXPR: {
        analyzerAnalyzeExpr(a, stmt->expr, NULL);
        break;
    }

    case STMT_RETURN: {
        assert(a->scope_func);
        Type *return_type = a->scope_func->type->func.return_type;

        if (return_type->kind == TYPE_VOID && stmt->return_.value)
        {
            addErr(compiler, &stmt->loc, "function does not return a value");
        }

        if (return_type->kind != TYPE_VOID && !stmt->return_.value)
        {
            addErr(compiler, &stmt->loc, "function needs a return value");
        }

        if (stmt->return_.value)
        {
            analyzerAnalyzeExpr(a, stmt->return_.value, return_type);
        }
        break;
    }

    case STMT_VAR_ASSIGN: {
        analyzerAnalyzeExpr(a, stmt->var_assign.assigned_expr, NULL);
        if (!stmt->var_assign.assigned_expr->assignable)
        {
            addErr(compiler, &stmt->var_assign.assigned_expr->loc, "expression is not assignable");
        }

        analyzerAnalyzeExpr(a, stmt->var_assign.value_expr, stmt->var_assign.assigned_expr->type);

        break;
    }
    }
}

static void analyzerAnalyzeDecl(Analyzer *a, AstDecl *decl)
{
    TscCompiler *compiler = a->compiler;
    Module *m = a->module;
    Scope *scope = analyzerCurrentScope(a);

    for (uint32_t i = 0; i < arrLength(decl->attributes); ++i)
    {
        AstAttribute *attr = &decl->attributes[i];
        for (uint32_t j = 0; j < arrLength(attr->values); ++j)
        {
            analyzerAnalyzeExpr(a, attr->values[j], NULL);
        }
    }

    switch (decl->kind)
    {
    case DECL_FUNC: {
        for (uint32_t i = 0; i < arrLength(decl->attributes); ++i)
        {
            AstAttribute *attrib = &decl->attributes[i];

            if (stringEquals("vk::vertex", attrib->name))
            {
                decl->func.execution_model = NEW(compiler, SpvExecutionModel);
                *decl->func.execution_model = SpvExecutionModelVertex;
            }
            else if (stringEquals("vk::fragment", attrib->name))
            {
                decl->func.execution_model = NEW(compiler, SpvExecutionModel);
                *decl->func.execution_model = SpvExecutionModelFragment;
            }
        }

        for (uint32_t i = 0; i < arrLength(decl->func.parameters); ++i)
        {
            AstDecl *param_decl = decl->func.parameters[i];

            if (param_decl->var.storage_class == SpvStorageClassInput)
            {
                arrPush(decl->func.inputs, param_decl);
            }
            else if (param_decl->var.storage_class == SpvStorageClassOutput)
            {
                arrPush(decl->func.outputs, param_decl);
            }
            else if (decl->func.execution_model)
            {
                param_decl->var.storage_class = SpvStorageClassInput;
                param_decl->var.kind = VAR_INPUT;
                arrPush(decl->func.inputs, param_decl);
            }
            else
            {
                arrPush(decl->func.func_params, param_decl);
            }
        }

        decl->scope = NEW(compiler, Scope);
        scopeInit(decl->scope, scope, decl);

        analyzerAnalyzeExpr(a, decl->func.return_type, newBasicType(m, TYPE_TYPE));
        Type *return_type = decl->func.return_type->as_type;

        if (!return_type)
        {
            addErr(compiler, &decl->loc, "could not resolve return type for function");
            break;
        }

        bool param_types_valid = true;
        Type **param_types = NULL;
        if (arrLength(decl->func.func_params) > 0)
        {
            param_types = NEW_ARRAY(compiler, Type *, arrLength(decl->func.func_params));
        }

        analyzerPushScope(a, decl->scope);
        for (uint32_t i = 0; i < arrLength(decl->func.inputs); ++i)
        {
            AstDecl *param_decl = decl->func.inputs[i];

            IRDecoration dec = {0};
            dec.kind = SpvDecorationLocation;
            dec.value = i;
            arrPush(param_decl->decorations, dec);

            analyzerTryRegisterDecl(a, param_decl);
            analyzerAnalyzeDecl(a, param_decl);
        }

        for (uint32_t i = 0; i < arrLength(decl->func.outputs); ++i)
        {
            AstDecl *param_decl = decl->func.outputs[i];

            IRDecoration dec = {0};
            dec.kind = SpvDecorationLocation;
            dec.value = i;
            arrPush(param_decl->decorations, dec);

            analyzerTryRegisterDecl(a, param_decl);
            analyzerAnalyzeDecl(a, param_decl);
        }

        for (uint32_t i = 0; i < arrLength(decl->func.func_params); ++i)
        {
            AstDecl *param_decl = decl->func.func_params[i];
            assert(param_decl->kind == DECL_VAR);

            analyzerTryRegisterDecl(a, param_decl);
            analyzerAnalyzeDecl(a, param_decl);
            param_types[i] = param_decl->type;
            if (!param_types[i])
            {
                addErr(compiler, &param_decl->loc, "could not resolve type for function parameter");
                param_types_valid = false;
            }
        }

        if (param_types_valid)
        {
            decl->type =
                newFuncType(m, return_type, param_types, arrLength(decl->func.func_params));
        }

        for (uint32_t i = 0; i < arrLength(decl->func.stmts); ++i)
        {
            AstStmt *stmt = decl->func.stmts[i];
            analyzerAnalyzeStmt(a, stmt);
        }
        analyzerPopScope(a, decl->scope);

        break;
    }

    case DECL_VAR: {
        if (decl->var.storage_class == SpvStorageClassFunction && !a->scope_func)
        {
            addErr(compiler, &decl->loc, "variable declaration must be inside a function");
            break;
        }

        analyzerAnalyzeExpr(a, decl->var.type_expr, newBasicType(m, TYPE_TYPE));
        if (!decl->var.type_expr->as_type)
        {
            break;
        }

        if (decl->var.value_expr)
        {
            analyzerAnalyzeExpr(a, decl->var.value_expr, decl->var.type_expr->as_type);
        }

        decl->type = decl->var.type_expr->as_type;

        if ((decl->var.kind == VAR_FUNCTION) && a->scope_func)
        {
            arrPush(a->scope_func->func.var_decls, decl);
        }

        if (decl->type->kind == TYPE_STRUCT)
        {
            decl->scope = NEW(compiler, Scope);
            scopeInit(decl->scope, NULL, decl);
            for (uint32_t i = 0; i < decl->type->struct_.field_count; ++i)
            {
                AstDecl *field_decl = decl->type->struct_.field_decls[i];
                scopeAdd(decl->scope, field_decl->name, field_decl);
            }
        }

        // Add decorations
        for (uint32_t i = 0; i < arrLength(decl->attributes); ++i)
        {
            AstAttribute *attr = &decl->attributes[i];

            if (stringEquals(attr->name, "vk::binding"))
            {
                if (arrLength(attr->values) >= 1)
                {
                    if (!attr->values[0]->resolved_int)
                    {
                        addErr(
                            compiler,
                            &attr->values[0]->loc,
                            "could not resolve integer from expression");
                    }
                    else
                    {
                        IRDecoration dec = {0};
                        dec.kind = SpvDecorationBinding;
                        dec.value = *attr->values[0]->resolved_int;
                        arrPush(decl->decorations, dec);
                    }
                }

                if (arrLength(attr->values) >= 2)
                {
                    if (!attr->values[1]->resolved_int)
                    {
                        addErr(
                            compiler,
                            &attr->values[1]->loc,
                            "could not resolve integer from expression");
                    }
                    else
                    {
                        IRDecoration dec = {0};
                        dec.kind = SpvDecorationDescriptorSet;
                        dec.value = *attr->values[1]->resolved_int;
                        arrPush(decl->decorations, dec);
                    }
                }
            }
        }

        break;
    }

    case DECL_CONST: {
        analyzerAnalyzeExpr(a, decl->constant.type_expr, newBasicType(m, TYPE_TYPE));
        if (!decl->constant.type_expr->as_type)
        {
            addErr(compiler, &decl->loc, "constant type expression does not represent a type");
        }

        analyzerAnalyzeExpr(a, decl->constant.value_expr, decl->constant.type_expr->as_type);

        decl->type = decl->constant.type_expr->as_type;
        decl->resolved_int = decl->constant.value_expr->resolved_int;

        break;
    }

    case DECL_STRUCT_FIELD: {
        analyzerAnalyzeExpr(a, decl->struct_field.type_expr, newBasicType(m, TYPE_TYPE));
        if (!decl->struct_field.type_expr->as_type)
        {
            addErr(compiler, &decl->loc, "struct field type expression does not represent a type");
        }

        decl->type = decl->struct_field.type_expr->as_type;

        if (decl->type->kind == TYPE_STRUCT)
        {
            decl->scope = NEW(compiler, Scope);
            scopeInit(decl->scope, NULL, decl);
            for (uint32_t i = 0; i < decl->type->struct_.field_count; ++i)
            {
                AstDecl *field_decl = decl->type->struct_.field_decls[i];
                scopeAdd(decl->scope, field_decl->name, field_decl);
            }
        }

        break;
    }

    case DECL_STRUCT: {
        uint32_t field_count = arrLength(decl->struct_.fields);
        Type **field_types = NEW_ARRAY(compiler, Type *, field_count);

        decl->scope = NEW(compiler, Scope);
        scopeInit(decl->scope, scope, NULL);

        analyzerPushScope(a, decl->scope);
        for (uint32_t i = 0; i < arrLength(decl->struct_.fields); ++i)
        {
            AstDecl *field = decl->struct_.fields[i];
            field->struct_field.index = i;
            analyzerTryRegisterDecl(a, field);
            analyzerAnalyzeDecl(a, field);

            field_types[i] = field->type;
        }
        analyzerPopScope(a, decl->scope);

        decl->type = newBasicType(m, TYPE_TYPE);
        decl->as_type =
            newStructType(m, decl->name, field_types, decl->struct_.fields, field_count);

        break;
    }
    }
}

static void analyzerAnalyze(Analyzer *a, TscCompiler *compiler, Module *module)
{
    memset(a, 0, sizeof(*a));
    a->compiler = compiler;
    a->module = module;

    for (uint32_t i = 0; i < arrLength(a->module->files); ++i)
    {
        File *file = a->module->files[i];

        analyzerPushScope(a, a->module->scope);

        for (uint32_t j = 0; j < arrLength(file->decls); ++j)
        {
            AstDecl *decl = file->decls[j];
            analyzerTryRegisterDecl(a, decl);
        }

        analyzerPopScope(a, a->module->scope);
    }

    for (uint32_t i = 0; i < arrLength(a->module->files); ++i)
    {
        File *file = a->module->files[i];

        analyzerPushScope(a, a->module->scope);

        for (uint32_t j = 0; j < arrLength(file->decls); ++j)
        {
            AstDecl *decl = file->decls[j];
            analyzerAnalyzeDecl(a, decl);
        }

        analyzerPopScope(a, a->module->scope);
    }
}
// }}}

// Codegen functions {{{
static void irModuleBuildDecl(IRModule *m, AstDecl *decl);
static void irModuleBuildStmt(IRModule *m, AstStmt *stmt);
static void irModuleBuildExpr(IRModule *m, AstExpr *expr);

static uint32_t irModuleReserveId(IRModule *m)
{
    return m->id_bound++;
}

static void irAddEntryPoint(
    IRModule *m,
    char *name,
    IRInst *func,
    SpvExecutionModel execution_model,
    IRInst **globals,
    uint32_t global_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_ENTRY_POINT;
    inst->entry_point.name = name;
    inst->entry_point.func = func;
    inst->entry_point.execution_model = execution_model;
    inst->entry_point.globals = globals;
    inst->entry_point.global_count = global_count;

    arrPush(m->entry_points, inst);
}

static IRInst *irAddFunction(IRModule *m, Type *func_type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_FUNCTION;
    inst->type = func_type;

    arrPush(m->functions, inst);

    return inst;
}

static IRInst *irAddFuncParam(IRModule *m, IRInst *func, Type *type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_FUNC_PARAM;
    inst->type = type;

    arrPush(func->func.params, inst);

    return inst;
}

static IRInst *irAddBlock(IRModule *m, IRInst *func)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BLOCK;

    assert(func->kind == IR_INST_FUNCTION);
    arrPush(func->func.blocks, inst);

    return inst;
}

static IRInst *irAddGlobal(IRModule *m, Type *type, SpvStorageClass storage_class)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = storage_class;
    inst->type = newPointerType(m->mod, inst->var.storage_class, type);

    arrPush(m->globals, inst);

    return inst;
}

static IRInst *irGetCurrentBlock(IRModule *m)
{
    return m->current_block;
}

static void irPositionAtEnd(IRModule *m, IRInst *block)
{
    m->current_block = block;
}

static bool irBlockHasTerminator(IRInst *block)
{
    if (arrLength(block->block.insts) == 0) return false;

    IRInst *last_inst = block->block.insts[arrLength(block->block.insts) - 1];
    switch (last_inst->kind)
    {
    case IR_INST_RETURN: return true;
    default: return false;
    }

    return false;
}

static IRInst *irBuildConstFloat(IRModule *m, Type *type, double value)
{
    assert(type->kind == TYPE_FLOAT);

    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_CONSTANT;

    inst->type = type;

    if (type->float_.bits == 32)
    {
        float *new_value = NEW(m->compiler, float);
        *new_value = (float)value;
        inst->constant.value = new_value;
        inst->constant.value_size_bytes = sizeof(float);
    }
    else if (type->float_.bits == 64)
    {
        double *new_value = NEW(m->compiler, double);
        *new_value = value;
        inst->constant.value = new_value;
        inst->constant.value_size_bytes = sizeof(double);
    }
    else
    {
        assert(0);
    }

    arrPush(m->constants, inst);

    return inst;
}

static IRInst *irBuildConstInt(IRModule *m, Type *type, uint64_t value)
{
    assert(type->kind == TYPE_INT);

    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_CONSTANT;

    inst->type = type;

    inst->constant.value_size_bytes = type->int_.bits / 8;

    switch (type->int_.bits)
    {
    case 8: {
        uint8_t *new_value = NEW(m->compiler, uint8_t);
        *new_value = (uint8_t)value;
        inst->constant.value = new_value;
        break;
    }

    case 16: {
        uint16_t *new_value = NEW(m->compiler, uint16_t);
        *new_value = (uint16_t)value;
        inst->constant.value = new_value;
        break;
    }

    case 32: {
        uint32_t *new_value = NEW(m->compiler, uint32_t);
        *new_value = (uint32_t)value;
        inst->constant.value = new_value;
        break;
    }

    case 64: {
        uint64_t *new_value = NEW(m->compiler, uint64_t);
        *new_value = (uint64_t)value;
        inst->constant.value = new_value;
        break;
    }

    default: assert(0);
    }

    arrPush(m->constants, inst);

    return inst;
}

static IRInst *irBuildAlloca(IRModule *m, Type *type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = SpvStorageClassFunction;
    inst->type = newPointerType(m->mod, inst->var.storage_class, type);

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static void irBuildStore(IRModule *m, IRInst *pointer, IRInst *value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_STORE;
    inst->store.pointer = pointer;
    inst->store.value = value;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);
}

static IRInst *irBuildLoad(IRModule *m, IRInst *pointer)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_LOAD;

    assert(pointer->type);
    assert(pointer->type->kind == TYPE_POINTER);

    inst->type = pointer->type->ptr.sub;
    inst->load.pointer = pointer;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *
irBuildAccessChain(IRModule *m, Type *type, IRInst *base, IRInst **indices, uint32_t index_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_ACCESS_CHAIN;

    inst->type = newPointerType(m->mod, base->type->ptr.storage_class, type);

    assert(base->type->kind == TYPE_POINTER);

    inst->access_chain.base = base;
    inst->access_chain.indices = indices;
    inst->access_chain.index_count = index_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildVectorShuffle(
    IRModule *m, IRInst *vector_a, IRInst *vector_b, uint32_t *indices, uint32_t index_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_VECTOR_SHUFFLE;

    assert(vector_a->type->kind == TYPE_VECTOR);
    assert(vector_b->type->kind == TYPE_VECTOR);
    assert(vector_a->type->vector.elem_type == vector_b->type->vector.elem_type);

    inst->type = newVectorType(m->mod, vector_a->type->vector.elem_type, index_count);

    inst->vector_shuffle.vector_a = vector_a;
    inst->vector_shuffle.vector_b = vector_b;
    inst->vector_shuffle.indices = indices;
    inst->vector_shuffle.index_count = index_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *
irBuildCompositeExtract(IRModule *m, IRInst *value, uint32_t *indices, uint32_t index_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_COMPOSITE_EXTRACT;

    if (value->type->kind == TYPE_VECTOR)
    {
        if (index_count == 1)
        {
            inst->type = value->type->vector.elem_type;
        }
        else
        {
            inst->type = newVectorType(m->mod, value->type->vector.elem_type, index_count);
        }
    }
    else
    {
        assert(0);
    }

    inst->composite_extract.value = value;
    inst->composite_extract.indices = indices;
    inst->composite_extract.index_count = index_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *
irBuildCompositeConstruct(IRModule *m, Type *type, IRInst **fields, uint32_t field_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_COMPOSITE_CONSTRUCT;

    inst->type = type;

    inst->composite_construct.fields = fields;
    inst->composite_construct.field_count = field_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildFuncCall(IRModule *m, IRInst *function, IRInst **params, uint32_t param_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_FUNC_CALL;

    inst->type = function->type->func.return_type;
    assert(inst->type);

    inst->func_call.func = function;
    inst->func_call.params = params;
    inst->func_call.param_count = param_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *
irBuildBuiltinCall(IRModule *m, IRBuiltinInstKind kind, IRInst **params, uint32_t param_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BUILTIN_CALL;

    switch (kind)
    {
    case IR_BUILTIN_DOT: {
        assert(param_count == 2);
        IRInst *a = params[0];
        IRInst *b = params[1];

        assert(a->type->kind == TYPE_VECTOR);
        assert(b->type == a->type);

        inst->type = a->type->vector.elem_type;
        assert(inst->type);

        break;
    }
    }

    assert(inst->type);

    inst->builtin_call.kind = kind;
    inst->builtin_call.params = params;
    inst->builtin_call.param_count = param_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildCast(IRModule *m, Type *dst_type, IRInst *value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_CAST;
    inst->type = dst_type;

    Type *src_type = value->type;
    assert(src_type);

    bool castable = isTypeCastable(src_type, dst_type, &inst->cast.op);
    assert(castable);

    if (inst->cast.op == SpvOpNop)
    {
        inst->cast.redundant = true;
    }

    inst->cast.dst_type = dst_type;
    inst->cast.value = value;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildUnary(IRModule *m, SpvOp op, Type *type, IRInst *right)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_UNARY;
    inst->type = type;
    assert(inst->type);

    inst->unary.op = op;
    inst->unary.right = right;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static void irBuildReturn(IRModule *m, IRInst *value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_RETURN;
    inst->return_.value = value;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);
}

static void irModuleEncodeInst(IRModule *m, SpvOp opcode, uint32_t *params, size_t params_count)
{
    uint32_t opcode_word = opcode;
    opcode_word |= ((uint16_t)(params_count + 1)) << 16;

    arrPush(m->stream, opcode_word);
    for (uint32_t i = 0; i < params_count; ++i)
    {
        arrPush(m->stream, params[i]);
    }
}

static void irModuleEncodeDecorations(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->globals); ++i)
    {
        IRInst *inst = m->globals[i];
        assert(inst->kind == IR_INST_VARIABLE);
        assert(inst->id);

        for (uint32_t j = 0; j < arrLength(inst->decorations); ++j)
        {
            IRDecoration *dec = &inst->decorations[j];
            uint32_t params[3] = {inst->id, dec->kind, dec->value};
            irModuleEncodeInst(m, SpvOpDecorate, params, 3);
        }
    }
}

static void irModuleEncodeTypes(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->mod->type_cache.values); ++i)
    {
        Type *type = (Type *)m->mod->type_cache.values[i];
        type->id = irModuleReserveId(m);
    }

    for (uint32_t i = 0; i < arrLength(m->mod->type_cache.values); ++i)
    {
        Type *type = (Type *)m->mod->type_cache.values[i];

        switch (type->kind)
        {
        case TYPE_VOID: {
            irModuleEncodeInst(m, SpvOpTypeVoid, &type->id, 1);
            break;
        }

        case TYPE_BOOL: {
            uint32_t params[3] = {type->id, 32, false};
            irModuleEncodeInst(m, SpvOpTypeInt, params, 3);
            break;
        }

        case TYPE_FLOAT: {
            uint32_t params[2] = {type->id, type->float_.bits};
            irModuleEncodeInst(m, SpvOpTypeFloat, params, 2);
            break;
        }

        case TYPE_INT: {
            uint32_t params[3] = {type->id, type->int_.bits, (uint32_t)type->int_.is_signed};
            irModuleEncodeInst(m, SpvOpTypeInt, params, 3);
            break;
        }

        case TYPE_POINTER: {
            uint32_t params[3] = {type->id, type->ptr.storage_class, type->ptr.sub->id};
            irModuleEncodeInst(m, SpvOpTypePointer, params, 3);
            break;
        }

        case TYPE_FUNC: {
            uint32_t param_count = 2 + type->func.param_count;
            uint32_t *params = NEW_ARRAY(m->mod->compiler, uint32_t, param_count);

            params[0] = type->id;
            params[1] = type->func.return_type->id;

            for (uint32_t j = 0; j < type->func.param_count; ++j)
            {
                Type *param_type = type->func.params[j];
                params[2 + j] = param_type->id;
            }

            irModuleEncodeInst(m, SpvOpTypeFunction, params, param_count);
            break;
        }

        case TYPE_VECTOR: {
            uint32_t params[3] = {type->id, type->vector.elem_type->id, type->vector.size};
            irModuleEncodeInst(m, SpvOpTypeVector, params, 3);
            break;
        }

        case TYPE_MATRIX: {
            uint32_t params[3] = {type->id, type->matrix.col_type->id, type->matrix.col_count};
            irModuleEncodeInst(m, SpvOpTypeMatrix, params, 3);
            break;
        }

        case TYPE_STRUCT: {
            uint32_t word_count = 1 + type->struct_.field_count;
            uint32_t *params = NEW_ARRAY(m->mod->compiler, uint32_t, word_count);
            params[0] = type->id;

            for (uint32_t i = 0; i < type->struct_.field_count; ++i)
            {
                params[1 + i] = type->struct_.fields[i]->id;
            }

            irModuleEncodeInst(m, SpvOpTypeStruct, params, word_count);
            break;
        }

        case TYPE_SAMPLER: {
            uint32_t params[1] = {
                type->id,
            };
            irModuleEncodeInst(m, SpvOpTypeSampler, params, 1);
            break;
        }

        case TYPE_IMAGE: {
            uint32_t params[8] = {
                type->id,
                type->image.sampled_type->id,
                type->image.dim,
                type->image.depth,
                type->image.arrayed,
                type->image.multisampled,
                type->image.sampled,
                type->image.format,
            };
            irModuleEncodeInst(m, SpvOpTypeImage, params, 8);
            break;
        }

        case TYPE_SAMPLED_IMAGE: {
            uint32_t params[2] = {
                type->id,
                type->sampled_image.image_type->id,
            };
            irModuleEncodeInst(m, SpvOpTypeSampledImage, params, 2);
            break;
        }

        case TYPE_TYPE: break;
        }
    }
}

static void irModuleEncodeEntryPoints(IRModule *m)
{
    TscCompiler *compiler = m->compiler;

    for (uint32_t i = 0; i < arrLength(m->entry_points); ++i)
    {
        IRInst *inst = m->entry_points[i];
        assert(inst->kind == IR_INST_ENTRY_POINT);
        assert(inst->entry_point.func);
        assert(inst->entry_point.func->id);

        {
            uint32_t name_len = strlen(inst->entry_point.name);
            uint32_t name_words = ROUND_TO_4(name_len + 1) / 4;

            uint32_t param_count = 2 + name_words + inst->entry_point.global_count;
            uint32_t *params = NEW_ARRAY(compiler, uint32_t, param_count);

            params[0] = inst->entry_point.execution_model;
            params[1] = inst->entry_point.func->id;

            memcpy(&params[2], inst->entry_point.name, name_len);

            uint32_t globals_start = 2 + name_words;
            for (uint32_t j = 0; j < inst->entry_point.global_count; ++j)
            {
                params[globals_start + j] = inst->entry_point.globals[j]->id;
            }

            irModuleEncodeInst(m, SpvOpEntryPoint, params, param_count);
        }

        if (inst->entry_point.execution_model == SpvExecutionModelFragment)
        {
            uint32_t params[2] = {inst->entry_point.func->id, SpvExecutionModeOriginUpperLeft};
            irModuleEncodeInst(m, SpvOpExecutionMode, params, 2);
        }
    }
}

static void irModuleEncodeBlock(IRModule *m, IRInst *block)
{
    assert(irBlockHasTerminator(block));

    block->id = irModuleReserveId(m);

    {
        uint32_t params[1] = {block->id};
        irModuleEncodeInst(m, SpvOpLabel, params, 1);
    }

    for (uint32_t i = 0; i < arrLength(block->block.insts); ++i)
    {
        IRInst *inst = block->block.insts[i];
        switch (inst->kind)
        {
        case IR_INST_VARIABLE: {
            inst->id = irModuleReserveId(m);

            Type *ptr_type = inst->type;
            SpvStorageClass storage_class = inst->var.storage_class;
            IRInst *initializer = inst->var.initializer;

            if (initializer)
            {
                uint32_t initializer_id = initializer->id;
                assert(initializer_id);
                uint32_t params[4] = {ptr_type->id, inst->id, storage_class, initializer_id};
                irModuleEncodeInst(m, SpvOpVariable, params, 4);
            }
            else
            {
                uint32_t params[3] = {ptr_type->id, inst->id, storage_class};
                irModuleEncodeInst(m, SpvOpVariable, params, 3);
            }
            break;
        }

        case IR_INST_RETURN: {
            if (inst->return_.value)
            {
                uint32_t params[1] = {inst->return_.value->id};
                irModuleEncodeInst(m, SpvOpReturnValue, params, 1);
            }
            else
            {
                irModuleEncodeInst(m, SpvOpReturn, NULL, 0);
            }
            break;
        }

        case IR_INST_LOAD: {
            inst->id = irModuleReserveId(m);

            Type *loaded_type = inst->load.pointer->type;
            assert(loaded_type->kind == TYPE_POINTER);
            loaded_type = loaded_type->ptr.sub;

            uint32_t params[3] = {loaded_type->id, inst->id, inst->load.pointer->id};
            irModuleEncodeInst(m, SpvOpLoad, params, 3);
            break;
        }

        case IR_INST_STORE: {
            uint32_t params[2] = {inst->store.pointer->id, inst->store.value->id};
            irModuleEncodeInst(m, SpvOpStore, params, 2);
            break;
        }

        case IR_INST_ACCESS_CHAIN: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 3 + inst->access_chain.index_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            assert(inst->type->id > 0);
            assert(inst->access_chain.base->id > 0);

            params[0] = inst->type->id;
            params[1] = inst->id;
            params[2] = inst->access_chain.base->id;

            for (uint32_t i = 0; i < inst->access_chain.index_count; ++i)
            {
                assert(inst->access_chain.indices[i]->id);
                params[3 + i] = inst->access_chain.indices[i]->id;
            }

            irModuleEncodeInst(m, SpvOpAccessChain, params, param_count);
            break;
        }

        case IR_INST_COMPOSITE_CONSTRUCT: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 2 + inst->composite_construct.field_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            assert(inst->type->id > 0);

            params[0] = inst->type->id;
            params[1] = inst->id;

            for (uint32_t i = 0; i < inst->composite_construct.field_count; ++i)
            {
                assert(inst->composite_construct.fields[i]->id);
                params[2 + i] = inst->composite_construct.fields[i]->id;
            }

            irModuleEncodeInst(m, SpvOpCompositeConstruct, params, param_count);
            break;
        }

        case IR_INST_FUNC_CALL: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 3 + inst->func_call.param_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            params[0] = inst->type->id;
            params[1] = inst->id;
            params[2] = inst->func_call.func->id;
            for (uint32_t i = 0; i < inst->func_call.param_count; ++i)
            {
                assert(inst->func_call.params[i]->id);
                params[3 + i] = inst->func_call.params[i]->id;
            }

            irModuleEncodeInst(m, SpvOpFunctionCall, params, param_count);
            break;
        }

        case IR_INST_BUILTIN_CALL: {
            inst->id = irModuleReserveId(m);

            uint32_t param_value_count = inst->builtin_call.param_count;
            IRInst **param_values = inst->builtin_call.params;

            for (uint32_t i = 0; i < param_value_count; ++i)
            {
                assert(param_values[i]);
                assert(param_values[i]->id);
            }

            switch (inst->builtin_call.kind)
            {
            case IR_BUILTIN_DOT: {
                IRInst *a = param_values[0];
                IRInst *b = param_values[1];
                uint32_t params[4] = {inst->type->id, inst->id, a->id, b->id};
                irModuleEncodeInst(m, SpvOpDot, params, 4);
                break;
            }
            }

            break;
        }

        case IR_INST_CAST: {
            if (inst->cast.redundant)
            {
                inst->id = inst->cast.value->id;
                break;
            }

            inst->id = irModuleReserveId(m);
            assert(inst->cast.op != SpvOpNop);

            uint32_t params[3] = {inst->type->id, inst->id, inst->cast.value->id};
            irModuleEncodeInst(m, inst->cast.op, params, 3);

            break;
        }

        case IR_INST_COMPOSITE_EXTRACT: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 3 + inst->composite_extract.index_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            assert(inst->type->id > 0);

            params[0] = inst->type->id;
            params[1] = inst->id;
            params[2] = inst->composite_extract.value->id;

            for (uint32_t i = 0; i < inst->composite_extract.index_count; ++i)
            {
                params[4 + i] = inst->composite_extract.indices[i];
            }

            irModuleEncodeInst(m, SpvOpCompositeExtract, params, param_count);
            break;
        }

        case IR_INST_VECTOR_SHUFFLE: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 4 + inst->vector_shuffle.index_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            assert(inst->type->id > 0);

            params[0] = inst->type->id;
            params[1] = inst->id;
            params[2] = inst->vector_shuffle.vector_a->id;
            params[3] = inst->vector_shuffle.vector_b->id;

            for (uint32_t i = 0; i < inst->vector_shuffle.index_count; ++i)
            {
                params[4 + i] = inst->vector_shuffle.indices[i];
            }

            irModuleEncodeInst(m, SpvOpVectorShuffle, params, param_count);
            break;
        }

        case IR_INST_UNARY: {
            inst->id = irModuleReserveId(m);
            assert(inst->type->id > 0);

            uint32_t param_count = 3;
            uint32_t params[3] = {inst->type->id, inst->id, inst->unary.right->id};

            irModuleEncodeInst(m, inst->unary.op, params, param_count);
            break;
        }

        case IR_INST_BINARY: {
            inst->id = irModuleReserveId(m);
            assert(inst->type->id > 0);

            uint32_t param_count = 4;
            uint32_t params[4] = {
                inst->type->id, inst->id, inst->binary.left->id, inst->binary.right->id};

            irModuleEncodeInst(m, inst->binary.op, params, param_count);
            break;
        }

        case IR_INST_FUNC_PARAM:
        case IR_INST_CONSTANT:
        case IR_INST_FUNCTION:
        case IR_INST_ENTRY_POINT:
        case IR_INST_BLOCK: assert(0); break;
        }
    }
}

static void irModuleEncodeConstants(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->constants); ++i)
    {
        IRInst *inst = m->constants[i];
        assert(inst->kind == IR_INST_CONSTANT);
        inst->id = irModuleReserveId(m);

        uint32_t value_words = ROUND_TO_4(inst->constant.value_size_bytes) / 4;
        assert((value_words * 4) >= inst->constant.value_size_bytes);
        assert(value_words > 0);

        uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, 2 + value_words);
        params[0] = inst->type->id;
        params[1] = inst->id;
        assert(params[0] != params[1]);
        memcpy(&params[2], inst->constant.value, inst->constant.value_size_bytes);

        irModuleEncodeInst(m, SpvOpConstant, params, 2 + value_words);
    }
}

static void irModuleEncodeGlobals(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->globals); ++i)
    {
        IRInst *inst = m->globals[i];
        assert(inst->kind == IR_INST_VARIABLE);

        uint32_t params[3] = {inst->type->id, inst->id, inst->var.storage_class};
        irModuleEncodeInst(m, SpvOpVariable, params, 3);
    }
}

static void irModuleEncodeFunctions(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->functions); ++i)
    {
        IRInst *inst = m->functions[i];
        assert(inst->kind == IR_INST_FUNCTION);
        assert(inst->id);

        {
            uint32_t params[4] = {
                inst->type->func.return_type->id,
                inst->id,
                SpvFunctionControlMaskNone,
                inst->type->id,
            };
            irModuleEncodeInst(m, SpvOpFunction, params, 4);
        }

        for (uint32_t j = 0; j < arrLength(inst->func.params); ++j)
        {
            IRInst *func_param = inst->func.params[j];
            uint32_t params[2] = {
                func_param->type->id,
                func_param->id,
            };
            irModuleEncodeInst(m, SpvOpFunctionParameter, params, 2);
        }

        for (uint32_t j = 0; j < arrLength(inst->func.blocks); ++j)
        {
            IRInst *block = inst->func.blocks[j];
            irModuleEncodeBlock(m, block);
        }

        irModuleEncodeInst(m, SpvOpFunctionEnd, NULL, 0);
    }
}

static void irModuleEncodeModule(IRModule *m)
{
    assert(m->stream == NULL);

    static const uint8_t MAGIC_NUMBER[4] = {'S', 'L', 'I', 'P'};
    uint32_t uint_magic_number;
    memcpy(&uint_magic_number, MAGIC_NUMBER, sizeof(uint32_t));

    arrPush(m->stream, SpvMagicNumber);
    arrPush(m->stream, SpvVersion);
    arrPush(m->stream, uint_magic_number);
    arrPush(m->stream, 0); // ID Bound (fill out later)
    arrPush(m->stream, 0);

    {
        uint32_t params[1] = {SpvCapabilityShader};
        irModuleEncodeInst(m, SpvOpCapability, params, 1);
    }

    {
        uint32_t params[5];
        memset(params, 0, sizeof(params));

        params[0] = irModuleReserveId(m);

        char *str = "GLSL.std.450";
        memcpy(&params[1], str, 12);

        irModuleEncodeInst(m, SpvOpExtInstImport, params, 5);
    }

    {
        uint32_t params[2] = {SpvAddressingModelLogical, SpvMemoryModelGLSL450};
        irModuleEncodeInst(m, SpvOpMemoryModel, params, 2);
    }

    irModuleEncodeEntryPoints(m);

    irModuleEncodeDecorations(m);

    irModuleEncodeTypes(m);

    irModuleEncodeConstants(m);

    irModuleEncodeGlobals(m);

    irModuleEncodeFunctions(m);

    // Fill out ID bound
    m->stream[3] = m->id_bound;
}

static bool isLvalue(IRInst *value)
{
    return value->kind == IR_INST_VARIABLE || value->kind == IR_INST_ACCESS_CHAIN;
}

static IRInst *irLoadVal(IRModule *m, IRInst *value)
{
    assert(value);
    if (isLvalue(value))
    {
        return irBuildLoad(m, value);
    }

    return value;
}

static void irModuleBuildExpr(IRModule *m, AstExpr *expr)
{
    TscCompiler *compiler = m->compiler;

    assert(expr->type);

    switch (expr->kind)
    {
    case EXPR_PRIMARY: {
        switch (expr->primary.token->kind)
        {
        case TOKEN_FLOAT_LIT: {
            expr->value = irBuildConstFloat(m, expr->type, expr->primary.token->double_);
            break;
        }

        case TOKEN_INT_LIT: {
            switch (expr->type->kind)
            {
            case TYPE_FLOAT: {
                expr->value = irBuildConstFloat(m, expr->type, (double)expr->primary.token->int_);
                break;
            }

            case TYPE_INT: {
                expr->value = irBuildConstInt(m, expr->type, (uint64_t)expr->primary.token->int_);
                break;
            }

            default: assert(0);
            }
            break;
        }

        default: assert(0);
        }
        break;
    }

    case EXPR_IDENT: {
        AstDecl *decl = expr->ident.decl;
        assert(decl);
        expr->value = decl->value;
        break;
    }

    case EXPR_ACCESS: {
        Type *index_type = newIntType(m->mod, 32, false);

        AstExpr *base = expr->access.base;
        assert(base->type);

        irModuleBuildExpr(m, base);
        IRInst *value = base->value;
        assert(value);

        uint32_t index_count = 0;
        IRInst **indices = NEW_ARRAY(compiler, IRInst *, arrLength(expr->access.chain));

        if (base->type->kind == TYPE_STRUCT)
        {
            for (uint32_t i = 0; i < arrLength(expr->access.chain); ++i)
            {
                AstExpr *field_ident = expr->access.chain[i];
                assert(field_ident->kind == EXPR_IDENT);

                if (!field_ident->ident.decl) break;

                index_count++;

                AstDecl *field_decl = field_ident->ident.decl;
                assert(field_decl->kind == DECL_STRUCT_FIELD);

                indices[i] = irBuildConstInt(m, index_type, field_decl->struct_field.index);
            }

            Type *last_type = expr->access.chain[index_count - 1]->type;
            value = irBuildAccessChain(m, last_type, base->value, indices, index_count);
        }

        for (uint32_t i = index_count; i < arrLength(expr->access.chain); ++i)
        {
            AstExpr *field_ident = expr->access.chain[i];
            assert(field_ident->kind == EXPR_IDENT);
            assert(!field_ident->ident.decl);
            assert(field_ident->ident.shuffle_indices);
            assert(field_ident->ident.shuffle_index_count > 0);

            uint32_t *shuffle_indices = field_ident->ident.shuffle_indices;
            uint32_t shuffle_index_count = field_ident->ident.shuffle_index_count;

            if (shuffle_index_count == 1)
            {
                if (!isLvalue(value))
                {
                    // It's a temporary value
                    IRInst *vec_value = irLoadVal(m, value);
                    value =
                        irBuildCompositeExtract(m, vec_value, shuffle_indices, shuffle_index_count);
                }
                else
                {
                    // It's a variable
                    IRInst **ir_indices = NEW_ARRAY(compiler, IRInst *, 1);
                    ir_indices[0] = irBuildConstInt(m, index_type, shuffle_indices[0]);
                    value = irBuildAccessChain(m, field_ident->type, value, ir_indices, 1);
                }
            }
            else
            {
                IRInst *vec_value = irLoadVal(m, value);

                value = irBuildVectorShuffle(
                    m, vec_value, vec_value, shuffle_indices, shuffle_index_count);
            }
        }

        expr->value = value;

        break;
    }

    case EXPR_FUNC_CALL: {
        Type *func_type = expr->func_call.func_expr->type;
        assert(func_type);

        if (func_type->kind == TYPE_TYPE)
        {
            Type *constructed_type = expr->func_call.func_expr->as_type;
            assert(constructed_type);

            uint32_t param_count = arrLength(expr->func_call.params);
            AstExpr **params = expr->func_call.params;

            if (constructed_type->kind == TYPE_VECTOR)
            {
                if (param_count == constructed_type->vector.size)
                {
                    IRInst **fields = NEW_ARRAY(compiler, IRInst *, constructed_type->vector.size);

                    for (uint32_t i = 0; i < constructed_type->vector.size; ++i)
                    {
                        irModuleBuildExpr(m, params[i]);
                        assert(params[i]->value);
                        fields[i] = irLoadVal(m, params[i]->value);
                    }

                    expr->value = irBuildCompositeConstruct(
                        m, constructed_type, fields, constructed_type->vector.size);
                }
                else if (param_count == 1)
                {
                    IRInst **fields = NEW_ARRAY(compiler, IRInst *, constructed_type->vector.size);

                    irModuleBuildExpr(m, params[0]);
                    assert(params[0]->value);
                    IRInst *field_val = irLoadVal(m, params[0]->value);

                    for (uint32_t i = 0; i < constructed_type->vector.size; ++i)
                    {
                        fields[i] = field_val;
                    }

                    expr->value = irBuildCompositeConstruct(
                        m, constructed_type, fields, constructed_type->vector.size);
                }
                else
                {
                    assert(0);
                }
            }
            else if (param_count == 1)
            {
                irModuleBuildExpr(m, params[0]);
                assert(params[0]->value);
                expr->value = irBuildCast(m, constructed_type, irLoadVal(m, params[0]->value));
            }
            else
            {
                assert(0);
            }
        }
        else
        {
            assert(func_type->kind == TYPE_FUNC);

            irModuleBuildExpr(m, expr->func_call.func_expr);
            IRInst *func_val = expr->func_call.func_expr->value;
            assert(func_val);

            uint32_t param_count = arrLength(expr->func_call.params);
            IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

            for (uint32_t i = 0; i < param_count; ++i)
            {
                AstExpr *param = expr->func_call.params[i];
                irModuleBuildExpr(m, param);
                assert(param->value);
                param_values[i] = irLoadVal(m, param->value);
            }

            expr->value = irBuildFuncCall(m, func_val, param_values, param_count);
        }

        break;
    }

    case EXPR_BUILTIN_CALL: {
        uint32_t param_count = arrLength(expr->builtin_call.params);
        IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

        for (uint32_t i = 0; i < param_count; ++i)
        {
            AstExpr *param = expr->builtin_call.params[i];
            irModuleBuildExpr(m, param);
            assert(param->value);
            param_values[i] = irLoadVal(m, param->value);
        }

        expr->value = irBuildBuiltinCall(m, expr->builtin_call.kind, param_values, param_count);

        break;
    }

    case EXPR_UNARY: {
        switch (expr->unary.op)
        {
        case UNOP_NEG: {
            irModuleBuildExpr(m, expr->unary.right);
            IRInst *right_val = irLoadVal(m, expr->unary.right->value);

            Type *scalar_type = getScalarType(expr->type);
            SpvOp op = {0};

            switch (scalar_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFNegate; break;
            case TYPE_INT: op = SpvOpSNegate; break;
            default: assert(0); break;
            }

            expr->value = irBuildUnary(m, op, expr->type, right_val);
            break;
        }

        case UNOP_NOT: {
            irModuleBuildExpr(m, expr->unary.right);
            IRInst *right_val = irLoadVal(m, expr->unary.right->value);

            expr->value = irBuildUnary(m, SpvOpNot, expr->type, right_val);
            break;
        }
        }
        break;
    }

    case EXPR_BINARY: {
        assert(0);
        break;
    }

    case EXPR_SAMPLER_TYPE:
    case EXPR_TEXTURE_TYPE:
    case EXPR_SAMPLED_TEXTURE_TYPE: {
        break;
    }
    }
}

static void irModuleBuildStmt(IRModule *m, AstStmt *stmt)
{
    switch (stmt->kind)
    {
    case STMT_DECL: {
        irModuleBuildDecl(m, stmt->decl);
        break;
    }

    case STMT_EXPR: {
        irModuleBuildExpr(m, stmt->expr);
        break;
    }

    case STMT_RETURN: {
        if (stmt->return_.value)
        {
            irModuleBuildExpr(m, stmt->return_.value);
            assert(stmt->return_.value->value);
            irBuildReturn(m, irLoadVal(m, stmt->return_.value->value));
        }
        else
        {
            irBuildReturn(m, NULL);
        }
        break;
    }

    case STMT_VAR_ASSIGN: {
        irModuleBuildExpr(m, stmt->var_assign.assigned_expr);
        IRInst *assigned_value = stmt->var_assign.assigned_expr->value;

        irModuleBuildExpr(m, stmt->var_assign.value_expr);
        IRInst *to_store = stmt->var_assign.value_expr->value;
        assert(to_store);
        to_store = irLoadVal(m, to_store);

        irBuildStore(m, assigned_value, to_store);

        break;
    }
    }
}

static void irModuleBuildDecl(IRModule *m, AstDecl *decl)
{
    TscCompiler *compiler = m->compiler;

    switch (decl->kind)
    {
    case DECL_FUNC: {
        if (decl->func.execution_model)
        {
            IRInst **globals = NULL;
            uint32_t global_count = 0;

            if (arrLength(m->globals) > 0)
            {
                global_count = arrLength(m->globals);
                globals = NEW_ARRAY(compiler, IRInst *, global_count);

                for (uint32_t i = 0; i < global_count; ++i)
                {
                    globals[i] = m->globals[i];
                }
            }

            irAddEntryPoint(
                m, decl->name, decl->value, *decl->func.execution_model, globals, global_count);
        }

        IRInst *entry_block = irAddBlock(m, decl->value);
        irPositionAtEnd(m, entry_block);

        for (uint32_t i = 0; i < arrLength(decl->func.var_decls); ++i)
        {
            AstDecl *var = decl->func.var_decls[i];
            var->value = irBuildAlloca(m, var->type);
        }

        for (uint32_t i = 0; i < arrLength(decl->func.stmts); ++i)
        {
            AstStmt *stmt = decl->func.stmts[i];
            irModuleBuildStmt(m, stmt);
        }

        if (!irBlockHasTerminator(irGetCurrentBlock(m)))
        {
            irBuildReturn(m, NULL);
        }

        break;
    }

    case DECL_VAR: {
        IRInst *initializer = NULL;

        if (decl->var.value_expr)
        {
            irModuleBuildExpr(m, decl->var.value_expr);
            initializer = decl->var.value_expr->value;
            assert(initializer);
            initializer = irLoadVal(m, initializer);
            irBuildStore(m, decl->value, initializer);
        }

        break;
    }

    case DECL_CONST: {
        irModuleBuildExpr(m, decl->constant.value_expr);
        decl->value = decl->constant.value_expr->value;
        break;
    }

    case DECL_STRUCT_FIELD: break;
    case DECL_STRUCT: break;
    }
}

static void irModuleCodegen(IRModule *m, TscCompiler *compiler, Module *mod)
{
    memset(m, 0, sizeof(*m));
    m->compiler = compiler;
    m->mod = mod;

    irModuleReserveId(m); // 0th ID

    // Add functions / globals
    for (uint32_t i = 0; i < arrLength(m->mod->files); ++i)
    {
        File *file = m->mod->files[i];

        for (uint32_t j = 0; j < arrLength(file->decls); ++j)
        {
            AstDecl *decl = file->decls[j];
            switch (decl->kind)
            {
            case DECL_FUNC: {
                assert(decl->type);
                decl->value = irAddFunction(m, decl->type);

                for (uint32_t k = 0; k < arrLength(decl->func.func_params); ++k)
                {
                    AstDecl *param = decl->func.func_params[k];
                    param->value = irAddFuncParam(m, decl->value, param->type);
                }

                for (uint32_t k = 0; k < arrLength(decl->func.inputs); ++k)
                {
                    AstDecl *input = decl->func.inputs[k];
                    input->value = irAddGlobal(m, input->type, input->var.storage_class);
                    input->value->decorations = input->decorations;
                }

                for (uint32_t k = 0; k < arrLength(decl->func.outputs); ++k)
                {
                    AstDecl *output = decl->func.outputs[k];
                    output->value = irAddGlobal(m, output->type, output->var.storage_class);
                    output->value->decorations = output->decorations;
                }

                break;
            }

            case DECL_VAR: {
                decl->value = irAddGlobal(m, decl->type, decl->var.storage_class);
                decl->value->decorations = decl->decorations;
                break;
            }

            default: break;
            }
        }
    }

    for (uint32_t i = 0; i < arrLength(m->mod->files); ++i)
    {
        File *file = m->mod->files[i];

        for (uint32_t j = 0; j < arrLength(file->decls); ++j)
        {
            AstDecl *decl = file->decls[j];
            irModuleBuildDecl(m, decl);
        }
    }

    irModuleEncodeModule(m);
}
// }}}

static bool handleErrors(TscCompiler *compiler, TscCompilerOutput *output)
{
    if (arrLength(compiler->errors) > 0)
    {
        output->error_count = arrLength(compiler->errors);
        output->errors = malloc(sizeof(char *) * output->error_count);
        for (uint32_t i = 0; i < output->error_count; ++i)
        {
            Error *err = &compiler->errors[i];
            sbReset(&compiler->sb);
            if (err->loc.file && err->loc.file->path)
            {
                sbAppend(&compiler->sb, err->loc.file->path);
                sbAppend(&compiler->sb, ":");
            }
            sbSprintf(&compiler->sb, "%u:%u: error: %s", err->loc.line, err->loc.col, err->message);
            output->errors[i] = sbBuildMalloc(&compiler->sb);
        }

        return true;
    }

    return false;
}

void tscCompile(TscCompiler *compiler, TscCompilerInput *input, TscCompilerOutput *output)
{
    assert(compiler);
    assert(input);
    assert(output);

    {
        File *file = NEW(compiler, File);
        file->path = input->path;
        file->text = input->input;
        file->text_size = input->input_size;

        arrPush(compiler->file_queue, file);
    }

    Module *module = NEW(compiler, Module);
    moduleInit(module, compiler);

    while (arrLength(compiler->file_queue) > 0)
    {
        File *file = compiler->file_queue[arrLength(compiler->file_queue) - 1];
        arrPop(compiler->file_queue);

        Lexer lexer = {0};
        lexerLex(&lexer, compiler, file);
        if (handleErrors(compiler, output)) return;

        Parser parser = {0};
        parserParse(&parser, compiler, file);
        if (handleErrors(compiler, output)) return;

        arrPush(module->files, file);
    }

    Analyzer analyzer = {0};
    analyzerAnalyze(&analyzer, compiler, module);
    if (handleErrors(compiler, output)) return;

    IRModule irModule = {0};
    irModuleCodegen(&irModule, compiler, module);

    output->spirv_byte_size = arrLength(irModule.stream) * 4;
    output->spirv = malloc(output->spirv_byte_size);
    memcpy(output->spirv, irModule.stream, output->spirv_byte_size);

    arrFree(irModule.stream);

    moduleDestroy(module);
}

void tscCompilerOutputDestroy(TscCompilerOutput *output)
{
    if (output->spirv) free(output->spirv);
    if (output->errors)
    {
        for (uint32_t i = 0; i < output->error_count; ++i)
        {
            free(output->errors[i]);
        }
        free(output->errors);
    }
}
