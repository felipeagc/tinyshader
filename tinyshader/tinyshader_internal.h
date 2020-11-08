/**
 * This file is part of the tinyshader library.
 * See tinyshader.h for license details.
 */
#ifndef TINYSHADER_INTERNAL_H
#define TINYSHADER_INTERNAL_H

#ifdef _MSC_VER
#pragma warning(disable : 4996)
#endif

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include "spirv.h"
#include "GLSL.std.450.h"
#include "tinyshader.h"

#define TS__MAX(a, b) ((a) > (b) ? (a) : (b))
#define TS__MIN(a, b) ((a) < (b) ? (a) : (b))

void *ts__arrayGrow(
    TsCompiler *compiler, void *ptr, size_t *cap, size_t wanted_cap, size_t item_size);

#define ARRAY_OF(type)                                                                   \
    struct                                                                               \
    {                                                                                    \
        type *ptr;                                                                       \
        size_t len;                                                                      \
        size_t cap;                                                                      \
    }

#define arrFull(a) ((a)->ptr ? ((a)->len >= (a)->cap) : 1)

#define arrLast(a) (&(a).ptr[(a).len - 1])

#define arrLength(a) ((a).len)

#define arrPush(compiler, a, item)                                                       \
    (arrFull(a) ? (a)->ptr = ts__arrayGrow(                                              \
                      (compiler), (a)->ptr, &(a)->cap, 0, sizeof(*((a)->ptr)))           \
                : 0,                                                                     \
     (a)->ptr[(a)->len++] = (item))

#define arrPop(a) ((a)->len > 0 ? ((a)->len--, &(a)->ptr[(a)->len]) : NULL)

#define arrFree(compiler, a)                                                             \
    do                                                                                   \
    {                                                                                    \
        (a)->ptr = NULL;                                                                 \
        (a)->len = 0;                                                                    \
        (a)->cap = 0;                                                                    \
    } while (0)

// Rounds to the next multiple of four
#define ROUND_TO_4(x) (((x) + 3) & ~0x03)

#define NEW(compiler, type) ts__bumpZeroAlloc(&(compiler)->alloc, sizeof(type))
#define NEW_ARRAY(compiler, type, count)                                                 \
    ts__bumpZeroAlloc(&(compiler)->alloc, sizeof(type) * ((count) > 0 ? (count) : 1))
#define NEW_ARRAY_UNINIT(compiler, type, count)                                          \
    ts__bumpAlloc(&(compiler)->alloc, sizeof(type) * (count))

////////////////////////////////
//
// Type definitons
//
////////////////////////////////

typedef struct HashMap
{
    TsCompiler *compiler;
    char **keys;
    uint64_t *hashes;
    uint64_t *indices;
    uint64_t size;

    ARRAY_OF(void *) values;
} HashMap;

typedef struct BumpBlock
{
    unsigned char *data;
    size_t size;
    size_t pos;
    struct BumpBlock *next;
} BumpBlock;

typedef struct BumpAlloc
{
    size_t block_size;
    size_t last_block_size;
    BumpBlock base_block;
    BumpBlock *last_block;
} BumpAlloc;

typedef struct StringBuilder
{
    char *buf;
    char *scratch;
    size_t len;
    size_t cap;
} StringBuilder;

typedef struct File File;
typedef struct Module Module;

typedef struct Scope Scope;
typedef ARRAY_OF(Scope *) ArrayOfScopePtr;

typedef struct AstExpr AstExpr;
typedef struct AstStmt AstStmt;
typedef struct AstDecl AstDecl;

typedef ARRAY_OF(AstExpr *) ArrayOfAstExprPtr;
typedef ARRAY_OF(AstStmt *) ArrayOfAstStmtPtr;
typedef ARRAY_OF(AstDecl *) ArrayOfAstDeclPtr;

typedef struct Location
{
    char *path;
    const char *buffer; // This is the entire source buffer, starting from the beginning
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
typedef ARRAY_OF(Error) ArrayOfError;

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

    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_COLON_COLON,

    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_MOD,

    TOKEN_ADDADD, // ++
    TOKEN_SUBSUB, // --

    TOKEN_BITOR,  // |
    TOKEN_BITXOR, // ^
    TOKEN_BITAND, // &
    TOKEN_BITNOT, // ~

    TOKEN_LSHIFT, // <<
    TOKEN_RSHIFT, // >>

    TOKEN_PERIOD,   // .
    TOKEN_COMMA,    // ,
    TOKEN_QUESTION, // ?

    TOKEN_NOT,    // !
    TOKEN_ASSIGN, // =

    TOKEN_EQUAL,     // ==
    TOKEN_NOTEQ,     // !=
    TOKEN_LESS,      // <
    TOKEN_LESSEQ,    // <=
    TOKEN_GREATER,   // >
    TOKEN_GREATEREQ, // >=

    TOKEN_ADD_ASSIGN, // +=
    TOKEN_SUB_ASSIGN, // -=
    TOKEN_MUL_ASSIGN, // *=
    TOKEN_DIV_ASSIGN, // /=
    TOKEN_MOD_ASSIGN, // %=

    TOKEN_BITAND_ASSIGN, // &=
    TOKEN_BITOR_ASSIGN,  // |=
    TOKEN_BITXOR_ASSIGN, // ^=

    TOKEN_LSHIFT_ASSIGN, // <<=
    TOKEN_RSHIFT_ASSIGN, // >>=

    TOKEN_AND, // &&
    TOKEN_OR,  // ||

    TOKEN_IDENT,
    TOKEN_IN,
    TOKEN_OUT,
    TOKEN_INOUT,
    TOKEN_STRUCT,
    TOKEN_FOR,
    TOKEN_WHILE,
    TOKEN_DO,
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
    TOKEN_STRUCTURED_BUFFER,
    TOKEN_RW_STRUCTURED_BUFFER,
    TOKEN_SAMPLER_STATE,
    TOKEN_TEXTURE_1D,
    TOKEN_TEXTURE_2D,
    TOKEN_TEXTURE_3D,
    TOKEN_TEXTURE_CUBE,

    TOKEN_DISCARD,

    TOKEN_INT_LIT,
    TOKEN_FLOAT_LIT,
    TOKEN_STRING_LIT,

    TOKEN_BOOL,

    TOKEN_UINT,
    TOKEN_INT,
    TOKEN_FLOAT,

    TOKEN_VOID,

    TOKEN_FALSE,
    TOKEN_TRUE,

    TOKEN_VECTOR_TYPE,
    TOKEN_MATRIX_TYPE,

    TOKEN_STATIC,
    TOKEN_GROUPSHARED,
    TOKEN_REGISTER,

    TOKEN_MAX,
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
        struct
        {
            TokenKind elem_type;
            uint8_t dim;
        } vector_type;
        struct
        {
            TokenKind elem_type;
            uint8_t dim1;
            uint8_t dim2;
        } matrix_type;
    };
} Token;
typedef ARRAY_OF(Token) ArrayOfToken;

//
// IR
//

typedef struct IRDecoration
{
    SpvDecoration kind;
    union
    {
        uint32_t value;
    };
} IRDecoration;
typedef ARRAY_OF(IRDecoration) ArrayOfIRDecoration;

typedef struct IRMemberDecoration
{
    SpvDecoration kind;
    uint32_t member_index;
    union
    {
        uint32_t value;
    };
} IRMemberDecoration;
typedef ARRAY_OF(IRMemberDecoration) ArrayOfIRMemberDecoration;

typedef enum IRTypeKind {
    IR_TYPE_VOID,

    IR_TYPE_BOOL,
    IR_TYPE_FLOAT,
    IR_TYPE_INT,

    IR_TYPE_VECTOR,
    IR_TYPE_MATRIX,

    IR_TYPE_RUNTIME_ARRAY,

    IR_TYPE_POINTER,
    IR_TYPE_FUNC,
    IR_TYPE_STRUCT,

    IR_TYPE_SAMPLER,
    IR_TYPE_IMAGE,
    IR_TYPE_SAMPLED_IMAGE,
} IRTypeKind;

typedef struct IRType
{
    IRTypeKind kind;
    char *string;
    uint32_t id;

    IRDecoration *decorations;
    uint32_t decoration_count;

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
            struct IRType *sub;
        } ptr;
        struct
        {
            uint32_t size;
            struct IRType *elem_type;
        } vector;
        struct
        {
            uint32_t col_count;
            struct IRType *col_type;
        } matrix;
        struct
        {
            struct IRType *return_type;

            struct IRType **params;
            uint32_t param_count;
        } func;
        struct
        {
            char *name;

            struct IRType **fields;
            uint32_t field_count;
            IRMemberDecoration *field_decorations;
            uint32_t field_decoration_count;
        } struct_;
        struct
        {
            struct IRType *sampled_type;
            SpvDim dim;
            uint32_t depth;
            uint32_t arrayed;
            uint32_t multisampled;
            uint32_t sampled;
            SpvImageFormat format;
        } image;
        struct
        {
            struct IRType *image_type;
        } sampled_image;
        struct
        {
            size_t size;
            struct IRType *sub;
        } array;
    };
} IRType;

typedef struct IRModule IRModule;
typedef struct IRInst IRInst;
typedef ARRAY_OF(IRInst *) ArrayOfIRInstPtr;

typedef enum IRInstKind {
    IR_INST_ENTRY_POINT,
    IR_INST_FUNCTION,
    IR_INST_BLOCK,
    IR_INST_FUNC_PARAM,
    IR_INST_VARIABLE,
    IR_INST_CONSTANT,
    IR_INST_CONSTANT_COMPOSITE,
    IR_INST_CONSTANT_BOOL,
    IR_INST_RETURN,
    IR_INST_DISCARD,
    IR_INST_STORE,
    IR_INST_LOAD,
    IR_INST_ACCESS_CHAIN,
    IR_INST_FUNC_CALL,
    IR_INST_BRANCH,
    IR_INST_COND_BRANCH,

    IR_INST_BUILTIN_CALL,
    IR_INST_BARRIER,
    IR_INST_CAST,
    IR_INST_COMPOSITE_CONSTRUCT,
    IR_INST_COMPOSITE_EXTRACT,
    IR_INST_VECTOR_SHUFFLE,

    IR_INST_SAMPLE_IMPLICIT_LOD,
    IR_INST_SAMPLE_EXPLICIT_LOD,
    IR_INST_QUERY_SIZE_LOD,
    IR_INST_QUERY_LEVELS,

    IR_INST_UNARY,
    IR_INST_BINARY,
    IR_INST_SELECT,
} IRInstKind;

typedef enum IRBuiltinInstKind {
    IR_BUILTIN_DOT,
    IR_BUILTIN_CROSS,
    IR_BUILTIN_LENGTH,
    IR_BUILTIN_NORMALIZE,
    IR_BUILTIN_MUL,
    IR_BUILTIN_DISTANCE,
    IR_BUILTIN_DEGREES,
    IR_BUILTIN_RADIANS,

    IR_BUILTIN_SIN,
    IR_BUILTIN_COS,
    IR_BUILTIN_TAN,
    IR_BUILTIN_ASIN,
    IR_BUILTIN_ACOS,
    IR_BUILTIN_ATAN,
    IR_BUILTIN_SINH,
    IR_BUILTIN_COSH,
    IR_BUILTIN_TANH,
    IR_BUILTIN_ATAN2,

    IR_BUILTIN_SQRT,
    IR_BUILTIN_RSQRT,

    IR_BUILTIN_REFLECT,
    IR_BUILTIN_REFRACT,

    IR_BUILTIN_POW,
    IR_BUILTIN_EXP,
    IR_BUILTIN_EXP2,
    IR_BUILTIN_LOG,
    IR_BUILTIN_LOG2,

    IR_BUILTIN_ABS,
    IR_BUILTIN_MIN,
    IR_BUILTIN_MAX,
    IR_BUILTIN_FRAC,
    IR_BUILTIN_TRUNC,
    IR_BUILTIN_CEIL,
    IR_BUILTIN_FLOOR,
    IR_BUILTIN_LERP,
    IR_BUILTIN_CLAMP,
    IR_BUILTIN_STEP,
    IR_BUILTIN_SMOOTHSTEP,
    IR_BUILTIN_FMOD,

    IR_BUILTIN_DDX,
    IR_BUILTIN_DDY,

    IR_BUILTIN_ASUINT,
    IR_BUILTIN_ASINT,
    IR_BUILTIN_ASFLOAT,

    IR_BUILTIN_INTERLOCKED_ADD,
    IR_BUILTIN_INTERLOCKED_AND,
    IR_BUILTIN_INTERLOCKED_MIN,
    IR_BUILTIN_INTERLOCKED_MAX,
    IR_BUILTIN_INTERLOCKED_OR,
    IR_BUILTIN_INTERLOCKED_XOR,
    IR_BUILTIN_INTERLOCKED_EXCHANGE,
    IR_BUILTIN_INTERLOCKED_COMPARE_EXCHANGE,
    IR_BUILTIN_INTERLOCKED_COMPARE_STORE,

    IR_BUILTIN_TRANSPOSE,
    IR_BUILTIN_DETERMINANT,

    IR_BUILTIN_CREATE_SAMPLED_IMAGE,
} IRBuiltinInstKind;

struct IRInst
{
    IRInstKind kind;
    uint32_t id;
    IRType *type;
    ArrayOfIRDecoration decorations;

    union
    {
        struct
        {
            IRInst *func;
            char *name;
            SpvExecutionModel execution_model;
            IRInst **globals;
            uint32_t global_count;

            struct
            {
                uint32_t x, y, z;
            } compute_dims;
        } entry_point;

        struct
        {
            ArrayOfIRInstPtr params;
            ArrayOfIRInstPtr blocks;
            ArrayOfIRInstPtr inputs;
            ArrayOfIRInstPtr outputs;
        } func;

        struct
        {
            IRInst *func;
            ArrayOfIRInstPtr insts;
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
            IRInst **values;
            uint32_t value_count;
        } constant_composite;

        struct
        {
            bool value;
        } constant_bool;

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
            bool is_by_reference;
        } func_param;

        struct
        {
            IRBuiltinInstKind kind;
            IRInst **params;
            uint32_t param_count;
        } builtin_call;

        struct
        {
            SpvOp op;
            IRType *dst_type;
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
            IRInst *image_sampler;
            IRInst *coords;
            IRInst *lod;
        } sample;

        struct
        {
            IRInst *image;
            IRInst *lod;
        } query_size_lod;

        struct
        {
            IRInst *image;
        } query_levels;

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

        struct
        {
            IRInst *target;
            IRInst *merge_block;
            IRInst *continue_block;
        } branch;

        struct
        {
            IRInst *cond;
            IRInst *true_block;
            IRInst *false_block;
            IRInst *merge_block;
            IRInst *continue_block;
        } cond_branch;

        struct
        {
            bool with_group_sync;
            IRInst *memory_scope;
            IRInst *execution_scope;
            IRInst *semantics;
        } barrier;

        struct
        {
            IRInst *cond;
            IRInst *true_value;
            IRInst *false_value;
        } select;
    };
};

struct IRModule
{
    TsCompiler *compiler;

    HashMap type_cache;
    HashMap const_cache;

    ArrayOfIRInstPtr entry_points;
    ArrayOfIRInstPtr constants;
    ArrayOfIRInstPtr functions;
    ArrayOfIRInstPtr globals;     /* This only counts uniforms/storage variables */
    ArrayOfIRInstPtr all_globals; /* This counts uniforms/storage variables and all
                                       inputs/outputs of every stage */

    uint32_t glsl_ext_inst; // ID of the imported GLSL instruction set
    bool uses_image_query;

    IRInst *current_block;

    uint32_t id_bound;
    ARRAY_OF(uint32_t) stream;
};

//
// AST
//

typedef enum AstBuiltinFunction {
    AST_BUILTIN_FUNC_DOT,
    AST_BUILTIN_FUNC_CROSS,
    AST_BUILTIN_FUNC_LENGTH,
    AST_BUILTIN_FUNC_NORMALIZE,
    AST_BUILTIN_FUNC_MUL,
    AST_BUILTIN_FUNC_DISTANCE,
    AST_BUILTIN_FUNC_DEGREES,
    AST_BUILTIN_FUNC_RADIANS,

    AST_BUILTIN_FUNC_SIN,
    AST_BUILTIN_FUNC_COS,
    AST_BUILTIN_FUNC_TAN,
    AST_BUILTIN_FUNC_ASIN,
    AST_BUILTIN_FUNC_ACOS,
    AST_BUILTIN_FUNC_ATAN,
    AST_BUILTIN_FUNC_SINH,
    AST_BUILTIN_FUNC_COSH,
    AST_BUILTIN_FUNC_TANH,
    AST_BUILTIN_FUNC_ATAN2,

    AST_BUILTIN_FUNC_SQRT,
    AST_BUILTIN_FUNC_RSQRT,

    AST_BUILTIN_FUNC_REFLECT,
    AST_BUILTIN_FUNC_REFRACT,

    AST_BUILTIN_FUNC_POW,
    AST_BUILTIN_FUNC_EXP,
    AST_BUILTIN_FUNC_EXP2,
    AST_BUILTIN_FUNC_LOG,
    AST_BUILTIN_FUNC_LOG2,

    AST_BUILTIN_FUNC_ABS,
    AST_BUILTIN_FUNC_MIN,
    AST_BUILTIN_FUNC_MAX,
    AST_BUILTIN_FUNC_FRAC,
    AST_BUILTIN_FUNC_TRUNC,
    AST_BUILTIN_FUNC_CEIL,
    AST_BUILTIN_FUNC_FLOOR,
    AST_BUILTIN_FUNC_LERP,
    AST_BUILTIN_FUNC_CLAMP,
    AST_BUILTIN_FUNC_STEP,
    AST_BUILTIN_FUNC_SMOOTHSTEP,
    AST_BUILTIN_FUNC_FMOD,

    AST_BUILTIN_FUNC_DDX,
    AST_BUILTIN_FUNC_DDY,

    AST_BUILTIN_FUNC_ASUINT,
    AST_BUILTIN_FUNC_ASINT,
    AST_BUILTIN_FUNC_ASFLOAT,

    AST_BUILTIN_FUNC_INTERLOCKED_ADD,
    AST_BUILTIN_FUNC_INTERLOCKED_AND,
    AST_BUILTIN_FUNC_INTERLOCKED_MIN,
    AST_BUILTIN_FUNC_INTERLOCKED_MAX,
    AST_BUILTIN_FUNC_INTERLOCKED_OR,
    AST_BUILTIN_FUNC_INTERLOCKED_XOR,
    AST_BUILTIN_FUNC_INTERLOCKED_EXCHANGE,
    AST_BUILTIN_FUNC_INTERLOCKED_COMPARE_EXCHANGE,
    AST_BUILTIN_FUNC_INTERLOCKED_COMPARE_STORE,

    AST_BUILTIN_FUNC_TRANSPOSE,
    AST_BUILTIN_FUNC_DETERMINANT,

    AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER,
    AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER_WITH_GROUP_SYNC,
    AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER,
    AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER_WITH_GROUP_SYNC,
    AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER,
    AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER_WITH_GROUP_SYNC,
} AstBuiltinFunction;

typedef enum AstTypeKind {
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

    TYPE_CONSTANT_BUFFER,
    TYPE_STRUCTURED_BUFFER,
    TYPE_RW_STRUCTURED_BUFFER,
} AstTypeKind;

typedef struct AstType
{
    AstTypeKind kind;
    char *string;
    uint32_t size;
    uint32_t align;

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
            struct AstType *sub;
        } ptr;
        struct
        {
            uint32_t size;
            struct AstType *elem_type;
        } vector;
        struct
        {
            uint32_t col_count;
            struct AstType *col_type;
        } matrix;
        struct
        {
            struct AstType *return_type;

            struct AstType **params;
            uint32_t param_count;
        } func;
        struct
        {
            char *name;

            AstDecl **field_decls;
            struct AstType **fields;
            ArrayOfIRMemberDecoration field_decorations;
            uint32_t field_count;
        } struct_;
        struct
        {
            struct AstType *sampled_type;
            SpvDim dim;
            uint32_t depth;
            uint32_t arrayed;
            uint32_t multisampled;
            uint32_t sampled;
            SpvImageFormat format;
        } image;
        struct
        {
            struct AstType *image_type;
        } sampled_image;
        struct
        {
            struct AstType *sub;
        } buffer;
    };
} AstType;

typedef struct AstAttribute
{
    char *name;
    ArrayOfAstExprPtr values;
} AstAttribute;
typedef ARRAY_OF(AstAttribute) ArrayOfAstAttribute;

typedef enum AstUnaryOp {
    UNOP_NEG,
    UNOP_NOT,
    UNOP_PRE_INC,
    UNOP_PRE_DEC,
    UNOP_POST_INC,
    UNOP_POST_DEC,
    UNOP_BITNOT,
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

    BINOP_LSHIFT,
    BINOP_RSHIFT,

    BINOP_BITOR,
    BINOP_BITAND,
    BINOP_BITXOR,

    BINOP_LOGICAL_AND,
    BINOP_LOGICAL_OR,
} AstBinaryOp;

typedef enum AstVarKind {
    VAR_PLAIN = 0,
    VAR_UNIFORM,
    VAR_GROUPSHARED,
    VAR_PARAM,
    VAR_IN_PARAM,
    VAR_OUT_PARAM,
    VAR_INOUT_PARAM,
} AstVarKind;

typedef enum AstStmtKind {
    STMT_DECL,
    STMT_EXPR,
    STMT_RETURN,
    STMT_DISCARD,
    STMT_CONTINUE,
    STMT_BREAK,
    STMT_BLOCK,
    STMT_IF,
    STMT_WHILE,
    STMT_DO_WHILE,
    STMT_FOR,
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
    EXPR_SUBSCRIPT,
    EXPR_SAMPLER_TYPE,
    EXPR_TEXTURE_TYPE,
    EXPR_CONSTANT_BUFFER_TYPE,
    EXPR_STRUCTURED_BUFFER_TYPE,
    EXPR_RW_STRUCTURED_BUFFER_TYPE,
    EXPR_FUNC_CALL,
    EXPR_VAR_ASSIGN,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_TERNARY,
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
            AstExpr *value;
        } return_;

        struct
        {
            ArrayOfAstStmtPtr stmts;
            Scope *scope;
        } block;

        struct
        {
            AstExpr *cond;
            AstStmt *if_stmt;
            AstStmt *else_stmt;
        } if_;

        struct
        {
            AstExpr *cond;
            AstStmt *stmt;
        } while_;

        struct
        {
            AstExpr *cond;
            AstStmt *stmt;
        } do_while;

        struct
        {
            AstStmt *init;
            AstExpr *cond;
            AstExpr *inc;

            AstStmt *stmt;
        } for_;
    };
};

struct AstDecl
{
    AstDeclKind kind;
    Location loc;
    char *name;
    AstType *type;
    AstType *as_type;
    IRInst *value;
    Scope *scope;
    ArrayOfAstAttribute attributes;
    ArrayOfIRDecoration decorations;
    int64_t *resolved_int;

    union
    {
        struct
        {
            SpvExecutionModel *execution_model;

            ArrayOfAstStmtPtr stmts;
            ArrayOfAstDeclPtr all_params;
            AstExpr *return_type;

            // To be filled later:
            ArrayOfAstDeclPtr var_decls;
            ArrayOfAstDeclPtr func_params;
            ArrayOfAstDeclPtr inputs;
            ArrayOfAstDeclPtr outputs;

            bool called; // if this function was called or if it's an entry point

            uint32_t compute_dims[3];
        } func;

        struct
        {
            AstExpr *type_expr;
            AstExpr *value_expr;
            char *semantic;
            AstVarKind kind;
            bool immutable;
        } var;

        struct
        {
            AstExpr *type_expr;
            AstExpr *value_expr;
        } constant;

        struct
        {
            ArrayOfAstDeclPtr fields;
        } struct_;

        struct
        {
            AstExpr *type_expr;
            uint32_t index;
            char *semantic;
        } struct_field;

        struct
        {
            uint32_t set_index;
            ArrayOfAstDeclPtr params;
        } parameter_block;
    };
};

struct AstExpr
{
    AstExprKind kind;
    Location loc;
    IRInst *value;
    AstType *type;
    AstType *as_type;
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
            ArrayOfAstExprPtr chain;
        } access;

        struct
        {
            AstExpr *left;
            AstExpr *right;
        } subscript;

        struct
        {
            AstExpr *func_expr;
            ArrayOfAstExprPtr params;
            AstExpr *self_param;
        } func_call;

        struct
        {
            AstExpr *sampled_type_expr;
            SpvDim dim;
        } texture;

        struct
        {
            AstExpr *sub_expr;
        } buffer;

        struct
        {
            AstExpr *assigned_expr;
            AstExpr *value_expr;
        } var_assign;

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

        struct
        {
            AstExpr *cond;
            AstExpr *true_expr;
            AstExpr *false_expr;
        } ternary;
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

typedef struct TsCompiler
{
    BumpAlloc alloc;
    StringBuilder sb;

    HashMap keyword_table;
    HashMap builtin_function_table;
    HashMap files; // Maps absolute paths to files

    ArrayOfError errors;

    uint32_t counter; // General purpose unique number generator
} TsCompiler;

//
// File
//

struct File
{
    const char *text;
    size_t text_size;

    char *path;
    char *dir;
};

struct Module
{
    TsCompiler *compiler;
    Scope *scope;

    const char *entry_point; // Requested entry point name
    TsShaderStage stage;

    HashMap type_cache;

    ArrayOfIRInstPtr continue_stack;
    ArrayOfIRInstPtr break_stack;

    AstDecl **decls;
    size_t decl_count;
};

//
// Preprocessor
//

typedef struct Preprocessor
{
    TsCompiler *compiler;
    StringBuilder sb;
    HashMap defines;
} Preprocessor;

//
// Lexer
//

typedef struct Lexer
{
    TsCompiler *compiler;

    char *text;
    size_t text_size;
    Token token;

    ArrayOfToken tokens;

    char *file_path;
    size_t pos;
    uint32_t line;
    uint32_t col;
} Lexer;

//
// Parser
//

typedef struct Parser
{
    TsCompiler *compiler;
    Token *tokens;
    size_t token_count;

    ArrayOfAstDeclPtr decls;

    size_t pos;
} Parser;

//
// Analyzer
//

typedef struct Analyzer
{
    TsCompiler *compiler;
    Module *module;

    AstDecl *scope_func;
    ArrayOfScopePtr scope_stack;

    ArrayOfAstStmtPtr continue_stack;
    ArrayOfAstStmtPtr break_stack;

    uint32_t last_uniform_binding;
} Analyzer;

////////////////////////////////
//
// Functions
//
////////////////////////////////

char *ts__getAbsolutePath(TsCompiler *compiler, const char *relative_path);
char *ts__getPathDir(TsCompiler *compiler, const char *path);
char *ts__getCurrentDir(TsCompiler *compiler);
char *ts__pathConcat(TsCompiler *compiler, const char *a, const char *b);
bool ts__fileExists(TsCompiler *compiler, const char *path);

void ts__hashInit(TsCompiler *compiler, HashMap *map, uint64_t size);
void *ts__hashSet(HashMap *map, const char *key, void *value);
bool ts__hashGet(HashMap *map, const char *key, void **result);
void ts__hashRemove(HashMap *map, const char *key);
void ts__hashDestroy(HashMap *map);

void ts__bumpInit(BumpAlloc *alloc, size_t block_size);
void *ts__bumpAlloc(BumpAlloc *alloc, size_t size);
void *ts__bumpZeroAlloc(BumpAlloc *alloc, size_t size);
char *ts__bumpStrndup(BumpAlloc *alloc, const char *str, size_t length);
void ts__bumpDestroy(BumpAlloc *alloc);

void ts__sbInit(StringBuilder *sb);
void ts__sbDestroy(StringBuilder *sb);
void ts__sbReset(StringBuilder *sb);
void ts__sbAppend(StringBuilder *sb, const char *str);
void ts__sbAppendChar(StringBuilder *sb, char c);
void ts__sbSprintf(StringBuilder *sb, const char *fmt, ...);
void ts__sbVsprintf(StringBuilder *sb, const char *fmt, va_list vl);
char *ts__sbBuildMalloc(StringBuilder *sb);
char *ts__sbBuild(StringBuilder *sb, BumpAlloc *bump);

void ts__addErr(TsCompiler *compiler, Location *loc, const char *msg, ...);

File *ts__createFile(
    TsCompiler *compiler, const char *text, size_t text_size, const char *path);

char *ts__preprocessRootFile(
    Preprocessor *p, TsCompiler *compiler, File *file, size_t *out_length);
void ts__lexerLex(Lexer *l, TsCompiler *compiler, char *text, size_t text_size);
void ts__parserParse(Parser *p, TsCompiler *compiler, Token *tokens, size_t token_count);
void ts__analyzerAnalyze(
    TsCompiler *compiler,
    Module *module,
    AstDecl **decls,
    size_t decl_count);

AstType *ts__getScalarType(AstType *type);
AstType *ts__getComparableType(AstType *type);
AstType *ts__getLogicalType(AstType *type);
uint32_t ts__getTypeElemCount(AstType *type);
AstType *ts__getElemType(AstType *type);
AstType *ts__getStructType(AstType *type);

void ts__astModuleBuild(Module *ast_mod, IRModule *ir_mod);

IRModule *ts__irModuleCreate(TsCompiler *compiler);
void ts__irModuleDestroy(IRModule *m);

void ts__irTypeSetDecorations(
    IRModule *m, IRType *type, IRDecoration *decorations, uint32_t decoration_count);

IRType *ts__irNewBasicType(IRModule *m, IRTypeKind kind);
IRType *ts__irNewPointerType(IRModule *m, SpvStorageClass storage_class, IRType *sub);
IRType *ts__irNewVectorType(IRModule *m, IRType *elem_type, uint32_t size);
IRType *ts__irNewMatrixType(IRModule *m, IRType *col_type, uint32_t col_count);
IRType *ts__irNewFloatType(IRModule *m, uint32_t bits);
IRType *ts__irNewIntType(IRModule *m, uint32_t bits, bool is_signed);
IRType *ts__irNewRuntimeArrayType(IRModule *m, IRType *sub);
IRType *
ts__irNewFuncType(IRModule *m, IRType *return_type, IRType **params, uint32_t param_count);
IRType *ts__irNewStructType(
    IRModule *m,
    char *name,
    IRType **fields,
    uint32_t field_count,
    IRMemberDecoration *field_decorations,
    uint32_t field_decoration_count);
IRType *ts__irNewImageType(IRModule *m, IRType *sampled_type, SpvDim dim);
IRType *ts__irNewSampledImageType(IRModule *m, IRType *image_type);

IRInst *ts__irAddEntryPoint(
    IRModule *m,
    char *name,
    IRInst *func,
    SpvExecutionModel execution_model,
    IRInst **globals,
    uint32_t global_count);
void
ts__irEntryPointSetComputeDims(IRInst *entry_point, uint32_t x, uint32_t y, uint32_t z);
IRInst *ts__irAddFunction(IRModule *m, IRType *func_type);
IRInst *
ts__irAddFuncParam(IRModule *m, IRInst *func, IRType *type, bool is_by_reference);
IRInst *ts__irCreateBlock(IRModule *m, IRInst *func);
void ts__irAddBlock(IRModule *m, IRInst *block);
IRInst *ts__irAddGlobal(IRModule *m, IRType *type, SpvStorageClass storage_class);
IRInst *ts__irAddInput(IRModule *m, IRInst *func, IRType *type);
IRInst *ts__irAddOutput(IRModule *m, IRInst *func, IRType *type);
IRInst *ts__irGetCurrentBlock(IRModule *m);
void ts__irPositionAtEnd(IRModule *m, IRInst *block);
bool ts__irBlockHasTerminator(IRInst *block);

IRInst *ts__irBuildConstFloat(IRModule *m, IRType *type, double value);
IRInst *ts__irBuildConstInt(IRModule *m, IRType *type, uint64_t value);
IRInst *ts__irBuildConstComposite(IRModule *m, IRType *type, IRInst **values, uint32_t value_count);
IRInst *ts__irBuildConstBool(IRModule *m, bool value);
IRInst *ts__irBuildAlloca(IRModule *m, IRType *type);
void ts__irBuildStore(IRModule *m, IRInst *pointer, IRInst *value);
IRInst *ts__irBuildLoad(IRModule *m, IRInst *pointer);
IRInst *ts__irBuildAccessChain(
    IRModule *m, IRType *type, IRInst *base, IRInst **indices, uint32_t index_count);
IRInst *ts__irBuildVectorShuffle(
    IRModule *m,
    IRInst *vector_a,
    IRInst *vector_b,
    uint32_t *indices,
    uint32_t index_count);
IRInst *ts__irBuildCompositeExtract(
    IRModule *m, IRInst *value, uint32_t *indices, uint32_t index_count);
IRInst *ts__irBuildCompositeConstruct(
    IRModule *m, IRType *type, IRInst **fields, uint32_t field_count);
IRInst *
ts__irBuildFuncCall(IRModule *m, IRInst *function, IRInst **params, uint32_t param_count);
IRInst *ts__irBuildBuiltinCall(
    IRModule *m,
    IRBuiltinInstKind kind,
    IRType *result_type,
    IRInst **params,
    uint32_t param_count);
IRInst *ts__irBuildBarrier(
    IRModule *m,
    bool with_group_sync,
    uint32_t execution_scope,
    uint32_t memory_scope,
    uint32_t semantics);
IRInst *ts__irBuildSampleImplicitLod(
    IRModule *m, IRType *type, IRInst *image_sampler, IRInst *coords);
IRInst *ts__irBuildSampleExplicitLod(
    IRModule *m, IRType *type, IRInst *image_sampler, IRInst *coords, IRInst *lod);
IRInst *ts__irBuildQuerySizeLod(IRModule *m, IRInst *image, IRInst *lod);
IRInst *ts__irBuildQueryLevels(IRModule *m, IRInst *image);
IRInst *ts__irBuildCast(IRModule *m, IRType *dst_type, IRInst *value);
IRInst *ts__irBuildUnary(IRModule *m, SpvOp op, IRType *type, IRInst *right);
IRInst *
ts__irBuildBinary(IRModule *m, SpvOp op, IRType *type, IRInst *left, IRInst *right);
IRInst *ts__irBuildSelect(
    IRModule *m, IRType *type, IRInst *cond, IRInst *true_value, IRInst *false_value);
void ts__irBuildReturn(IRModule *m, IRInst *value);
void ts__irBuildDiscard(IRModule *m);
void ts__irBuildBr(
    IRModule *m, IRInst *target, IRInst *merge_block, IRInst *continue_block);
void ts__irBuildCondBr(
    IRModule *m,
    IRInst *cond,
    IRInst *true_block,
    IRInst *false_block,
    IRInst *merge_block,
    IRInst *continue_block);

uint32_t *ts__irModuleCodegen(IRModule *mod, size_t *word_count);

#endif
