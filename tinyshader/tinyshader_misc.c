/**
 * This file is part of the tinyshader library.
 * See tinyshader.h for license details.
 */
#include "tinyshader_internal.h"

#define TS_PATHSEP '/'

#if defined(__unix__) || defined(__APPLE__)
#include <limits.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <spawn.h>

extern char **environ;
#endif

#if defined(__APPLE__)
#include <mach-o/dyld.h>
#endif

#if defined(_WIN32)
#include <windows.h>
#include <shlwapi.h>

#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "ole32.lib")
#pragma comment(lib, "oleaut32.lib")
#pragma comment(lib, "Shlwapi.lib")

#undef TS_PATHSEP
#define TS_PATHSEP '\\'
#endif

#define ARRAY_INITIAL_CAPACITY 16

void *ts__arrayGrow(
    TsCompiler *compiler, void *ptr, size_t *cap, size_t wanted_cap, size_t item_size)
{
    size_t desired_cap = TS__MAX(wanted_cap, ARRAY_INITIAL_CAPACITY);
    desired_cap = TS__MAX(desired_cap, (*cap) * 2);
    TS_ASSERT(desired_cap > 0);
    TS_ASSERT(desired_cap > (*cap));
    TS_ASSERT(desired_cap >= wanted_cap);

    void* new_arr = ts__bumpAlloc(&compiler->alloc, item_size * desired_cap);

    if (ptr)
    {
        memcpy(new_arr, ptr, (*cap) * item_size);
    }

    *cap = desired_cap;
    return new_arr;
}

int ts__strcasecmp(const char *_l, const char *_r)
{
	const unsigned char *l=(void *)_l, *r=(void *)_r;
	for (; *l && *r && (*l == *r || tolower(*l) == tolower(*r)); l++, r++);
	return tolower(*l) - tolower(*r);
}

////////////////////////////////
//
// Paths
//
////////////////////////////////

#if defined(_WIN32)
static char *utf16ToUtf8(TsCompiler *compiler, const wchar_t *source)
{
    int required_size = WideCharToMultiByte(CP_UTF8, 0, source, -1, NULL, 0, NULL, NULL);
    char *buf = NEW_ARRAY(compiler, char, required_size + 1);
    WideCharToMultiByte(CP_UTF8, 0, source, -1, buf, required_size, NULL, NULL);
    buf[required_size] = 0;
    return buf;
}

static wchar_t *utf8ToUtf16(TsCompiler *compiler, const char *source)
{
    int source_length = (int)strlen(source);
    DWORD wpath_len = MultiByteToWideChar(CP_UTF8, 0, source, source_length, NULL, 0);

    wchar_t *wpath = NEW_ARRAY(compiler, wchar_t, wpath_len + 1);

    MultiByteToWideChar(CP_UTF8, 0, source, source_length, wpath, wpath_len);

    wpath[wpath_len] = 0;

    return wpath;
}
#endif

static void replaceSlashes(char *path)
{
    for (size_t i = 0; i < strlen(path); ++i)
    {
        if (path[i] == '\\' || path[i] == '/') path[i] = TS_PATHSEP;
    }
}

char *ts__getAbsolutePath(TsCompiler *compiler, const char *relative_path)
{
    if (!relative_path) return NULL;

#if defined(__unix__) || defined(__APPLE__)
    char *path = realpath(relative_path, NULL);
    if (!path) return NULL;

    size_t path_size = strlen(path) + 1;
    char *new_path = NEW_ARRAY(compiler, char, path_size);
    memcpy(new_path, path, path_size);
    free(path);

    replaceSlashes(new_path);
    return new_path;
#elif defined(_WIN32)
    wchar_t *wide_relative_path = utf8ToUtf16(compiler, relative_path);

    DWORD length = GetFullPathNameW(wide_relative_path, 0, NULL, NULL);
    wchar_t *buf = NEW_ARRAY(compiler, wchar_t, 4 + length);
    GetFullPathNameW(wide_relative_path, length, buf, NULL);
    buf[length] = 0;

    char *c_str = utf16ToUtf8(compiler, buf);

    replaceSlashes(c_str);
    return c_str;
#else
#error OS not supported
#endif
}

char *ts__getPathDir(TsCompiler *compiler, const char *path)
{
    if (!path) return NULL;

    char *abs = ts__getAbsolutePath(compiler, path);
    for (int i = (int)strlen(abs) - 1; i >= 0; i--)
    {
        if (abs[i] == TS_PATHSEP)
        {
            abs[i + 1] = '\0';
            break;
        }
    }
    return abs;
}

char *ts__getCurrentDir(TsCompiler *compiler)
{
#if defined(__unix__) || defined(__APPLE__)
    char *cwd = getcwd(NULL, 0);
    if (!cwd) return NULL;

    size_t cwd_size = strlen(cwd) + 1;
    char *new_cwd = NEW_ARRAY(compiler, char, cwd_size);
    memcpy(new_cwd, cwd, cwd_size);
    free(cwd);
    return new_cwd;
#elif defined(_WIN32)
    DWORD required_size = GetCurrentDirectory(0, NULL);

    TCHAR *buf = NEW_ARRAY(compiler, TCHAR, required_size);
    GetCurrentDirectory(required_size, buf);
    if (sizeof(TCHAR) == sizeof(char))
    {
        return buf;
    }
    else
    {
        TS_ASSERT(sizeof(TCHAR) == sizeof(wchar_t));
        return utf16ToUtf8(compiler, (wchar_t *)buf);
    }

    return NULL;
#else
#error OS not supported
#endif
}

char *ts__pathConcat(TsCompiler *compiler, const char *a, const char *b)
{
    if (!a || !b) return NULL;

    size_t a_len = strlen(a);
    size_t b_len = strlen(b);

    bool insert_sep = !(a[a_len - 1] == '/' || a[a_len - 1] == '\\');

    size_t new_size = a_len + b_len + 1;
    if (insert_sep) new_size++;

    char *new_path = NEW_ARRAY(compiler, char, new_size);

    char *curr = new_path;
    memcpy(curr, a, a_len);
    curr += a_len;

    if (insert_sep)
    {
        *curr = TS_PATHSEP;
        curr += 1;
    }

    memcpy(curr, b, b_len);
    curr += b_len;

    *curr = '\0';

    replaceSlashes(new_path);
    return new_path;
}

bool ts__fileExists(TsCompiler *compiler, const char *path)
{
    if (!path) return false;
#if defined(__unix__) || defined(__APPLE__)
    (void)compiler;
    return (access(path, F_OK) != -1);
#elif defined(_WIN32)
    wchar_t *wide_path = utf8ToUtf16(compiler, path);
    return (bool)PathFileExistsW(wide_path);
#else
#error OS not supported
#endif
}

////////////////////////////////
//
// HashMap
//
////////////////////////////////

#define DEFAULT_HASHMAP_SIZE 32

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

static void hashGrow(HashMap *map);

void ts__hashInit(TsCompiler *compiler, HashMap *map, uint64_t size)
{
    memset(map, 0, sizeof(*map));
    map->compiler = compiler;

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
    map->keys = ts__bumpAlloc(&compiler->alloc, sizeof(*map->keys) * map->size);
    map->hashes = ts__bumpAlloc(&compiler->alloc, sizeof(*map->hashes) * map->size);
    map->indices = ts__bumpAlloc(&compiler->alloc, sizeof(*map->indices) * map->size);

    memset(map->keys, 0, sizeof(*map->keys) * map->size);
    memset(map->hashes, 0, sizeof(*map->hashes) * map->size);
}

static uint64_t hashSetInternal(HashMap *map, const char *key, uint64_t index)
{
    uint64_t hash = hashStr(key);
    uint64_t i = hash & (map->size - 1);
    uint64_t iters = 0;
    while ((map->hashes[i] != hash || strcmp(map->keys[i], key) != 0) &&
           map->hashes[i] != 0 && iters < map->size)
    {
        i = (i + 1) & (map->size - 1);
        iters++;
    }

    if (iters >= map->size)
    {
        hashGrow(map);
        return hashSetInternal(map, key, index);
    }

    map->keys[i] = (char *)key;
    map->hashes[i] = hash;
    map->indices[i] = index;

    return index;
}

void *ts__hashSet(HashMap *map, const char *key, void *value)
{
    size_t index = arrLength(map->values);
    arrPush(map->compiler, &map->values, value);
    hashSetInternal(map, key, index);
    return map->values.ptr[index];
}

bool ts__hashGet(HashMap *map, const char *key, void **result)
{
    uint64_t hash = hashStr(key);
    uint64_t i = hash & (map->size - 1);
    uint64_t iters = 0;
    while ((map->hashes[i] != hash || strcmp(map->keys[i], key) != 0) &&
           map->hashes[i] != 0 && iters < map->size)
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
        if (result) *result = map->values.ptr[map->indices[i]];
        return true;
    }

    return false;
}

void ts__hashRemove(HashMap *map, const char *key)
{
    uint64_t hash = hashStr(key);
    uint64_t i = hash & (map->size - 1);
    uint64_t iters = 0;
    while ((map->hashes[i] != hash || strcmp(map->keys[i], key) != 0) &&
           map->hashes[i] != 0 && iters < map->size)
    {
        i = (i + 1) & (map->size - 1);
        iters++;
    }

    if (iters >= map->size)
    {
        return;
    }

    map->hashes[i] = 0;
}

static void hashGrow(HashMap *map)
{
    uint64_t old_size = map->size;
    char **old_keys = map->keys;
    uint64_t *old_hashes = map->hashes;
    uint64_t *old_indices = map->indices;

    map->size = old_size * 2;
    map->hashes = ts__bumpAlloc(&map->compiler->alloc, sizeof(*map->hashes) * map->size);
    map->indices = ts__bumpAlloc(&map->compiler->alloc, sizeof(*map->indices) * map->size);
    map->keys = ts__bumpAlloc(&map->compiler->alloc, sizeof(*map->keys) * map->size);
    memset(map->hashes, 0, sizeof(*map->hashes) * map->size);
    memset(map->keys, 0, sizeof(*map->keys) * map->size);

    for (uint64_t i = 0; i < old_size; i++)
    {
        if (old_hashes[i] != 0)
        {
            hashSetInternal(map, old_keys[i], old_indices[i]);
        }
    }

    /* free(old_hashes); */
    /* free(old_indices); */
    /* free(old_keys); */
}

void ts__hashDestroy(HashMap *map)
{
    /* free(map->hashes); */
    /* free(map->indices); */
    /* free(map->keys); */
    arrFree(map->compiler, &map->values);
}

////////////////////////////////
//
// Bump allocator
//
////////////////////////////////

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
    size_t padding = (block->pos % 8);
    if (padding > 0) padding = 8 - padding;

    TS_ASSERT((block->size - block->pos) >= (size + padding));

    void *data = block->data + block->pos + padding;
    TS_ASSERT((uintptr_t)data % 8 == 0);
    block->pos += size + padding;
    return data;
}

void ts__bumpInit(BumpAlloc *alloc, size_t block_size)
{
    alloc->block_size = block_size;
    alloc->last_block_size = alloc->block_size;
    blockInit(&alloc->base_block, block_size);
    alloc->last_block = &alloc->base_block;
}

void *ts__bumpAlloc(BumpAlloc *alloc, size_t size)
{
    if (size == 0)
    {
        return NULL;
    }

    size_t padding = (alloc->last_block->pos % 8);
    if (padding > 0) padding = 8 - padding;

    size_t space = alloc->last_block->size - alloc->last_block->pos;
    if (space < (size + padding))
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

void *ts__bumpZeroAlloc(BumpAlloc *alloc, size_t size)
{
    TS_ASSERT(size > 0);
    void *data = ts__bumpAlloc(alloc, size);
    TS_ASSERT(data);
    memset(data, 0, size);
    return data;
}

char *ts__bumpStrndup(BumpAlloc *alloc, const char *str, size_t length)
{
    char *ptr = ts__bumpAlloc(alloc, length + 1);
    memcpy(ptr, str, length);
    ptr[length] = '\0';
    return ptr;
}

void ts__bumpDestroy(BumpAlloc *alloc)
{
    blockDestroy(&alloc->base_block);
}

////////////////////////////////
//
// String builder
//
////////////////////////////////

void ts__sbInit(StringBuilder *sb)
{
    sb->len = 0;
    sb->cap = 1 << 16; // 64k
    sb->buf = malloc(sb->cap);
    sb->scratch = malloc(sb->cap);
}

void ts__sbDestroy(StringBuilder *sb)
{
    free(sb->buf);
    free(sb->scratch);
}

void ts__sbReset(StringBuilder *sb)
{
    sb->len = 0;
}

static void sbGrow(StringBuilder *sb)
{
    sb->cap *= 2;
    sb->buf = realloc(sb->buf, sb->cap);
    sb->scratch = realloc(sb->scratch, sb->cap);
}

void ts__sbAppend(StringBuilder *sb, const char *str)
{
    size_t len = strlen(str);
    while (len + sb->len >= sb->cap)
    {
        sbGrow(sb);
    }
    memcpy(&sb->buf[sb->len], str, len);
    sb->len += len;
}

void ts__sbAppendLen(StringBuilder *sb, const char *str, size_t len)
{
    while (len + sb->len >= sb->cap)
    {
        sbGrow(sb);
    }
    strncpy(&sb->buf[sb->len], str, len);
    sb->len += len;
}

void ts__sbAppendChar(StringBuilder *sb, char c)
{
    while (1 + sb->len >= sb->cap)
    {
        sbGrow(sb);
    }
    sb->buf[sb->len] = c;
    sb->len += 1;
}

void ts__sbSprintf(StringBuilder *sb, const char *fmt, ...)
{
    va_list vl;
    va_start(vl, fmt);
    vsnprintf(sb->scratch, sb->cap, fmt, vl);
    va_end(vl);
    ts__sbAppend(sb, sb->scratch);
}

void ts__sbVsprintf(StringBuilder *sb, const char *fmt, va_list vl)
{
    vsnprintf(sb->scratch, sb->cap, fmt, vl);
    ts__sbAppend(sb, sb->scratch);
}

char *ts__sbBuildMalloc(StringBuilder *sb)
{
    char *result = malloc(sb->len + 1);
    strncpy(result, sb->buf, sb->len);
    result[sb->len] = '\0';
    return result;
}

char *ts__sbBuild(StringBuilder *sb, BumpAlloc *bump)
{
    char *result = ts__bumpAlloc(bump, sb->len + 1);
    strncpy(result, sb->buf, sb->len);
    result[sb->len] = '\0';
    return result;
}

static const char *TOKEN_STRINGS[TOKEN_MAX] = {
    [TOKEN_LPAREN] = "(",
    [TOKEN_RPAREN] = ")",
    [TOKEN_LBRACK] = "[",
    [TOKEN_RBRACK] = "]",
    [TOKEN_LCURLY] = "{",
    [TOKEN_RCURLY] = "}",
    [TOKEN_HASH] = "#",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_COLON] = ":",
    [TOKEN_COLON_COLON] = "::",
    [TOKEN_ADD] = "+",
    [TOKEN_SUB] = "-",
    [TOKEN_MUL] = "*",
    [TOKEN_DIV] = "/",
    [TOKEN_MOD] = "%",
    [TOKEN_ADDADD] = "++",
    [TOKEN_SUBSUB] = "--",
    [TOKEN_BITOR] = "|",
    [TOKEN_BITXOR] = "^",
    [TOKEN_BITAND] = "&",
    [TOKEN_BITNOT] = "~",
    [TOKEN_LSHIFT] = "<<",
    [TOKEN_RSHIFT] = ">>",
    [TOKEN_PERIOD] = ".",
    [TOKEN_COMMA] = ",",
    [TOKEN_QUESTION] = "?",
    [TOKEN_NOT] = "!",
    [TOKEN_ASSIGN] = "=",
    [TOKEN_EQUAL] = "==",
    [TOKEN_NOTEQ] = "!=",
    [TOKEN_LESS] = "<",
    [TOKEN_LESSEQ] = "<=",
    [TOKEN_GREATER] = ">",
    [TOKEN_GREATEREQ] = ">=",
    [TOKEN_ADD_ASSIGN] = "+=",
    [TOKEN_SUB_ASSIGN] = "-=",
    [TOKEN_MUL_ASSIGN] = "*=",
    [TOKEN_DIV_ASSIGN] = "/=",
    [TOKEN_MOD_ASSIGN] = "%=",
    [TOKEN_BITAND_ASSIGN] = "&=",
    [TOKEN_BITOR_ASSIGN] = "|=",
    [TOKEN_BITXOR_ASSIGN] = "^=",
    [TOKEN_LSHIFT_ASSIGN] = "<<=",
    [TOKEN_RSHIFT_ASSIGN] = ">>=",
    [TOKEN_AND] = "&&",
    [TOKEN_OR] = "||",
    [TOKEN_IDENT] = "<identifier>",
    [TOKEN_IN] = "in",
    [TOKEN_OUT] = "out",
    [TOKEN_INOUT] = "inout",
    [TOKEN_STRUCT] = "struct",
    [TOKEN_FOR] = "for",
    [TOKEN_WHILE] = "while",
    [TOKEN_DO] = "do",
    [TOKEN_SWITCH] = "switch",
    [TOKEN_CASE] = "case",
    [TOKEN_DEFAULT] = "default",
    [TOKEN_BREAK] = "break",
    [TOKEN_CONTINUE] = "continue",
    [TOKEN_IF] = "if",
    [TOKEN_ELSE] = "else",
    [TOKEN_RETURN] = "return",
    [TOKEN_CONST] = "const",
    [TOKEN_CBUFFER] = "cbuffer",
    [TOKEN_CONSTANT_BUFFER] = "ConstantBuffer",
    [TOKEN_STRUCTURED_BUFFER] = "StructuredBuffer",
    [TOKEN_RW_STRUCTURED_BUFFER] = "RWStructuredBuffer",
    [TOKEN_SAMPLER] = "sampler",
    [TOKEN_SAMPLER_STATE] = "SamplerState",
    [TOKEN_TEXTURE_1D] = "Texture1D",
    [TOKEN_TEXTURE_2D] = "Texture2D",
    [TOKEN_TEXTURE_3D] = "Texture3D",
    [TOKEN_TEXTURE_CUBE] = "TextureCube",
    [TOKEN_DISCARD] = "discard",
    [TOKEN_INT_LIT] = "<integer literal>",
    [TOKEN_FLOAT_LIT] = "<float literal>",
    [TOKEN_STRING_LIT] = "<string literal>",
    [TOKEN_BOOL] = "bool",
    [TOKEN_UINT] = "uint",
    [TOKEN_INT] = "int",
    [TOKEN_FLOAT] = "float",
    [TOKEN_VOID] = "void",
    [TOKEN_FALSE] = "false",
    [TOKEN_TRUE] = "true",
    [TOKEN_VECTOR_TYPE] = "<vector type>",
    [TOKEN_MATRIX_TYPE] = "<matrix type>",
    [TOKEN_STATIC] = "static",
    [TOKEN_UNIFORM] = "uniform",
    [TOKEN_GROUPSHARED] = "groupshared",
    [TOKEN_REGISTER] = "register",
};

const char *ts__getTokenString(TokenKind kind)
{
    return TOKEN_STRINGS[kind];
}
