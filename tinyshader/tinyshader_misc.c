#include "tinyshader_internal.h"

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

void ts__hashInit(HashMap *map, uint64_t size)
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

    map->keys[i] = key;
    map->hashes[i] = hash;
    map->indices[i] = index;

    return index;
}

void *ts__hashSet(HashMap *map, const char *key, void *value)
{
    size_t index = arrLength(map->values);
    arrPush(map->values, value);
    hashSetInternal(map, key, index);
    return map->values[index];
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
        if (result) *result = map->values[map->indices[i]];
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

void ts__hashDestroy(HashMap *map)
{
    free(map->hashes);
    free(map->indices);
    free(map->keys);
    arrFree(map->values);
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
    assert((block->size - block->pos) >= size);
    void *data = block->data + block->pos;
    block->pos += size;
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

void *ts__bumpZeroAlloc(BumpAlloc *alloc, size_t size)
{
    void *data = ts__bumpAlloc(alloc, size);
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
