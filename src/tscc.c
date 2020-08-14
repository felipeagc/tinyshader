#include "tsc.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static char *loadFile(const char *path, size_t *out_size)
{
    FILE *f = fopen(path, "rb");
    if (!f) return false;

    fseek(f, 0, SEEK_END);
    *out_size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *data = malloc(*out_size);
    fread(data, 1, *out_size, f);

    fclose(f);

    return data;
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <file>\n", argv[0]);
        exit(1);
    }

    char *path = argv[1];

    size_t file_size = 0;
    char *file_data = loadFile(path, &file_size);

    TscCompiler *compiler = tscCompilerCreate();

    TscCompilerInput input = {
        .input = file_data,
        .input_size = file_size,
        .path = path,
    };

    TscCompilerOutput output = {0};
    tscCompile(compiler, &input, &output);
    if (output.error_count > 0)
    {
        for (char **err = output.errors; err != output.errors + output.error_count; ++err)
        {
            fprintf(stderr, "%s\n", *err);
        }

        exit(1);
    }

    FILE *f = fopen("a.spv", "wb");
    if (!f)
    {
        fprintf(stderr, "failed to open output file\n");
        exit(1);
    }

    fwrite(output.spirv, output.spirv_byte_size, 1, f);

    fclose(f);

    tscCompilerOutputDestroy(compiler, &output);

    tscCompilerDestroy(compiler);

    free(file_data);

    return 0;
}
