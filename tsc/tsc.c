/**
 * This file is part of the tinyshader library.
 * See tinyshader.h for license details.
 */
#include "tinyshader.h"

#define OPTPARSE_IMPLEMENTATION
#include "optparse.h"

#ifdef _MSC_VER
#pragma warning(disable:4996)
#endif

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *loadFile(const char *path, size_t *out_size)
{
    FILE *f = fopen(path, "rb");
    if (!f) return false;

    fseek(f, 0, SEEK_END);
    *out_size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *data = malloc(*out_size);
    size_t read_size = fread(data, 1, *out_size, f);
    assert(read_size == *out_size);

    fclose(f);

    return data;
}

static bool compileStage(
    char *out_file_name,
    char *input_path,
    char *file_data,
    size_t file_size,
    char *entry_point,
    TsShaderStage stage)
{
    TsCompilerOptions *options = tsCompilerOptionsCreate();
    tsCompilerOptionsSetStage(options, stage);
    tsCompilerOptionsSetSource(options, file_data, file_size, input_path, strlen(input_path));
    tsCompilerOptionsSetEntryPoint(options, entry_point, strlen(entry_point));

    TsCompilerOutput *output = tsCompile(options);
    const char *errors = tsCompilerOutputGetErrors(output);
    if (errors)
    {
        fprintf(stderr, "%s", errors);
        tsCompilerOutputDestroy(output);
        tsCompilerOptionsDestroy(options);
        return false;
    }

    size_t spirv_byte_size = 0;
    const unsigned char *spirv = tsCompilerOutputGetSpirv(output, &spirv_byte_size);

    FILE *f = fopen(out_file_name, "wb");
    if (!f)
    {
        fprintf(stderr, "failed to open output file\n");
        fclose(f);
        tsCompilerOutputDestroy(output);
        tsCompilerOptionsDestroy(options);
        return false;
    }

    fwrite(spirv, spirv_byte_size, 1, f);

    fclose(f);

    tsCompilerOutputDestroy(output);
    tsCompilerOptionsDestroy(options);
    return true;
}

int main(int argc, char *argv[])
{
    (void)argc;

    struct optparse_long longopts[] = {
        {"shader-stage", 'T', OPTPARSE_REQUIRED},
        {"entry-point", 'E', OPTPARSE_REQUIRED},
        {"output", 'o', OPTPARSE_REQUIRED},
        {0}};

    TsShaderStage stage = TS_SHADER_STAGE_VERTEX;
    char *out_path = "a.spv";
    char *entry_point = "main";
    char *path = NULL;

    char *arg;
    int option;
    struct optparse options;

    optparse_init(&options, argv);
    while ((option = optparse_long(&options, longopts, NULL)) != -1)
    {
        switch (option)
        {
        case 'T':
            if (strcmp(options.optarg, "vertex") == 0)
            {
                stage = TS_SHADER_STAGE_VERTEX;
            }
            else if (strcmp(options.optarg, "fragment") == 0)
            {
                stage = TS_SHADER_STAGE_FRAGMENT;
            }
            else if (strcmp(options.optarg, "compute") == 0)
            {
                stage = TS_SHADER_STAGE_COMPUTE;
            }
            else
            {
                fprintf(stderr, "Unrecognized shader stage: %s\n", options.optarg);
                exit(EXIT_FAILURE);
            }
            break;
        case 'E': entry_point = options.optarg; break;
        case 'o': out_path = options.optarg; break;
        case '?':
            fprintf(stderr, "%s: %s\n", argv[0], options.errmsg);
            exit(EXIT_FAILURE);
        }
    }

    while ((arg = optparse_arg(&options)))
    {
        path = arg;
    }

    if (!path)
    {
        fprintf(
            stderr,
            "Usage: %s [--shader-stage <stage>] [--entry-point <entry point>] [-o "
            "<output path>] <filename>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    size_t file_size = 0;
    char *file_data = loadFile(path, &file_size);

    bool result = compileStage(out_path, path, file_data, file_size, entry_point, stage);

    free(file_data);

    if (!result)
    {
        return 1;
    }

    return 0;
}
