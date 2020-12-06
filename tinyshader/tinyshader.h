/**
 * This library is distributed under the MIT License. See notice at the end of this file.
 */

#ifndef TINY_SHADER_COMPILER_H
#define TINY_SHADER_COMPILER_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct TsCompilerOptions TsCompilerOptions;
typedef struct TsCompilerOutput TsCompilerOutput;

typedef enum TsShaderStage {
    TS_SHADER_STAGE_VERTEX,
    TS_SHADER_STAGE_FRAGMENT,
    TS_SHADER_STAGE_COMPUTE,
} TsShaderStage;

TsCompilerOptions *tsCompilerOptionsCreate(void);
void tsCompilerOptionsSetStage(TsCompilerOptions *options, TsShaderStage stage);
void tsCompilerOptionsSetEntryPoint(TsCompilerOptions *options, const char *entry_point, size_t entry_point_length);
void tsCompilerOptionsSetSource(
    TsCompilerOptions *options,
    const char* source,
    size_t source_length,
    const char *path, // can be NULL
    size_t path_length // if path is NULL, this should be zero
);
void tsCompilerOptionsAddIncludePath(TsCompilerOptions *options, const char* path, size_t path_length);
void tsCompilerOptionsDestroy(TsCompilerOptions *options);

TsCompilerOutput *tsCompile(TsCompilerOptions *options);
const char *tsCompilerOutputGetErrors(TsCompilerOutput *output);
const unsigned char *tsCompilerOutputGetSpirv(TsCompilerOutput *output, size_t *spirv_byte_size);
void tsCompilerOutputDestroy(TsCompilerOutput *output);

#ifdef __cplusplus
}
#endif

#endif // TINY_SHADER_COMPILER_H

/**
 * tinyshader is distributed under the MIT license:
 *
 * Copyright (c) 2020 Felipe A. Costa
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */
