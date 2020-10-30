/**
 * This library is distributed under the MIT License. See notice at the end of this file.
 */

#ifndef TINY_SHADER_COMPILER_H
#define TINY_SHADER_COMPILER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum TsShaderStage {
    TS_SHADER_STAGE_VERTEX,
    TS_SHADER_STAGE_FRAGMENT,
    TS_SHADER_STAGE_COMPUTE,
} TsShaderStage;

typedef struct TsCompilerInput {
  const char *path; // optional
  const char *input;
  size_t input_size;

  const char *entry_point;
  TsShaderStage stage;
} TsCompilerInput;

typedef struct TsCompilerOutput {
  uint8_t *spirv;
  size_t spirv_byte_size;

  char *error;
} TsCompilerOutput;

typedef struct TsCompiler TsCompiler;

TsCompiler *tsCompilerCreate();
void tsCompilerDestroy(TsCompiler *compiler);

void tsCompile(TsCompiler *compiler, TsCompilerInput *input,
                TsCompilerOutput *output);

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
