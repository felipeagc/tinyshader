#ifndef TINY_SHADER_COMPILER_H
#define TINY_SHADER_COMPILER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct TsCompilerInput {
  char *path; // optional
  char *input;
  size_t input_size;
} TsCompilerInput;

typedef struct TsCompilerOutput {
  uint8_t *spirv;
  size_t spirv_byte_size;

  char **errors;
  size_t error_count;
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
