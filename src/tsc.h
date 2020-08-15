#ifndef TINY_SHADER_COMPILER_H
#define TINY_SHADER_COMPILER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct TscCompilerInput {
  char *path; // optional
  char *input;
  size_t input_size;
} TscCompilerInput;

typedef struct TscCompilerOutput {
  uint8_t *spirv;
  size_t spirv_byte_size;

  char **errors;
  size_t error_count;
} TscCompilerOutput;

typedef struct TscCompiler TscCompiler;

TscCompiler *tscCompilerCreate();
void tscCompilerDestroy(TscCompiler *compiler);

void tscCompile(TscCompiler *compiler, TscCompilerInput *input,
                TscCompilerOutput *output);

void tscCompilerOutputDestroy(TscCompilerOutput *output);

#ifdef __cplusplus
}
#endif

#endif // TINY_SHADER_COMPILER_H
