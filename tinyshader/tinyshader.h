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
  char *path; // optional
  char *input;
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
