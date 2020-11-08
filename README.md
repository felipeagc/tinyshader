# Tinyshader
**Lightweight, easy to embed HLSL to SPIR-V compiler written in C99**

## Using the command-line compiler
The command-line compiler sources are located under the folder `tsc` and
can be used as follows:

```
Usage: tsc <input file path>
    --shader-stage | -T <vertex|fragment|compute>
    --entry-point | -E <entry point name>
    -o <output file path>
```

## Using the compiler as a library
The compiler library sources are located under the folder `tinyshader` and
can be used as follows:

```c
#include "tinyshader.h"

TsCompiler *compiler = tsCompilerCreate();

TsCompilerInput input = {
    .input = hlsl_text,
    .input_size = hlsl_text_size,
    .path = input_path, // optional, required for #include support
    .entry_point = "main",
    .stage = TS_SHADER_STAGE_VERTEX,
};

TsCompilerOutput output = {0};
tsCompile(compiler, &input, &output);
if (output.error)
{
    fprintf(stderr, "%s", output.error);
    exit(1);
}

// Now we have SPIR-V code, ready to pass to Vulkan
uint8_t *spirv = output.spirv;
size_t spirv_byte_size = output.spirv_byte_size;

// Cleanup
tsCompilerOutputDestroy(&output); // This frees output.spirv
tsCompilerDestroy(compiler);
```

## Compiling
Compiling tinyshader is very simple, you just need to compile the `tinyshader/tinyshader_*.c`
files (except `tinyshader/tinyshader_unity.c`), no complicated build system involved.
Alternatively you can also compile `tinyshader/tinyshader_unity.c` to compile all of
the files in one go.

## Goals and implemented features
The goal of this compiler is to be as compatible as possible with
[DXC](https://github.com/microsoft/DirectXShaderCompiler), implementing most of its features,
while being very lightweight in terms of code size.

So far, though, only a subset of HLSL features are supported.
You can check out the progress in [this issue](https://github.com/felipeagc/tinyshader/issues/1).

Regarding optimization and quality of the generated code,
tinyshader is supposed to provide 80% of what you need for
10% of the code, so more advanced optimization is not planned as of now.

## Vulkan resource binding

### Descriptor set/binding mapping
Descriptor binding information can either be automatic
(using only descriptor set 0 and incremented bindings)
or explicit through the `[[vk::binding(binding, set)]]` attribute.

HLSL register bindings are currently ignored.

Examples of resource binding:

```hlsl
ConstantBuffer<Uniform> gInput; // Binding: 0, Set: 0
Texture2D gInput2; // Binding: 1, Set: 0
SamplerState gInput3; // Binding: 2, Set: 0
```

```hlsl
[[vk::binding(0)]] ConstantBuffer<Uniform> gInput; // Binding: 0, Set: 0
[[vk::binding(1)]] Texture2D gInput2; // Binding: 1, Set: 0
[[vk::binding(2)]] SamplerState gInput3; // Binding: 2, Set: 0
```

```hlsl
[[vk::binding(0)]] ConstantBuffer<Uniform> gInput; // Binding: 0, Set: 0
[[vk::binding(1, 0)]] Texture2D gInput2; // Binding: 1, Set: 0
[[vk::binding(2, 1)]] SamplerState gInput3; // Binding: 2, Set: 1
```

### Entry point inputs/outputs
As of now, you cannot pass the inputs of shader entry points as a struct.
You also cannot pass the outputs as return values.
You need to use in/out parameters for that instead, for example:

```hlsl
void main(
    in float4 pos : POSITION, // Input location: 0
    in float2 uv : TEXCOORD0, // Input location: 1
    out float4 out_color : SV_Target // Output location: 0
) {
    out_color = 1.0f;
}
```

## License
This library is available to anybody free of charge, under the terms of MIT License
(see [LICENSE](https://github.com/felipeagc/tinyshader/blob/master/LICENSE)).
