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

TsCompilerOptions *options = tsCompilerOptionsCreate();

tsCompilerOptionsSetStage(options, TS_SHADER_STAGE_VERTEX);

const char *entry_point = "main";
tsCompilerOptionsSetEntryPoint(options, entry_point, strlen(entry_point));

tsCompilerOptionsSetSource(
    options,
    hlsl_source,
    strlen(hlsl_source),
    path, // optional, can be NULL
    strlen(path) // if path is NULL, this should be zero
);

TsCompilerOutput *output = tsCompile(options);

/*
 * 'errors' is a null-terminated string containing compiler error messages.
 * If it's NULL, compilation was successful.
 */
const char *errors = tsCompilerOutputGetErrors(output);
if (errors)
{
    printf("%s\n", errors);
    exit(1);
}

/*
 * Now we have SPIR-V code, ready to pass to Vulkan.
 * NOTE: the spirv pointer is owned by TsCompilerOutput and is freed when it's destroyed.
 */
size_t spirv_byte_size;
const unsigned char *spirv = tsCompilerOutputGetSpirv(output, &spirv_byte_size);

// Cleanup
tsCompilerOutputDestroy(output);
tsCompilerOptionsDestroy(options);
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

### Stage inputs/outputs
You can either pass the stage inputs/outputs as individual parameters or put them in a struct, where
each struct member occupies a location.
You can also use the returned value of a function as the stage output.

Examples of stage inputs/outputs:

```hlsl
void main(
    in float4 pos : POSITION, // Input location: 0
    in float2 uv : TEXCOORD0, // Input location: 1
    out float4 out_color : SV_Target // Output location: 0
) {
    out_color = 1.0f;
}
```

```hlsl
struct PSInput
{
    float4 pos : POSITION; // Input location: 0
    float4 sv_pos : SV_Position; // Same as gl_FragCoord from GLSL, doesn't count as an input location
    float2 uv : TEXCOORD0; // Input location: 1
};

float4 main(in PSInput ps_in) : SV_Target {
    return float4(ps_in.uv, 0.0, 1.0); // Outputs to location 0
}
```

## License
This library is available to anybody free of charge, under the terms of MIT License
(see [LICENSE](https://github.com/felipeagc/tinyshader/blob/master/LICENSE)).
