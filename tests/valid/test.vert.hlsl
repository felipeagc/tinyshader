#include "included.hlsl"

#define SOME_CONST 1.0123123
#define HELLO 123 + 123

#ifndef ASDASD
#else
heyy
#endif

struct Uniform
{
    float4 yoo;
    float hey;
    Hello hello;
    float4x4 transform;
    float4x4 view;
};

ConstantBuffer<Uniform> gInput;
Texture2D gInput2;
SamplerState gInput3;

void otherFunc(inout float3 pos)
{
    pos.x = pos.x * SOME_CONST;
}

float rand(float2 uv)
{
    return frac(sin(dot(uv.xy, float2(12.9898,78.233))) * 43758.5453);
}

struct VsInput
{
    uint vertexIndex : SV_VertexID;
    float3 pos : POSITION;
};

struct VsOutput
{
    float4 sv_pos : SV_POSITION;
    float4 pos : POSITION;
    float2 uv : TEXCOORD0;
};

void main(in VsInput vs_in, out VsOutput vs_out)
{
    float3x3 my_matrix = float3x3(1, 2, 3, 4, 5, 6, 7, 8, 9);

    vs_out.uv = float2(float((vs_in.vertexIndex << 1) & 2), float(vs_in.vertexIndex & 2));
    float2 temp = vs_out.uv * 2.0 - 1.0;
    vs_out.sv_pos = float4(temp.x, temp.y, 0.0, 1.0);

    float noise = rand(vs_out.uv);
    float2 a2 = sin(float2(1, 1));

    {
        vs_out.uv = float2(float((vs_in.vertexIndex << 1) & 2), float(vs_in.vertexIndex & 2));
        vs_out.sv_pos = float4(vs_out.uv * 2.0f - 1.0f, 0.0f, 1.0f);
    }

    {
        Hello struct_var;
        struct_var.a = 123;
        struct_var.c.c.c = 123;
        struct_var.c.vec.x = 123;
    }

    {
        int a = 123;
        float b = float(a);
        int c = int(b);

        float3 hey = float3(123, 123, 123);
        float2 hey1 = float2(123, 123);
        float4 hey2 = float4(b, b, b, b);
        float3 vec = float3(1, 2, 3);
        float shuffled = -vec.yz.x;
        float yoo = dot(vec, vec);
        float shuffled2 = float3(yoo, yoo, yoo).yz.x;
        bool my_bool = vec.x == vec.y;

        float3 new_vec = vec / float3(shuffled, shuffled, shuffled);
        vs_out.sv_pos = mul(gInput.yoo, mul(gInput.view, gInput.transform)).xyzw;
        // otherFunc(pos);
        vs_out.sv_pos = float4(vs_in.pos, 1.0);
        my_bool = vs_in.pos.x > 1;
    }

    {
        vs_in.pos = normalize(vs_in.pos);
        vs_in.pos.x = distance(vs_in.pos, vs_in.pos);

        while (vs_in.pos.x > 0.0)
        {
            vs_in.pos.x = vs_in.pos.x - 1.0;
        }
    }

    {
        float clamped = clamp(log2(123), 0, 1);
        float a = smoothstep(0, 1, 213) * 123 * 3 * 2 * 2;
        float2x3 mat;
        float3x2 mat2 = transpose(mat);
        float3x3 mat3;
        float b = determinant(mat3);
    }

    if (vs_in.pos.x > 0.0)
    {
        vs_in.pos.x = sqrt(123.0);
    }
    vs_in.pos.x = 1.0;

    {
        int4 a_int_vec = 1.0;
        float4 a_vector = a_int_vec;
        a_vector = 1.0;
        a_vector = int(1.0);
    }

    vs_out.pos = vs_out.sv_pos;
}
