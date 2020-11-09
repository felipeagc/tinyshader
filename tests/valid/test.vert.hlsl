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

void main(
	in uint vertexIndex : SV_VertexID,
	in float3 pos : Heyy,
	out float4 out_pos : SV_POSITION,
	out float2 out_uv : AAA)
{
	float3x3 my_matrix = float3x3(1, 2, 3, 4, 5, 6, 7, 8, 9);

	out_uv = float2(float((vertexIndex << 1) & 2), float(vertexIndex & 2));
	float2 temp = out_uv * 2.0 - 1.0;
    out_pos = float4(temp.x, temp.y, 0.0, 1.0);

	float noise = rand(out_uv);
	float2 a2 = sin(float2(1, 1));

    {
        out_uv = float2(float((vertexIndex << 1) & 2), float(vertexIndex & 2));
        out_pos = float4(out_uv * 2.0f - 1.0f, 0.0f, 1.0f);
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
        out_pos = mul(gInput.yoo, mul(gInput.view, gInput.transform)).xyzw;
        // otherFunc(pos);
        out_pos = float4(pos, 1.0);
        my_bool = pos.x > 1;
    }

    {
        pos = normalize(pos);
        pos.x = distance(pos, pos);

        while (pos.x > 0.0)
        {
            pos.x = pos.x - 1.0;
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

	if (pos.x > 0.0)
    {
		pos.x = sqrt(123.0);
	}
}
