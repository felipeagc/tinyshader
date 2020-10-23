/* struct OtherOtherStruct { int c; }; */
/* struct OtherStruct { */
/* 	OtherOtherStruct c; */  
/* 	float3 vec; */
/* }; */

/* struct Hello */
/* { */
/*     int a; */
/*     int b; */
/* 	OtherStruct c; */
/* }; */

#include "../examples/hello.hlsl"
#include "../examples/hello.hlsl"

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
	/* Hello hello; */
	float4x4 transform;
	float4x4 view;
};

const int my_const = 123;

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

void vertex(
	in uint vertexIndex : SV_VertexID,
	in float3 pos : Heyy,
	out float4 out_pos : SV_Position,
	out float2 out_uv : AAA)
{
	/* uint a = (vertexIndex << 1) & 2; */

	float3x3 my_matrix = float3x3(1, 2, 3, 4, 5, 6, 7, 8, 9);

	out_uv = float2(float((vertexIndex << 1) & 2), float(vertexIndex & 2));
	float2 temp = out_uv * 2.0 - 1.0;
    out_pos = float4(temp.x, temp.y, 0.0, 1.0);

	float noise = rand(out_uv);
	float2 a = sin(float2(1, 1));

	/* out_uv = float2((vertexIndex << 1) & 2, vertexIndex & 2); */
    /* out_pos = float4(out_uv * 2.0f - 1.0f, 0.0f, 1.0f); */

	// float aaaaaaa = HELLO;
	// int other = 12;
	// int hello = 777777;
	// hello = 321;
	// other = hello;
	// int another = other;

	// float3 heyy;
	// float4x4 mat;

	// Hello struct_var;
	// struct_var.a = 123;
	// struct_var.c.c.c = 123;
	// struct_var.c.vec.x = 123;


	// otherFunc(123);

	// float3 a;
	// float3 b;
	// float c = dot(a, b);

	// float my_float = otherFunc(123.123);

	// int a = 123;
	// float b = float(a);
	// int c = int(b);

	// float3 hey = float3(123, 123, 123);
	// float2 hey1 = float2(123, 123);
	// float4 hey2 = float4(b, b, b, b);
	// float4 hey3 = float4(b);
	// float3 vec = float3(1, 2, 3);
	// float shuffled = -vec.yz.x;
	// float yoo = dot(vec, vec);
	// float shuffled2 = float3(yoo, yoo, yoo).yz.x;
	// bool hey = vec.x == vec.y;

	// float3 new_vec = vec / float3(shuffled, shuffled, shuffled);
	// otherFunc(123);
	// out_pos = mul(gInput.yoo, mul(gInput.view, gInput.transform)).xyz;
	// otherFunc(pos);
	// out_pos = pos;
	// bool hey = pos.x > 1;

	// pos = normalize(pos);

	// distance(pos, pos);
	// while (pos.x > 0.0) {
	// 	pos.x = pos.x - 1.0;
	// }

	// clamp(log2(123), 0, 1);
	// float a = smoothstep(0, 1, 213) * 123 * 3 * 2 * 2;
	// float2x3 mat;
	// float3x2 mat2 = transpose(mat);
	// float b = determinant(mat);

	// if (pos.x > 0.0) {
	// 	pos.x = sqrt(123.0);
	// }
}

void pixel(in float4 pos : POSITION, in float2 uv : TEXCOORD0, out float4 color : SV_Target)
{
    for (; pos.x > 0;) {
        if (pos.x <= 123)
        {
            break;
        }
    }

    float4 my_color = gInput2.Sample(gInput3, uv) * float4(ddx(1.0), 1, 1, 1) *
        float4(gInput.hey, gInput.hey, gInput.hey, gInput.hey);

	my_color = gInput2.SampleLevel(gInput3, uv, 1);
    float width;
    float height;
    float mip_levels;
    gInput2.GetDimensions(0, width, height, mip_levels);

    float3 up = abs(pos.x) < 0.999 ? float3(0.0, 0.0, 1.0) : float3(1.0, 0.0, 0.0);

	color = my_color;
    color.r = up.z;
}
