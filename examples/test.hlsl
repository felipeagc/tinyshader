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
Texture2D<float4> gInput2;
SamplerState gInput3;

void otherFunc(inout float3 pos)
{
	pos.x = pos.x * SOME_CONST;
}

void vertex(in float3 pos : Heyy, out float3 out_pos : AAA)
{
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

void pixel(in float3 pos : POSITION, in float2 uv : TEXCOORD0, out float4 color : SV_Target)
{
    for (; pos.x > 0.0;) {
        if (pos.x == 123.0)
        {
            break;
        }
    }

	// float4 my_color = gInput2.Sample(gInput3, uv) * float4(ddx(1.0), 1, 1, 1);
	// color = my_color;
}
