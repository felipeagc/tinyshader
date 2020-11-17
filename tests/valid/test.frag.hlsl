#include "./included.hlsl"
#include "./included.hlsl"

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

void main(
    in float4 pos : POSITION,
    in float2 uv : TEXCOORD0,
    out float4 color : SV_Target)
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
    color = float4(1, 1, 1, 1);
}
