#include "./included.hlsl"
#include "./included.hlsl"

#define SOME_CONST 1.0123123
#define \
HELLO 123 + 123 \
123 123123

#ifndef ASDASD
#else
dont compile this
#endif

#ifndef HELLO
dont compile this either
#endif

cbuffer gInput : register(b0) {
    float4 yoo;
    float hey;
    Hello hello;
    float4x4 transform;
    float4x4 view;
}

Texture2D gInput2;
uniform sampler gInput3;

struct VsOutput
{
    float4 pos : POSITION;
    float2 uv : TEXCOORD0;
};

float4 main(in VsOutput vs_out, out float4 out_color2 : SV_Target1) : SV_Target0
{
    for (; vs_out.pos.x > 0;)
    {
        if (vs_out.pos.x <= 123)
        {
            break;
        }
    }

    float4 my_color =
        gInput2.Sample(gInput3, vs_out.uv) *
        float4(ddx(1.0), 1, 1, 1) *
        float4(hey, hey, hey, hey);

    my_color = gInput2.SampleLevel(gInput3, vs_out.uv, 1);
    float width;
    float height;
    float mip_levels;
    gInput2.GetDimensions(0, width, height, mip_levels);

    float3 up = abs(vs_out.pos.x) < 0.999 ? float3(0.0, 0.0, 1.0) : float3(1.0, 0.0, 0.0);

    float4 color = my_color;
    color.r = up.z;
    color = float4(1, 1, 1, 1);

    out_color2 = color;

    return color;
}
