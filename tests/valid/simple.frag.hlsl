Texture2D my_textures[];
Texture2D my_textures_fixed[2];
SamplerState my_sampler;

float4 main() : SV_Target
{
    float my_floats[2];
    my_floats[0] = 1.0f;

    float4 color = my_textures[0].Sample(my_sampler, float2(0, 0));
    color = my_textures_fixed[1].Sample(my_sampler, float2(0, 0));
    return color;
}
