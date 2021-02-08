// Texture2DArray my_texture;
// Texture2D my_textures_fixed[2];
SamplerState my_sampler;
Texture2D array_texture;

const float3 positions[3];

float4 main() : SV_Target
{
    float4 color;
    {
        float hey = 1.0f;
        color.x = hey;
    }
    {
        float hey = 2.0f;
        color.y = hey;
    }

    float3 my_vector = {1, 2, 3};
    float my_array[2] = {1, 2};
    float my_floats[] = {2, 2, 3};

    // float4 color = my_textures[0].Sample(my_sampler, float2(0, 0));
    // float4 color = my_textures_fixed[1].Sample(my_sampler, float2(0, 0));
    // return color;
    // array_texture.Sample(my_sampler, float2(0, 0), int2(0, 0));
    return float4(positions[2], color.x);
}
