struct MaterialBuffer
{
    float4 color;
    uint albedo_texture_index;
    uint albedo_sampler_index;
};

struct ModelBuffer
{
    float4x4 transform;
};

struct PushConstant {
    uint model_buffer_index;
    uint model_index;
    uint material_buffer_index;
    uint material_index;
};

[[vk::binding(0)]] StructuredBuffer<ModelBuffer> model_buffers[];
[[vk::binding(0)]] StructuredBuffer<MaterialBuffer> material_buffers[];
[[vk::binding(1)]] Texture2D<float4> textures[];
[[vk::binding(2)]] SamplerState samplers[];

[[vk::push_constant]] PushConstant pc;

float4 main() : SV_Target {
    ModelBuffer model = model_buffers[pc.model_buffer_index][pc.model_buffer_index];
    MaterialBuffer material = material_buffers[pc.material_buffer_index][pc.material_index];

	Texture2D<float4> my_texture = textures[material.albedo_texture_index];
	SamplerState my_sampler = samplers[material.albedo_sampler_index];

	my_texture = textures[material.albedo_texture_index+1];

	return my_texture.Sample(my_sampler, float2(1, 1));
}
