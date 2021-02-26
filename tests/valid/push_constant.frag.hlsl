struct MaterialBuffer
{
    float4 color;
    uint albedo_texture_index;
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

[[vk::binding(0, 0)]] StructuredBuffer<ModelBuffer> model_buffers[];
[[vk::binding(0, 0)]] StructuredBuffer<MaterialBuffer> material_buffers[];
[[vk::push_constant]] PushConstant pc;

float4 main() : SV_Target {
    ModelBuffer model = model_buffers[pc.model_buffer_index][pc.model_buffer_index];
    MaterialBuffer material = material_buffers[pc.material_buffer_index][pc.material_index];
    return material.color;
}
