struct Hello {
    float4 my_vec;
};

[[vk::binding(0, 0)]] ConstantBuffer<Hello> hey;
[[vk::push_constant]] Hello pc;

float4 main() : SV_Target {
    return pc.my_vec;
}
