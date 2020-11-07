struct BufType
{
    int i;
    float f;
};

StructuredBuffer<BufType> Buffer0;

groupshared uint a;

float4 myfunc(float4 aa) {
    return aa;
}

[numthreads(1, 1, 1)]
void main(
    in uint3 dtid : SV_DispatchThreadID,
    in uint3 gpid: SV_GroupID,
    in uint gpindex: SV_GroupIndex,
    in uint3 gpthreadid: SV_GroupThreadID
) {
    AllMemoryBarrier();
    AllMemoryBarrierWithGroupSync();
    DeviceMemoryBarrier();
    DeviceMemoryBarrierWithGroupSync();
    GroupMemoryBarrier();
    GroupMemoryBarrierWithGroupSync();
    float f1 = Buffer0[0].f + float(a);
    InterlockedAdd(a, 123);
    InterlockedAnd(a, 123);
    InterlockedMin(a, 123);
    InterlockedMax(a, 123);
    InterlockedOr(a, 123);
    InterlockedXor(a, 123);
    uint original;
    InterlockedExchange(a, 123, original);
    InterlockedCompareExchange(a, 123, 1, original);
    InterlockedCompareStore(a, 123, 1);

    // uint b = a;
    float3 c = float3(1, 2, 3);
    uint3 d = asuint(c);
    float3 e = asfloat(c);
    int3 f = asint(e);
    int3 g = asint(f);

    float2x4 view;
    view[0][3] = 0.0;
    view[1][3] = 0.0;
    view[1][3] = false ? 0.0 : 2.0e-1;
    view[1][3] = fmod(2.3, 1.2);

    float4 f4 = float4(c, 1);
    f4 += 4.0;
    f4.y += -4.1231f;
    f4 = myfunc(4.0f);
    f4 = 1;

    if ((dtid.x == 1 && dtid.y == 2) || 3) {
        AllMemoryBarrier();
    }

    bool my_bool = 1.0f;
    if (0.0f && my_bool) {
        AllMemoryBarrier();
    }


    // uint2 my_data_dims;
    // Buffer0.GetDimensions(my_data_dims.x, my_data_dims.y);
    // BufType my_data = Buffer0[0];
}
