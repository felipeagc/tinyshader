struct BufType
{
    int i;
    float f;
};

StructuredBuffer<BufType> Buffer0;

groupshared uint a;

[numthreads(1, 1, 1)]
void main(
    in uint3 dtid : SV_DispatchThreadID,
    in uint3 gpid: SV_GroupID,
    in uint gpindex: SV_GroupIndex,
    in uint3 gpthreadid: SV_GroupThreadID
) {
    // AllMemoryBarrier();
    // AllMemoryBarrierWithGroupSync();
    // DeviceMemoryBarrier();
    // DeviceMemoryBarrierWithGroupSync();
    // GroupMemoryBarrier();
    // GroupMemoryBarrierWithGroupSync();
    // float f = Buffer0[0].f + float(a);
    // InterlockedAdd(a, 123);
    // InterlockedAnd(a, 123);
    // InterlockedMin(a, 123);
    // InterlockedMax(a, 123);
    // InterlockedOr(a, 123);
    // InterlockedXor(a, 123);
    // uint original;
    // InterlockedExchange(a, 123, original);
    // InterlockedCompareExchange(a, 123, 1, original);
    // InterlockedCompareStore(a, 123, 1);

    // uint b = a;
    float3 c = float3(1, 2, 3);
    uint3 d = asuint(c);
    float3 e = asfloat(c);
    int3 f = asint(e);
    int3 g = asint(f);
}
