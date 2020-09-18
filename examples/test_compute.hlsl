struct BufType
{
    int i;
    float f;
};

StructuredBuffer<BufType> Buffer0;

groupshared uint a;

[numthreads(1, 1, 1)]
void main()
{
    AllMemoryBarrier();
    AllMemoryBarrierWithGroupSync();
    DeviceMemoryBarrier();
    DeviceMemoryBarrierWithGroupSync();
    GroupMemoryBarrier();
    GroupMemoryBarrierWithGroupSync();
    float f = Buffer0[0].f + float(a);
    InterlockedAdd(a, 123);
    InterlockedAnd(a, 123);
    InterlockedMin(a, 123);
    InterlockedMax(a, 123);
    uint b = a;
}
