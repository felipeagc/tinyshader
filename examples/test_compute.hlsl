struct BufType
{
    int i;
    float f;
};

StructuredBuffer<BufType> Buffer0;

[numthreads(1, 1, 1)]
void main()
{
    AllMemoryBarrier();
    AllMemoryBarrierWithGroupSync();
    DeviceMemoryBarrier();
    DeviceMemoryBarrierWithGroupSync();
    GroupMemoryBarrier();
    GroupMemoryBarrierWithGroupSync();
    float f = Buffer0[0].f;
}
