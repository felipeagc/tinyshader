#ifndef INCLUDED_HLSL
#define INCLUDED_HLSL

struct IncludedStruct {

};

struct StructB
{
    float c;
};

struct StructA
{
    StructB c;
    float3 vec;
};

struct Hello
{
    uint a;
    StructA c;
};

#endif
