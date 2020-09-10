@echo off

pushd examples

cmake --build ..\build

del *.spv

..\build\Debug\tsc.exe -E vertex -T vertex -o vertex.spv test.hlsl
..\build\Debug\tsc.exe -E pixel -T fragment -o frag.spv test.hlsl

spirv-val vertex.spv
spirv-val frag.spv

spirv-dis vertex.spv
spirv-dis frag.spv

popd
