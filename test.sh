#!/bin/bash

pushd ../examples

rm -f *.spv

cmake --build ../build

../build/tsc -E vertex -T vertex -o vert.spv test.hlsl &&\
../build/tsc -E pixel -T fragment -o frag.spv test.hlsl &&\
../build/tsc -E main -T compute -o comp.spv test_compute.hlsl &&\
	spirv-dis vert.spv &&\
	spirv-val vert.spv &&\
	spirv-dis frag.spv &&\
	spirv-val frag.spv &&\
	spirv-dis comp.spv &&\
	spirv-val comp.spv
# od --width=4 -Ad -H a.spv

popd
