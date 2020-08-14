cmake_minimum_required(VERSION 3.1)
project(tsc VERSION 1.0 LANGUAGES C)

add_library(
	tsc

	src/tsc.c
	src/tsc.h
	src/spirv.h)

add_executable(tscc src/tscc.c)

target_link_libraries(tscc PRIVATE tsc)
