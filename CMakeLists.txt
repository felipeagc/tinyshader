cmake_minimum_required(VERSION 3.1)
project(tinyshader VERSION 1.0 LANGUAGES C)

add_library(
	tinyshader

	src/tinyshader.c
	src/tinyshader.h
	src/spirv.h)

target_compile_options(tinyshader PRIVATE -Wall -Wextra -Wno-unused-function)

add_executable(tsc src/tsc.c)

target_link_libraries(tsc PRIVATE tinyshader)
