cmake_minimum_required(VERSION 3.1)
project(tinyshader VERSION 1.0 LANGUAGES C)

add_library(
	tinyshader

	tinyshader/tinyshader.h
	tinyshader/tinyshader_internal.h
	tinyshader/tinyshader.c
	tinyshader/tinyshader_misc.c
	tinyshader/tinyshader_parser.c
	tinyshader/tinyshader_ir.c
	tinyshader/tinyshader_analysis.c
	tinyshader/spirv.h)

if (MSVC)
else()
	target_compile_options(tinyshader PRIVATE -Wall -Wextra -Wno-unused-function)
endif()

target_include_directories(tinyshader PUBLIC tinyshader)

add_executable(tsc tsc/tsc.c)
target_link_libraries(tsc PRIVATE tinyshader)
