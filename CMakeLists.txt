cmake_minimum_required(VERSION 3.1)
project(tinyshader VERSION 1.0 LANGUAGES C)

add_library(
  tinyshader

  tinyshader/tinyshader.h
  tinyshader/tinyshader_internal.h
  tinyshader/tinyshader.c
  tinyshader/tinyshader_misc.c
  tinyshader/tinyshader_lexer.c
  tinyshader/tinyshader_preprocess.c
  tinyshader/tinyshader_parser.c
  tinyshader/tinyshader_ir.c
  tinyshader/tinyshader_ast_ir.c
  tinyshader/tinyshader_analysis.c
  tinyshader/spirv.h)
target_include_directories(tinyshader PUBLIC tinyshader)

add_executable(tsc tsc/tsc.c)
target_link_libraries(tsc PRIVATE tinyshader)

if (NOT MSVC)
  target_compile_options(
    tinyshader
    PUBLIC
    -Wall -Wextra -Wno-unused-function
    -fsanitize=undefined
  )
  target_link_options(
    tinyshader
    PUBLIC
    -fsanitize=undefined
  )
endif()
