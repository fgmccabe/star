cmake_minimum_required(VERSION 3.7)
project(compiler)

add_custom_target(compiler)
add_dependencies(compiler sbc boot star.operators star.escapes)

set(REPO_DIR "${CMAKE_BINARY_DIR}/.star-repo/")

add_custom_command(TARGET compiler DEPENDS sbc star.escapes
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler
  COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../BootCompiler/sbc" "-w" "${CMAKE_CURRENT_SOURCE_DIR}/" "-r" "${REPO_DIR}" "star.compiler"
  )




