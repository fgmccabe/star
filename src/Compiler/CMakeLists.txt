cmake_minimum_required(VERSION 3.7)
project(compiler)

add_custom_target(compiler ALL)
add_dependencies(boot sbc)

set(REPO_DIR "${CMAKE_BINARY_DIR}/.star-repo/")

file(MAKE_DIRECTORY ${REPO_DIR})
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${REPO_DIR}")

add_custom_command(TARGET compiler DEPENDS sbc star.escapes
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler
  COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../BootCompiler/sbc" "-w" "${CMAKE_CURRENT_SOURCE_DIR}/" "-r" "${REPO_DIR}" "star.compiler"
  )

add_dependencies(compiler star.operators star.escapes)


