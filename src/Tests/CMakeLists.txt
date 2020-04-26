cmake_minimum_required(VERSION 3.7)
project(units)

set(REPODIR "${CMAKE_BINARY_DIR}/.star-repo/")
set(STAREXEC "${CMAKE_BINARY_DIR}/src/Engine/star")
set(STARBOOTCOMPILER "${CMAKE_BINARY_DIR}/src/BootCompiler/sbc")
set(STARCOMPILER "${CMAKE_BINARY_DIR}/src/Compiler/sc")
set(TESTDIR "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/runtests.in
  ${CMAKE_CURRENT_BINARY_DIR}/runtests.py)

add_custom_target(unit DEPENDS ${STAREXEC} ${STARCOMPILER} boot
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND python ${CMAKE_CURRENT_BINARY_DIR}/runtests.py --all)

