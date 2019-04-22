cmake_minimum_required(VERSION 3.7)
project(units)

set(REPODIR "${CMAKE_BINARY_DIR}/.star-repo/")
set(STAREXEC "${CMAKE_BINARY_DIR}/src/Engine/star")
set(STARCOMPILER "${CMAKE_BINARY_DIR}/src/BootCompiler/sbc")
set(TESTDIR "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/runtests.in
  ${CMAKE_CURRENT_BINARY_DIR}/runtests.py)

add_custom_target(unit
  COMMAND python ${CURRENT_BINARY_DIR}/runtests.py --all)

