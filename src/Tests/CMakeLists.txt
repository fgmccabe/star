cmake_minimum_required(VERSION 3.7)
project(units)

enable_testing()

set(REPO_DIR "${CMAKE_CURRENT_BINARY_DIR}/Build/")

add_test(NAME c WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND star -r ${REPO_DIR} -b test.c -m test.c@init)
