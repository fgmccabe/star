cmake_minimum_required(VERSION 3.15)
project(units)

find_package(Python COMPONENTS Interpreter REQUIRED)

if(Python_FOUND)
  message(STATUS "Python interpreter found: ${Python_EXECUTABLE}")
else()
  message(FATAL_ERROR "Python not found")
endif()


set(STARBUILD "${CMAKE_BINARY_DIR}")
set(STARCOMPILER "${CMAKE_BINARY_DIR}/src/Compiler/sc")
set(TESTDIR "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/runtests.in
  ${CMAKE_CURRENT_BINARY_DIR}/runtests.py)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/testRunner.in
  ${CMAKE_CURRENT_BINARY_DIR}/testRunner.py)

add_custom_target(unit DEPENDS ${STAREXEC} ${STARCOMPILER}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/testRunner.py --tester=${TESTDIR}/sc-test --all)

