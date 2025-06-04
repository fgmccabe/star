project(units)

set(STARBUILD "${CMAKE_BINARY_DIR}")
set(STARCOMPILER "${CMAKE_BINARY_DIR}/src/Compiler/sc")
set(TESTDIR "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/runtests.in
  ${CMAKE_CURRENT_BINARY_DIR}/runtests.py)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/testRunner.in
  ${CMAKE_CURRENT_BINARY_DIR}/testRunner.py)

add_custom_target(unit DEPENDS ${STAREXEC} ${STARCOMPILER}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND python ${CMAKE_CURRENT_BINARY_DIR}/testRunner.py --tester=scx-test --all)

