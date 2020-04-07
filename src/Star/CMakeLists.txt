cmake_minimum_required(VERSION 3.7)
project(bootlib)

set(REPO_DIR "${CMAKE_BINARY_DIR}/.star-repo/")

add_custom_target(boot ALL)
add_dependencies(boot sbc)

file(MAKE_DIRECTORY ${REPO_DIR})
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${REPO_DIR}")

add_custom_command(TARGET boot
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler
  COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../BootCompiler/sbc" "-w" "${CMAKE_CURRENT_SOURCE_DIR}/" "-r" "${REPO_DIR}" "star"
  )
