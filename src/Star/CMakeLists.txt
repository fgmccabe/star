cmake_minimum_required(VERSION 3.7)
project(bootlib)

set(TEMP_REPO "${CMAKE_BINARY_DIR}/.temp-repo/")

add_custom_target(boot ALL)
add_dependencies(boot sbc)

file(MAKE_DIRECTORY ${TEMP_REPO})
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${TEMP_REPO}/*")

add_custom_command(TARGET boot
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler
  COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../BootCompiler/sbc" "-w" "${CMAKE_CURRENT_SOURCE_DIR}/" "-r" "${TEMP_REPO}" "star"
  )
