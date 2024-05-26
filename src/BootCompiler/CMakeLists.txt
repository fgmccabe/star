cmake_minimum_required(VERSION 3.7)
project(sbc)

set(BOOT_COMPILER_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
find_program(SWIPL swipl)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/sbc.in ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sbc)
file(COPY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sbc
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ
  GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

set(HDR ${CMAKE_CURRENT_SOURCE_DIR}/../HeaderGenerator)

add_custom_target(sbc)
add_dependencies(sbc pl.operators pl.escapes pl.intrinsics pl.memnonics)


