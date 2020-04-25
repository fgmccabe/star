cmake_minimum_required(VERSION 3.7)
project(compiler)

add_custom_target(compiler)
add_dependencies(compiler sbc boot star.operators star.escapes)

set(REPO_DIR "${CMAKE_BINARY_DIR}/.star-repo/")

add_custom_command(TARGET compiler DEPENDS sbc star.escapes
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../BootCompiler/sbc" "-w" "${CMAKE_CURRENT_SOURCE_DIR}/" "-r" "${REPO_DIR}" "star.compiler"
  )

set(STAR ${CMAKE_BINARY_DIR}/src/engine/star)
set(COMPILER_REPOSITORY ${REPO_DIR})
set(STAR_COMPILER_FLAGS "-h256m -s2m -ds -p5")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/sc.in ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sc)
file(COPY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sc
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ
  GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

add_custom_target(sc)




