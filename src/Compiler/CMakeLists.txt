cmake_minimum_required(VERSION 3.7)
project(compiler)

add_custom_target(compiler)
add_dependencies(compiler sbc star.operators star.escapes sc)

set(REPO_DIR "${CMAKE_BINARY_DIR}/.star-repo/")
set(TEMP_REPO "${CMAKE_BINARY_DIR}/.temp-repo/")

file(MAKE_DIRECTORY ${TEMP_REPO})
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${TEMP_REPO}")

file(MAKE_DIRECTORY ${REPO_DIR})
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${REPO_DIR}/*")

add_custom_command(TARGET compiler DEPENDS sbc star.escapes
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../BootCompiler/sbc" "-w" "${CMAKE_CURRENT_SOURCE_DIR}/" "-r" "${TEMP_REPO}" "star.compiler"
  )

set(STAR ${CMAKE_BINARY_DIR}/src/Engine/star)
set(INSTALLED_STAR ${CMAKE_INSTALL_PREFIX}/bin/star)
set(INSTALLED_STAR_SHARE ${CMAKE_INSTALL_PREFIX}/share/star)
set(INSTALLED_STAR_REPO ${INSTALLED_STAR_SHARE}/.star-repo)
set(COMPILER_REPOSITORY ${REPO_DIR})
set(STAR_COMPILER_FLAGS "-h2g -s16m -p5")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/sc.in ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sc)
file(COPY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sc
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  FILE_PERMISSIONS OWNER_READ OWNER_EXECUTE GROUP_READ
  GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

add_custom_target(sc)
add_dependencies(sc sbc star)

add_custom_command(OUTPUT sc
        COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../BootCompiler/sbc" "-w" "${CMAKE_CURRENT_SOURCE_DIR}/" "-r" "${TEMP_REPO}/" "star.compiler"
        COMMAND "${STAR}" "${STAR_COMPILER_FLAGS}" "-r" "${TEMP_REPO}/" "star.compiler" "-R" "${REPO_DIR}/" "-W" "${CMAKE_CURRENT_SOURCE_DIR}/" "star.compiler"
	)

install(PROGRAMS  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sc DESTINATION bin)
Install(DIRECTORY ${CMAKE_BINARY_DIR}/.star-repo DESTINATION ${INSTALLED_STAR_SHARE})
  
	





