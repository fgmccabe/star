cmake_minimum_required(VERSION 3.20)
project(compiler)

set(REPO_DIR "${CMAKE_BINARY_DIR}/.star-repo/")
set(TEMP_REPO "${CMAKE_BINARY_DIR}/.temp-repo/")

set(TEMP_MANIFEST ${TEMP_REPO}/manifest)
set(MANIFEST ${REPO_DIR}/manifest)

file(MAKE_DIRECTORY ${TEMP_REPO})
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${TEMP_REPO}")

file(MAKE_DIRECTORY ${REPO_DIR})
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${REPO_DIR}/*")

set(SBC "${CMAKE_CURRENT_BINARY_DIR}/../BootCompiler/sbc")
set(SC "${STAR}" "${STAR_COMPILER_FLAGS}" "-r" "${TEMP_REPO}/" "star.compiler")
set(STAR ${CMAKE_CURRENT_BINARY_DIR}/../Engine/star)

file(GLOB COMPILER_DEPS
        CONFIGURE_DEPENDS
        LIST_DIRECTORIES false
        ${CMAKE_CURRENT_SOURCE_DIR}/*.star)

add_custom_command(OUTPUT ${TEMP_MANIFEST}
        DEPENDS ${COMPILER_DEPS}
        COMMENT "Build into ${TEMP_REPO}"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND ${SBC} "-w" "${CMAKE_CURRENT_SOURCE_DIR}/" "-r" "${TEMP_REPO}" "star.compiler"
  )

add_custom_command(OUTPUT ${MANIFEST}
  DEPENDS ${COMPILER_DEPS} ${STAR} ${TEMP_MANIFEST}
  COMMAND ${STAR} "-r" "${TEMP_REPO}" "-h2g" "star.compiler" "-R" "${REPO_DIR}/" "-W" "${CMAKE_CURRENT_SOURCE_DIR}/" "star.compiler"
  )

add_custom_target(compiler
  DEPENDS ${MANIFEST} ${COMPILER_DEPS})

#add_dependencies(compiler star.memnonics star.operators star.escapes star.intrinsics)

set(STAR ${CMAKE_BINARY_DIR}/src/Engine/star)
set(INSTALLED_STAR ${CMAKE_INSTALL_PREFIX}/bin/star)
set(INSTALLED_STAR_SHARE ${CMAKE_INSTALL_PREFIX}/share/star)
set(INSTALLED_STAR_REPO ${INSTALLED_STAR_SHARE}/.star-repo)
set(COMPILER_REPOSITORY ${REPO_DIR})
set(STAR_COMPILER_FLAGS "-h2g -p1")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/sc.in ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sc)
file(COPY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sc
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  FILE_PERMISSIONS OWNER_READ OWNER_EXECUTE GROUP_READ
  GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

add_custom_target(sc)

install(PROGRAMS  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/sc DESTINATION bin)
install(DIRECTORY ${CMAKE_BINARY_DIR}/.star-repo DESTINATION ${INSTALLED_STAR_SHARE})
  
	





