cmake_minimum_required(VERSION 3.7)
project(bootlib)

add_custom_target(boot ALL)

set(REPO_DIR "${CMAKE_CURRENT_BINARY_DIR}/Build/")

add_custom_command(TARGET boot
        COMMAND ${CMAKE_COMMAND} -E make_directory ${REPO_DIR}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler
        COMMAND "${CMAKE_CURRENT_BINARY_DIR}/../BootCompiler/sbc" "-w" "${CMAKE_CURRENT_SOURCE_DIR}/" "-r" "${REPO_DIR}" "star.boot"
      )

