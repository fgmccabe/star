cmake_minimum_required(VERSION 3.7)
project(bootlib)

add_custom_target(boot ALL)

set(BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/Build/")

add_custom_command(TARGET boot
        COMMAND "echo" "Hello world ${BUILD_DIR}")
