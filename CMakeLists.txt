cmake_minimum_required(VERSION 3.7)
project(Star)

include(./emacs.cmake)

add_subdirectory("src/Util")
add_subdirectory("src/HeaderGenerator")
add_subdirectory("src/Escapes")
add_subdirectory("src/Asm")
add_subdirectory("src/Infra")
add_subdirectory("src/Engine")
add_subdirectory("src/Star")
add_subdirectory("src/Tests")

add_subdirectory("Docs")

add_emacs(star)



