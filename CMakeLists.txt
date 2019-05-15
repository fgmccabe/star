cmake_minimum_required(VERSION 3.7)
project(Star)

include(./emacs.cmake)

add_subdirectory("src")
add_subdirectory("Docs")

add_emacs(star-util star star-config star-indent star-repo star-ops star-shell)

