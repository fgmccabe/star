cmake_minimum_required(VERSION 3.7)
project(Fibers)

# Implement fibers on X64

include_directories("Headers" "../Headers" "../../Headers")

set(HEADER_FILES
        ../Headers/fiber.h
        ../Headers/buddy.h
        fiberP.h)

set(SOURCE_FILES
        fiber.c)

add_library(fiber STATIC ${SOURCE_FILES})
target_link_libraries(fiber m)

