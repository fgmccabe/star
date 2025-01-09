cmake_minimum_required(VERSION 3.20)
project(x86_64lib)

# add_subdirectory(Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("Headers"
        "../Headers"
        "../../Util/Headers"
        "../../Headers")

set(HEADER_FILES
        Headers/arm64.h
        Headers/arm64P.h
)

set(SOURCE_FILES
        jumpbuff.c)

add_library(X86_64LIB STATIC ${SOURCE_FILES})
