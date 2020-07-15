cmake_minimum_required(VERSION 3.5)
project(x86_64)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers"
        "../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Headers")

set(HEADER_FILES
        x86_64.h
        x86_64P.h
        )

set(SOURCE_FILES assemx86_64.c lower.c)

add_library(X86_64 STATIC ${SOURCE_FILES})

