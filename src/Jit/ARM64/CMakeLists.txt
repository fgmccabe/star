cmake_minimum_required(VERSION 3.5)
project(arm64)

add_subdirectory(Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers"
        "../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Headers"
        "./Headers")

set(HEADER_FILES
        Headers/arm64.h
        Headers/arm64P.h
        Headers/lower.h
        Headers/lowerP.h
        ../../Util/Headers/array.h
        )

set(SOURCE_FILES assem_arm64.c lower.c assem_encode.c assem_encode.h)

add_library(ARM64 STATIC ${SOURCE_FILES})


