cmake_minimum_required(VERSION 3.20)
project(arm64)

add_subdirectory(Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers"
        "../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Escapes/Headers"
        "../../Headers"
        "../Common/Headers"
        "./Headers")

set(HEADER_FILES
        Headers/arm64.h
        Headers/arm64P.h
        Headers/lower.h
        Headers/lowerP.h
        ../../Util/Headers/array.h
        ../../Escapes/Headers/libEscapes.h
        assem_encode.h
        Headers/macros.h
        ../Headers/infra.h
        ../Common/Headers/asminfra.h
)

set(SOURCE_FILES
        assem_arm64.c
        lower.c
        assem_encode.c
        macros.c
        ../Common/asmInfra.c)

add_library(ARM64 STATIC ${SOURCE_FILES})


