cmake_minimum_required(VERSION 3.20)
project(assem)

add_subdirectory(Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers"
        "../Headers"
        "../../../Util/Headers"
        "../../../Infra/Headers"
        "../../../Engine/Headers"
        "../../../Escapes/Headers"
        "../../../Headers"
        "../../Headers"
        "../../Common/Headers")

set(HEADER_FILES
        arm64.h
        arm64P.h
        Headers/assem_encode.h
        ../../Util/Headers/array.h
        ../../Escapes/Headers/libEscapes.h
        assem_encode.h
        Headers/macros.h
        ../Headers/infra.h
        ../Common/Headers/asminfra.h
        Headers/disass.h
)

set(SOURCE_FILES
        assem_arm64.c
        assem_encode.c
        macros.c
        asmInfra.c
        disass.c)

add_library(assem STATIC ${SOURCE_FILES})


