cmake_minimum_required(VERSION 3.20)
project(assem_x64)

add_subdirectory(Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories(
        "../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Headers"
        "../../../Headers"
        "../../../../Headers"
        "./Headers")

set(HEADER_FILES
        x86_64.h
        x86_64P.h
        assem.h
        macros.h
        config.h)

set(SOURCE_FILES assemx86_64.c ins_x64.c macros.c)
add_library(assem STATIC ${SOURCE_FILES})

