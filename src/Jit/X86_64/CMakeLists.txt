cmake_minimum_required(VERSION 3.20)
project(x86_64)

add_subdirectory(Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories(
        "../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Headers"
        "./Headers"
        "Assem/Headers")

set(HEADER_FILES
        Assem/Headers/x86_64.h
        Assem/Headers/x86_64P.h
        Headers/lower.h
        Headers/lowerP.h
        Headers/macros.h
        )

set(SOURCE_FILES Assem/assemx86_64.c Assem/ins_x64.c ../ARM64/Assem/asmInfra.c lower.c )
add_library(X86_64 STATIC ${SOURCE_FILES})




