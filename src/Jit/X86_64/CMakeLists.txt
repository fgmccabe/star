cmake_minimum_required(VERSION 3.20)
project(x86_64)

add_subdirectory(Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers"
        "../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Headers"
        "AsmInfra/Headers"
        "./Headers")

set(HEADER_FILES
        Headers/x86_64.h
        Headers/x86_64P.h
        Headers/lower.h
        Headers/lowerP.h
        )

set(SOURCE_FILES assemx86_64.c ins_x64.c ../ARM64/Assem/asmInfra.c lower.c )
add_library(X86_64 STATIC ${SOURCE_FILES})




