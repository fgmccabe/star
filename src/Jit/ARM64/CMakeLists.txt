cmake_minimum_required(VERSION 3.5)
project(x86_64)

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
        )

set(SOURCE_FILES assem_arm64.c ins_arm.c lower.c )
add_library(X86_64 STATIC ${SOURCE_FILES})




