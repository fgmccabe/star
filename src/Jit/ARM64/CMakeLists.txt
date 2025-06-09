cmake_minimum_required(VERSION 3.20)
project(arm64)

add_subdirectory(Assem)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers"
        "../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Infra/Headers"
        "../../Escapes/Headers"
        "../../Headers"
        "Common/Headers"
        "Assem/Headers"
        "./Headers")

set(HEADER_FILES
        arm64.h
        arm64P.h
        Headers/lower.h
        Headers/lowerP.h
        ../../Util/Headers/array.h
        ../../Engine/Headers/escapeP.h
        Assem/Headers/assem_encode.h
        Headers/macros.h
        ../Headers/infra.h
        Headers/asminfra.h
)

set(SOURCE_FILES
        lower.c)

add_library(ARM64 STATIC ${SOURCE_FILES})
target_link_libraries(ARM64 assem)


