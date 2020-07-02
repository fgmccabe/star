cmake_minimum_required(VERSION 3.5)
project(jit)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

string(TOUPPER "${CMAKE_HOST_SYSTEM_PROCESSOR}" JIT_ARCH)

include_directories("Headers"
        "../Headers"
        "../Util/Headers"
        "../Escapes/Headers"
        "../Engine/Headers"
        "../Infra/Headers"
        "${JIT_ARCH}/Headers")

add_subdirectory(${JIT_ARCH})

set(HEADER_FILES
        Headers/jit.h Headers/jitP.h ${JIT_ARCH}/jitOps.h
        )

set(SOURCE_FILES jit.c jitInfra.c)

add_library(jit STATIC ${SOURCE_FILES})
