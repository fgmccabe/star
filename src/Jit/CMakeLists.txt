cmake_minimum_required(VERSION 3.20)
project(jit)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

if (NOT DEFINED JIT_ARCH)
    string(TOUPPER "${CMAKE_HOST_SYSTEM_PROCESSOR}" JIT_ARCH)
endif ()

add_subdirectory(${JIT_ARCH})

include_directories("Headers"
        "../Headers"
        "../Util/Headers"
        "../Escapes/Headers"
        "../Engine/Headers"
        "../Infra/Headers"
        "Common/Headers"
        "${JIT_ARCH}/Headers")

set(HEADER_FILES
        Headers/jit.h
        Headers/jitP.h
        Headers/jitOps.h
        Common/Headers/asminfra.h
        ../Util/Headers/array.h
        ../Escapes/Headers/libEscapes.h
        ${JIT_ARCH}/Headers/lower.h
        )

set(JIT_SOURCE_FILES jit.c jitInfra.c Common/asmInfra.c
        Common/Headers/asminfra.h)

add_library(jit STATIC ${JIT_SOURCE_FILES})
target_link_libraries(jit ${JIT_ARCH})
