cmake_minimum_required(VERSION 3.5)
project(jit)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Util/Headers" "../Escapes/Headers" "../Engine/Headers")

set(HEADER_FILES
        Headers/jit.h Headers/jitP.h
        Headers/x64Ops.h
        )

set(SOURCE_FILES jitInfra.c)

add_library(jit ${SOURCE_FILES} ${HEADER_FILES})
