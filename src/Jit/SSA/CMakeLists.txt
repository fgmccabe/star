cmake_minimum_required(VERSION 3.20)
project(ssa)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("./Headers"
        "../Headers"
        "../../Headers"
        "../../Util/Headers"
        "../../Escapes/Headers"
        "../../Engine/Headers"
        "../../Infra/Headers")

set(HEADER_FILES
        Headers/ssa.h
        Headers/ssaP.h
        )

set(SSA_SOURCE_FILES split.c ssaInfra.c)

add_library(ssa STATIC ${SSA_SOURCE_FILES})


