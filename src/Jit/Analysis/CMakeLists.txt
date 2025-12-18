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
        Headers/analyse.h
        Headers/analyseP.h
)

set(SSA_SOURCE_FILES analyse.c infra.c)

add_library(ssa STATIC ${SSA_SOURCE_FILES})


