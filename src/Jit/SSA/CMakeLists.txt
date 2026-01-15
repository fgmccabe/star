cmake_minimum_required(VERSION 3.20)
project(ssa)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("./Headers"
        "../Headers"
        "../../Headers"
        "../../Util/Headers"
        "../../Escapes/Headers"
        "../../Engine/Headers"
        "../../Infra/Headers"
        "../Analysis/Headers")

set(SSA_HEADER_FILES
        Headers/ssaInstructions.h
        Headers/ssaOpcodes.h
        Headers/ssa.h
        ../Analysis/Headers/analyse.h
        ../Analysis/Headers/analyseP.h
)

set(SSA_SOURCE_FILES
        ssa.c)

add_library(ssa STATIC ${SSA_SOURCE_FILES})
target_link_libraries(ssa analysis ooio)


