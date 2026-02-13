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
        "../Analysis/Headers"
        "Common/Headers"
        "Assem/Headers")

set(HEADER_FILES
        Assem/Headers/arm64.h
        Assem/Headers/arm64P.h
        Assem/Headers/assem.h
        Headers/lower.h
        Headers/lowerAP.h
        Headers/lowerP.h
        ../../Util/Headers/array.h
        ../../Engine/Headers/escapeP.h
        Assem/Headers/assem_encode.h
        Headers/macros.h
        ../Headers/infra.h
        ../Analysis/Headers/analyse.h
        ../Analysis/Headers/analyseP.h
        Headers/shuffle.h
        Headers/lowerAP.h
)

set(SOURCE_FILES
        lower.c
 #       lowerA.c
        lowerStack.c
        lowerFrame.c
        lowerFFI.c
        shuffle.c)

add_library(ARM64 STATIC ${SOURCE_FILES} ${HEADER_FILES})
target_link_libraries(ARM64 assem ssa)


