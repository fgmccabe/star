cmake_minimum_required(VERSION 3.20)
project(x86_64)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories(
        "./Headers"
        "../Headers"
        "../../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Escapes/Headers"
        "../../Infra/Headers"
        "./Assem/Headers")

set(HEADER_FILES
        assem.h
        x86_64.h
        lower.h
        lowerP.h
        macros.h
        config.h
        )

set(SOURCE_FILES lower.c lowerFFI.c lowerFrame.c lowerStack.c)
add_library(X86_64 STATIC ${SOURCE_FILES})
target_link_libraries(X86_64 assem)




