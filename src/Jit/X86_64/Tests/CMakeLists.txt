cmake_minimum_required(VERSION 3.5)
project(x86_64)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers"
        "../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Headers")

set(HEADER_FILES
        x86_64.h
        x86_64P.h
        )

set(TEST_SOURCE_FILES unitTests.c x64Tests.c)
set(TEST_HEADER_FILES x86_64.h x86_64P.h unitTests.h)

set(SOURCE_FILES assemx86_64.c lower.c)

add_library(X86_64 STATIC ${SOURCE_FILES})

add_executable(x64Test ${TEST_SOURCE_FILES} ${TEST_HEADER_FILES})
target_link_libraries(x64Test X86_64 ooio m)


