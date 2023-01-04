cmake_minimum_required(VERSION 3.5)
project(lightning)

# add_subdirectory(Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers"
        "../Headers"
        "../../Util/Headers"
        "../../Engine/Headers"
        "../../Headers"
        "./Headers"
        "/usr/local/lib")

set(HEADER_FILES
        Headers/lower.h
        Headers/lowerP.h
        lightning.h
        )

set(SOURCE_FILES lower.c )
add_library(Lightning STATIC ${SOURCE_FILES})




