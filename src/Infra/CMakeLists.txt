cmake_minimum_required(VERSION 3.5)
project(infra)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Util/Headers" "../Lib/Headers")

set(HEADER_FILES
        Headers/manifest.h Headers/manifestP.h Headers/pkg.h Headers/pkgP.h Headers/encoding.h
        ../Headers/signature.h
        )

set(SOURCE_FILES ${HEADER_FILES} manifest.c pkg.c encode.c signature.c)

add_library(infra ${SOURCE_FILES} ${HEADER_FILES})
