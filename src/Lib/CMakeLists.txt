cmake_minimum_required(VERSION 3.5)
project(escapes)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../Headers" "Headers" "../Util/Headers" "../Engine/Headers")

set(HEADER_FILES
        Headers/escape.h
        Headers/clock.h
        ../Engine/Headers/engine.h
        ../Engine/Headers/escodes.h
        ../Headers/escapes.h
        Headers/args.h
        Headers/arithmetic.h)

set(SOURCE_FILES
        ${HEADER_FILES}
        esc.c
        clock.c
        show.c
        system.c arithmetic.c Headers/errorCodes.h)

add_library(escapes STATIC ${SOURCE_FILES})

install(FILES ${HEADER_FILES} DESTINATION include)
