cmake_minimum_required(VERSION 3.5)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../Headers" "Headers" "../Util/Headers" "../Engine/Headers" "../Infra/Headers")

set(HEADER_FILES
        Headers/libEscapes.h
        Headers/clock.h
        ../Engine/Headers/engine.h
        ../Engine/Headers/escodes.h
        ../Headers/escapes.h
        Headers/errorCodes.h
        Headers/arithmetic.h
        Headers/unicode.h
        ../Engine/Headers/iochnnl.h ../Engine/Headers/iochnnlP.h
        Headers/assigns.h
        Headers/ioops.h
        Headers/locks.h
        Headers/fileops.h
        Headers/netfile.h
        Headers/stringops.h
        Headers/datentime.h
        Headers/threds.h
        Headers/lblops.h
        Headers/pkgops.h
        Headers/turm.h
        Headers/listops.h
        )

set(SOURCE_FILES
        ${HEADER_FILES}
        libEscapes.c
        clock.c
        system.c arithmetic.c unicode.c assigns.c
        ioops.c locks.c fileops.c netfile.c stringops.c datentime.c threds.c lblops.c turm.c pkgops.c listops.c )

add_library(escapes STATIC ${SOURCE_FILES})
