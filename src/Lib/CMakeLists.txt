cmake_minimum_required(VERSION 3.5)
project(escapes)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../Headers" "Headers" "../Util/Headers" "../Engine/Headers" "../Infra/Headers")

set(HEADER_FILES
        Headers/libEscapes.h
        Headers/clock.h
        ../Engine/Headers/engine.h
        ../Engine/Headers/escodes.h
        ../Headers/escapes.h
        Headers/args.h
        Headers/errorCodes.h
        Headers/arithmetic.h
        Headers/unicode.h
        ../Engine/Headers/iochnnl.h ../Engine/Headers/iochnnlP.h
        )

set(SOURCE_FILES
        ${HEADER_FILES}
        libEscapes.c
        clock.c
        show.c
        system.c arithmetic.c unicode.c assigns.c Headers/assigns.h ioops.c Headers/ioops.h locks.c Headers/locks.h fileops.c Headers/fileops.h netfile.c Headers/netfile.h stringops.c Headers/stringops.h datentime.c Headers/datentime.h threds.c Headers/threds.h lblops.c Headers/lblops.h turm.c Headers/turm.h pkgops.c Headers/pkgops.h)

add_library(escapes STATIC ${SOURCE_FILES})

install(FILES ${HEADER_FILES} DESTINATION include)
