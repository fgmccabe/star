set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("Headers" "../Headers" "../Util/Headers" "../Engine/Headers" "../Infra/Headers" "../Jit/Headers")

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
        Headers/fileops.h
        Headers/netfile.h
        Headers/stringops.h
        Headers/datentime.h
        Headers/threds.h
        Headers/lblops.h
        Headers/pkgops.h
        Headers/listops.h
        Headers/stkops.h Headers/futureops.h
        timeout.h)

set(SOURCE_FILES
        ${HEADER_FILES}
        assigns.c
        bigintops.c
        clock.c
        datentime.c
        fileops.c
        fltops.c
        intops.c
        ioops.c
        libEscapes.c
        system.c
        netfile.c
        pkgops.c
        stkops.c
        stringops.c
        threds.c
        unicode.c fiberops.c
        timeout.c
        futops.c
        outops.c
        jitcompile.c)

add_library(escapes STATIC ${SOURCE_FILES})
