project(infra)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("Headers" "../Headers" "../Util/Headers" "../Escapes/Headers" "../Engine/Headers" "../Jit/Headers")

set(HEADER_FILES
        Headers/manifest.h Headers/manifestP.h Headers/pkg.h Headers/pkgP.h Headers/encoding.h
        ../Headers/signature.h
        Headers/formexts.h
        ../Util/Headers/signals.h
        ../Engine/Headers/engineOptions.h
        Headers/streamDecode.h
        Headers/streamDecodeP.h timers.c Headers/timers.h)

set(SOURCE_FILES ${HEADER_FILES} manifest.c
        pkg.c
        encode.c
        signature.c
        streamDecode.c
        timers.c)

add_library(infra ${SOURCE_FILES} ${HEADER_FILES})
