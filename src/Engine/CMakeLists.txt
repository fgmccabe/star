cmake_minimum_required(VERSION 3.5)
project(cafe)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Util/Headers" "../Lib/Headers" "../Infra/Headers")

set(HEADER_FILES
        ../Headers/version.h
        ../Headers/config.h
        ../Headers/instructions.h
        ../Util/Headers/ooio.h
        ../Util/Headers/options.h
        ../Lib/Headers/escape.h
        Headers/code.h Headers/codeP.h
        Headers/engine.h
        Headers/decode.h ../Infra/Headers/encoding.h Headers/decodeP.h
        ../Infra/Headers/manifest.h ../Infra/Headers/manifestP.h ../Infra/Headers/pkg.h
        Headers/cafe.h
        Headers/opcodes.h Headers/ops.h
        ../Headers/cafeOptions.h
        Headers/heap.h Headers/heapP.h
        Headers/term.h Headers/termP.h Headers/str.h Headers/strP.h Headers/arith.h Headers/arithP.h
        Headers/debug.h)

set(SOURCE_FILES boot.c decode.c debug.c eval.c engine.c engineOptions.c gc.c heap.c load.c main.c Headers/tpl.h)

add_executable(cafe ${SOURCE_FILES} ${HEADER_FILES})

target_link_libraries(cafe ooio escapes)
