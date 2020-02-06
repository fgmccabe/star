cmake_minimum_required(VERSION 3.7)
project(star)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99 -fsanitize=address -fsanitize=leak")

include_directories("Headers" "../Headers" "../Util/Headers" "../Escapes/Headers" "../Infra/Headers")

set(HEADER_FILES
        ../Headers/version.h
        ../Headers/config.h
        ../Headers/instructions.h
        ../Util/Headers/ooio.h
        ../Util/Headers/cmdOptions.h
        ../Escapes/Headers/libEscapes.h
        ../Util/Headers/integer.h
        Headers/args.h
        Headers/bkpoint.h
        Headers/array.h Headers/arrayP.h
        Headers/cell.h Headers/cellP.h
        Headers/code.h Headers/codeP.h
        Headers/engine.h
        Headers/decode.h
        ../Infra/Headers/encoding.h Headers/decodeP.h
        ../Infra/Headers/streamDecode.h
        ../Infra/Headers/manifest.h ../Infra/Headers/manifestP.h ../Infra/Headers/pkg.h
        Headers/star.h
        Headers/globalsP.h
        Headers/opcodes.h
        ../Headers/starOptions.h
        Headers/globals.h
        Headers/heap.h Headers/heapP.h
        Headers/lockvar.h Headers/lockvarP.h
        Headers/term.h Headers/termP.h Headers/str.h Headers/arith.h
        Headers/debug.h
        Headers/tpl.h Headers/engineP.h
        Headers/local.h Headers/engineOptions.h
        Headers/labels.h Headers/labelsP.h
        Headers/arithP.h Headers/strP.h
        Headers/struct.h Headers/structP.h
        Headers/thr.h Headers/thrP.h
        Headers/repoP.h Headers/repo.h
        Headers/rrb.h Headers/rrbP.h
        Headers/verify.h Headers/verifyP.h Headers/srcMap.h Headers/srcMapP.h Headers/debugP.h Headers/capability.h Headers/capabilityP.h Headers/memo.h Headers/memoP.h)

set(SOURCE_FILES
        decode.c debug.c eval.c engine.c engineOptions.c gc.c globals.c heap.c load.c main.c
        ${HEADER_FILES} code.c term.c labels.c arith.c repo.c rrb.c
        str.c array.c lockvar.c cell.c iochnl.c thr.c bkpoint.c verify.c srcMap.c capability.c memo.c boot.c)

add_executable(star ${SOURCE_FILES} ${HEADER_FILES})

target_link_libraries(star escapes infra ooio m pthread)

install(TARGETS star DESTINATION bin)
