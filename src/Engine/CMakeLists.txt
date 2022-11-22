cmake_minimum_required(VERSION 3.7)
project(star)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99 -fsanitize=address -fsanitize=leak")

include_directories("Headers" "../Headers" "../Util/Headers" "../Escapes/Headers" "../Infra/Headers" "../Jit/Headers")
add_subdirectory("EngineTests")

set(HEADER_FILES
        ../Headers/version.h
        ../Headers/config.h
        ../Headers/instructions.h
        ../Util/Headers/ooio.h
        ../Util/Headers/cmdOptions.h
        ../Escapes/Headers/libEscapes.h
        ../Util/Headers/integer.h
        ../Jit/Headers/jit.h
        Headers/args.h
        Headers/bignum.h Headers/bignumP.h
        Headers/bkpoint.h
        Headers/cell.h Headers/cellP.h
        Headers/code.h Headers/codeP.h
        Headers/cons.h
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
        Headers/term.h Headers/termP.h Headers/strings.h Headers/arith.h
        Headers/debug.h
        Headers/tpl.h Headers/engineP.h
        Headers/local.h Headers/engineOptions.h
        Headers/labels.h Headers/labelsP.h
        Headers/arithP.h Headers/stringsP.h
        Headers/struct.h Headers/structP.h
        Headers/thr.h Headers/thrP.h
        Headers/repoP.h Headers/repo.h
        Headers/verify.h Headers/verifyP.h
        Headers/debugP.h Headers/capability.h Headers/capabilityP.h Headers/asm.h
        Headers/codegen.h Headers/consP.h Headers/option.h Headers/optionP.h
        Headers/starP.h Headers/stackP.h
        Headers/stack.h Headers/futureP.h Headers/future.h Headers/thunkP.h
        Headers/char.h Headers/charP.h Headers/bignumP.h Headers/continuationP.h)

set(SOURCE_FILES
        decode.c debug.c eval.c engine.c engineOptions.c gc.c globals.c heap.c load.c main.c
        ${HEADER_FILES} code.c term.c labels.c arith.c repo.c
        strings.c lockvar.c cell.c iochnl.c thr.c bkpoint.c verify.c capability.c cons.c option.c ltype.c Headers/ltype.h stack.c
        future.c thunk.c Headers/thunk.h char.c bignum.c ideal.c ideal.h continuation.c Headers/continuation.h)

add_executable(star ${SOURCE_FILES} ${HEADER_FILES})

target_link_libraries(star escapes infra ooio m pthread jit)

install(TARGETS star DESTINATION bin)
