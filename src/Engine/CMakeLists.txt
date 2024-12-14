cmake_minimum_required(VERSION 3.7)
project(star)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("Headers" "../Headers" "../Util/Headers" "../Escapes/Headers" "../Infra/Headers" "../Jit/Headers")
add_subdirectory("EngineTests")

set(HEADER_FILES
	Headers/ideal.h
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
        ../Headers/starOptions.h
        Headers/globals.h
        Headers/heap.h Headers/heapP.h
        Headers/term.h Headers/termP.h Headers/strings.h Headers/arith.h
        Headers/debug.h
        Headers/tpl.h Headers/engineP.h
        Headers/local.h Headers/engineOptions.h
        Headers/labels.h Headers/labelsP.h
        Headers/arithP.h Headers/stringsP.h
        Headers/thr.h Headers/thrP.h
        Headers/verify.h Headers/verifyP.h
        Headers/debugP.h
        Headers/consP.h Headers/option.h Headers/optionP.h
        Headers/starP.h Headers/stackP.h
        Headers/stack.h
        Headers/char.h Headers/charP.h Headers/bignumP.h Headers/closure.h Headers/closureP.h
        Headers/normal.h
        Headers/normalP.h
        Headers/singleP.h
        Headers/single.h
		Headers/future.h
        Headers/futureP.h
        Headers/vect.h
        Headers/vectP.h
        Headers/escape.h
        Headers/either.h
        Headers/eitherP.h
)

set(SOURCE_FILES
	arith.c
	bignum.c
	bkpoint.c
	cell.c
        char.c
	closure.c
	code.c
	cons.c
        decode.c
	debug.c
        either.c
	eval.c
	engine.c
	engineOptions.c
        future.c
	gc.c
	globals.c
	heap.c
	ideal.c
	iochnl.c
	labels.c
	load.c
	ltype.c
	main.c
	option.c
        single.c
	stack.c
        strings.c
	term.c
        thr.c
	verify.c
        vect.c
)

add_executable(star ${SOURCE_FILES} ${HEADER_FILES})

target_link_libraries(star escapes infra ooio jit)

install(TARGETS star DESTINATION bin)
