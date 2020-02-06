cmake_minimum_required(VERSION 3.7)
project(ASM)

set(BSDLIB "")
if (APPLE)
    set(BSDLIB "")
    set(BUILD_OS "apple")
elseif (UNIX)
    set(BUILD_OS "unix")
endif (APPLE)

BISON_TARGET(AsmGrammar asmGrammar.y ${CMAKE_CURRENT_BINARY_DIR}/asmGrammar.c)
FLEX_TARGET(AsmTokeniser asmToken.l ${CMAKE_CURRENT_BINARY_DIR}/asmToken.c)
ADD_FLEX_BISON_DEPENDENCY(AsmTokeniser AsmGrammar)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lpthread -lm")

include_directories("../Headers" "Headers" "../Engine/Headers" "../Util/Headers" "../Infra/Headers")

set(HEADER_FILES
        Headers/asm.h
        Headers/asmOptions.h
        Headers/assem.h
        Headers/assemP.h
        Headers/errors.h

        ../Headers/instructions.h
        ../Infra/Headers/encoding.h
        ../Infra/Headers/manifest.h

        ../Util/Headers/io.h)

set(SOURCE_FILES
        asmGrammar.y
        asmToken.l

        ${BISON_AsmGrammar_OUTPUTS}
        ${FLEX_AsmTokeniser_OUTPUTS}

        assem.c
        asmOptions.c
        display.c
        driver.c
        asmEncode.c
        errors.c
        asm.c

        ${HEADER_FILES})

add_executable(sasm ${SOURCE_FILES})
target_link_libraries(sasm infra ooio)

install(TARGETS sasm DESTINATION lib/star)
