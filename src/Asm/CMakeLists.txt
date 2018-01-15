cmake_minimum_required(VERSION 3.9)
project(ASM)

BISON_TARGET(AsmGrammar asmGrammar.y ${CMAKE_CURRENT_BINARY_DIR}/asmGrammar.c)
FLEX_TARGET(AsmTokeniser asmToken.l ${CMAKE_CURRENT_BINARY_DIR}/asmToken.c)
ADD_FLEX_BISON_DEPENDENCY(AsmTokeniser AsmGrammar)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../Headers" "Headers" "../Engine/Headers" "../Util/Headers" "../Infra/Headers" ${BISON_AsmGrammar_OUTPUTS})

set(HEADER_FILES
        Headers/asm.h
        Headers/asmOptions.h
        Headers/assem.h
        Headers/assemP.h
        Headers/errors.h

        ../Headers/instructions.h
        ../Infra/Headers/encoding.h
        ../Engine/Headers/ops.h
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
        main.c

        ${HEADER_FILES})

add_executable(asm ${SOURCE_FILES})
target_link_libraries(asm ooio infra)

install(TARGETS asm DESTINATION lib)
