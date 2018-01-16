cmake_minimum_required(VERSION 3.5)
project(gen)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Engine/Headers" "../Util/Headers" "../Infra/Headers")

set(SOURCE_FILES opcodes.c ../Headers/instructions.h)

add_executable(opcodes ${SOURCE_FILES})

add_executable(ops genoperators.c trie.c ../Headers/operators.h trie.h trieP.h)
target_link_libraries(ops ooio infra)

set(ESC_SOURCE_FILES genescapes.c ../Headers/escapes.h ../Headers/signature.h)
add_executable(genesc ${ESC_SOURCE_FILES})

target_link_libraries(genesc ooio)

set(INS_SOURCE_FILES instr.c ../Headers/instructions.h)
add_executable(genins ${INS_SOURCE_FILES})

target_link_libraries(genins ooio)

set(ASM_SOURCE_FILES mnem.c)
add_executable(mnem ${ASM_SOURCE_FILES})
target_link_libraries(mnem ooio)

set(OPS_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/../Engine/Headers/ops.h)
#set(ASM_LO ${CMAKE_CURRENT_SOURCE_DIR}/../LOCompiler/asm.lo)
#set(INSTRUCTIONS_LO ${CMAKE_CURRENT_SOURCE_DIR}/../LOCompiler/instructions.lo)
set(OPERATORS_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/operators.star)
set(ESCAPES_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/escapes.star)
set(OPERATORS_PY ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/operators.py)
set(OPERATORS_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/operators.pl)
set(ESCAPES_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/escapes.pl)

add_custom_target(generate ALL opcodes ${OPS_HEADER}
        COMMAND opcodes ${OPS_HEADER}
#        COMMAND mnem ${ASM_LO}
#        COMMAND genins ${INSTRUCTIONS_LO}
#        COMMAND ops -c lo.comp.operators ${OPERATORS_ST}
#        COMMAND ops -y -t starops.py.plate ${OPERATORS_PY}
         COMMAND ops -p -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.pl.plate ${OPERATORS_PL}
#        COMMAND genesc -c lo.comp.escapes ${ESCAPES_ST}
        COMMAND genesc -p ${ESCAPES_PL}
        )
