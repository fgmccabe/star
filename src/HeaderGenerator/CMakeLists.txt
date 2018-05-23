cmake_minimum_required(VERSION 3.5)
project(gen)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Engine/Headers" "../Util/Headers" "../Infra/Headers")

add_executable(ops genoperators.c trie.c trie.h trieP.h parseOperators.c parseOperators.h genoperators.h)
target_link_libraries(ops ooio infra)

set(ESC_SOURCE_FILES genescapes.c ../Headers/escapes.h ../Headers/signature.h)
add_executable(genesc ${ESC_SOURCE_FILES})

target_link_libraries(genesc ooio)

# set(INS_SOURCE_FILES instr.c ../Headers/instructions.h)
# add_executable(genins ${INS_SOURCE_FILES})

# target_link_libraries(genins ooio)

set(ASM_SOURCE_FILES plmnem.c)
add_executable(plmnem ${ASM_SOURCE_FILES})
target_link_libraries(plmnem ooio)

set(OPCODES_FILES opcodes.c)
add_executable(opcodes ${OPCODES_FILES})
target_link_libraries(opcodes ooio)


#set(ASM_LO ${CMAKE_CURRENT_SOURCE_DIR}/../LOCompiler/asm.lo)
#set(INSTRUCTIONS_LO ${CMAKE_CURRENT_SOURCE_DIR}/../LOCompiler/instructions.lo)
set(OPERATORS_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/operators.star)
set(ESCAPES_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/escapes.star)
set(OPERATORS_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/operators.pl)
set(ESCAPES_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/escapes.pl)
set(ASM_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/assem.pl)
set(OPCODES_H ${CMAKE_CURRENT_SOURCE_DIR}/../Engine/Headers/opcdes.h)

add_custom_target(generate ALL
#        COMMAND genins ${INSTRUCTIONS_LO}
#        COMMAND ops -c lo.comp.operators ${OPERATORS_ST}
#        COMMAND ops -y -t starops.py.plate ${OPERATORS_PY}
         COMMAND ops -p -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.pl.plate -o ${CMAKE_CURRENT_SOURCE_DIR}/../Headers/operators.json ${OPERATORS_PL}
#        COMMAND genesc -c lo.comp.escapes ${ESCAPES_ST}
        COMMAND genesc -p ${ESCAPES_PL}
        COMMAND plmnem -p ${ASM_PL}
#        COMMAND opcodes ${OPCODES_H}
        )
