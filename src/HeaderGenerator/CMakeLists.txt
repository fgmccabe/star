cmake_minimum_required(VERSION 3.5)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Engine/Headers" "../Util/Headers" "../Infra/Headers")

add_executable(ops genoperators.c trie.c trie.h trieP.h parseOperators.c parseOperators.h genoperators.h)
target_link_libraries(ops ooio infra)

set(ESC_SOURCE_FILES genescapes.c ../Headers/escapes.h ../Headers/intrinsics.h ../Headers/signature.h)
add_executable(genesc ${ESC_SOURCE_FILES})

target_link_libraries(genesc ooio)

set(ASM_SOURCE_FILES platemnem.c)
add_executable(plmnem ${ASM_SOURCE_FILES})
target_link_libraries(plmnem ooio infra)

set(OPCODES_FILES opcodes.c)
add_executable(opcodes ${OPCODES_FILES})
target_link_libraries(opcodes ooio)

set(OPERATORS_JSON "${CMAKE_CURRENT_SOURCE_DIR}/../Headers/operators.json")
set(OPERATORS_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/operators.star)
set(ESCAPES_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/escapes.star)
set(OPERATORS_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/operators.pl)
set(ESCAPES_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/escapes.pl)
set(ASM_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/assem.pl)
set(ASM_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/assem.star)
set(OPCODES_H ${CMAKE_CURRENT_SOURCE_DIR}/../Engine/Headers/opcdes.h)
set(OPERATORS_TEXI ${CMAKE_CURRENT_SOURCE_DIR}/../../Docs/Reference/operators.texi)
set(OPERATORS_EL ${CMAKE_SOURCE_DIR}/star-ops.el)

add_custom_command(OUTPUT operators DEPENDS ${OPERATORS_JSON}
  COMMAND ops -p -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.pl.plate -o ${CMAKE_CURRENT_SOURCE_DIR}/../Headers/operators.json ${OPERATORS_PL}
  COMMAND ops -i -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.texi.plate -o ${CMAKE_CURRENT_SOURCE_DIR}/../Headers/operators.json ${OPERATORS_TEXI}
  COMMAND ops -e -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.el.plate -o ${CMAKE_CURRENT_SOURCE_DIR}/../Headers/operators.json ${OPERATORS_EL}
  )

add_custom_command(OUTPUT ${OPERATORS_ST} DEPENDS ops ${OPERATORS_JSON}
  COMMAND ops -s -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.star.plate -o ${CMAKE_CURRENT_SOURCE_DIR}/../Headers/operators.json ${OPERATORS_ST}
  COMMENT "Creating star operators in  ${OPERATORS_ST}"
)  

add_custom_target(star.operators DEPENDS ops ${OPERATORS_JSON}
  COMMAND ops -s -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.star.plate -o ${CMAKE_CURRENT_SOURCE_DIR}/../Headers/operators.json ${OPERATORS_ST})

add_custom_target(star.escapes DEPENDS genesc ${ESCAPES_ST})

add_custom_command(OUTPUT ${ESCAPES_ST} DEPENDS genesc
  COMMAND genesc -s star.comp.escapes ${ESCAPES_ST}
  COMMENT "Creating star escapes"
)  

add_custom_target(generate ALL DEPENDS operators star.operators star.escapes
  COMMAND genesc -p ${ESCAPES_PL}
  COMMAND plmnem -t ${CMAKE_CURRENT_SOURCE_DIR}/assem.pl.plate -p ${ASM_PL}
  COMMAND plmnem -t ${CMAKE_CURRENT_SOURCE_DIR}/assem.star.plate -s ${ASM_ST}
  )
