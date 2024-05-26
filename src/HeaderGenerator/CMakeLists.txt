cmake_minimum_required(VERSION 3.5)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Engine/Headers" "../Util/Headers" "../Infra/Headers")

set(OPERATORS ../Headers/operators.h)

add_executable(ops genoperators.c stringTrie.c stringTrie.h stringTrieP.h genoperators.h
        ../Headers/operators.h)
target_link_libraries(ops ooio infra)

set(ESC_SOURCE_FILES genescapes.c ../Headers/escapes.h ../Headers/intrinsics.h ../Headers/signature.h)
add_executable(genesc ${ESC_SOURCE_FILES})
target_link_libraries(genesc ooio)

set(ASM_SOURCE_FILES plmnem.c)
add_executable(plmnem ${ASM_SOURCE_FILES})
target_link_libraries(plmnem ooio infra)

set(STAR_ASM_SOURCE_FILES starmnem.c)
add_executable(starmnem ${STAR_ASM_SOURCE_FILES})
target_link_libraries(starmnem ooio infra)

set(INTRINSIC_SOURCE_FILES plateinstrinsic.c)
add_executable(intrinsic ${INTRINSIC_SOURCE_FILES})
target_link_libraries(intrinsic ooio infra)

set(OPERATORS_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/operators.star)
set(ESCAPES_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/escapes.star)
set(INTRINSIC_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/intrinsics.star)
set(OPERATORS_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/operators.pl)
set(ESCAPES_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/escapes.pl)
set(INTRINSIC_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/intrinsics.pl)
set(INTRINSIC_ST_PLATE ${CMAKE_CURRENT_SOURCE_DIR}/intrinsics.star.plate)
set(INTRINSIC_PL_PLATE ${CMAKE_CURRENT_SOURCE_DIR}/intrinsics.pl.plate)
set(ASM_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/assem.pl)
set(ASM_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/assem.star)
set(OPCODES_H ${CMAKE_CURRENT_SOURCE_DIR}/../Engine/Headers/opcdes.h)
set(OPERATORS_TEXI ${CMAKE_CURRENT_SOURCE_DIR}/../../Docs/Reference/operators.texi)
set(OPERATORS_EL ${CMAKE_CURRENT_SOURCE_DIR}/../Emacs/star-ops.el)

add_custom_command(OUTPUT operators DEPENDS ops
        COMMAND ops -i -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.texi.plate  ${OPERATORS_TEXI}
        COMMAND ops -e -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.el.plate  ${OPERATORS_EL}
        )

add_custom_target(pl.operators DEPENDS ops
        COMMAND ops -p -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.pl.plate ${OPERATORS_PL})

add_custom_command(OUTPUT ${OPERATORS_ST} DEPENDS ops ${OPERATORS}
        COMMAND ops -s -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.star.plate  ${OPERATORS_ST}
        COMMENT "Creating star operators in  ${OPERATORS_ST}"
        )

add_custom_target(star.operators DEPENDS ops
        COMMAND ops -s -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.star.plate  ${OPERATORS_ST})

add_custom_target(star.escapes DEPENDS genesc ${ESCAPES_ST})

add_custom_target(pl.escapes DEPENDS genesc -p ${ESCAPES_PL})

add_custom_target(star.intrinsics DEPENDS intrinsic
        COMMAND intrinsic -s -t ${INTRINSIC_ST_PLATE} ${INTRINSIC_ST}
        )

add_custom_target(pl.intrinsics DEPENDS intrinsic
        COMMAND intrinsic -p -t ${INTRINSIC_PL_PLATE} ${INTRINSIC_PL}
        )
add_custom_target(pl.memnonics DEPENDS plmnem
        COMMAND plmnem -t ${CMAKE_CURRENT_SOURCE_DIR}/assem.pl.plate ${ASM_PL}
        COMMENT "Creating star assembler in ${ASM_PL}"
        )

add_custom_target(star.memnonics DEPENDS starmnem
        COMMAND starmnem -t ${CMAKE_CURRENT_SOURCE_DIR}/assem.star.plate ${ASM_ST}
        COMMENT "Creating star assembler in ${ASM_ST}"
        )

add_custom_target(generate DEPENDS star.operators star.escapes star.intrinsics star.memnonics pl.escapes pl.operators pl.intrinsics)

