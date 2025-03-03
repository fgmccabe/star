cmake_minimum_required(VERSION 3.20)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("Headers" "../Headers" "../Engine/Headers" "../Util/Headers" "../Infra/Headers")

set(OPERATORS ../Headers/operators.h)

add_executable(ops genoperators.c stringTrie.c stringTrie.h stringTrieP.h genoperators.h
        ../Headers/operators.h)
target_link_libraries(ops ooio infra)

set(PLESC_SOURCE_FILES
        ../Headers/escapes.h ../Headers/signature.h
        plescapes.c
        plsig.c
        plsig.h
        plescapes.h)
add_executable(genplesc ${PLESC_SOURCE_FILES})
target_link_libraries(genplesc ooio)

set(STARESC_SOURCE_FILES
        ../Headers/escapes.h ../Headers/signature.h
        starescapes.c
        starsig.c
        starsig.h
        starescapes.h)
add_executable(genstaresc ${STARESC_SOURCE_FILES})
target_link_libraries(genstaresc ooio)

set(OPCODES_SOURCE_FILES opcodes.c ../Headers/instructions.h)
add_executable(opcodes ${OPCODES_SOURCE_FILES})
target_link_libraries(opcodes ooio)

set(OPCODES_H ${CMAKE_CURRENT_SOURCE_DIR}/../Headers/opcodes.h)
set(OPCODES_PLATE ${CMAKE_CURRENT_SOURCE_DIR}/opcodes.plate)

add_custom_target(opcodes.h DEPENDS opcodes
        COMMAND opcodes -t ${OPCODES_PLATE} ${OPCODES_H}
        COMMENT "Opcodes header in ${OPCODES_H}")

set(ASM_SOURCE_FILES plmnem.c)
add_executable(plmnem ${ASM_SOURCE_FILES})
target_link_libraries(plmnem ooio infra)

set(STAR_ASM_SOURCE_FILES starmnem.c )
add_executable(starmnem ${STAR_ASM_SOURCE_FILES})
target_link_libraries(starmnem ooio infra)

set(INTRINSIC_SOURCE_FILES plateinstrinsic.c
        starsig.h)
add_executable(intrinsic ${INTRINSIC_SOURCE_FILES})
target_link_libraries(intrinsic ooio infra)

set(OPERATORS_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/operators.star)
set(ESCAPES_ST_PLATE ${CMAKE_CURRENT_SOURCE_DIR}/escapes.star.plate)
set(ESCAPES_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/escapes.star)
set(INTRINSIC_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/intrinsics.star)
set(OPERATORS_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/operators.pl)
set(ESCAPES_PL_PLATE ${CMAKE_CURRENT_SOURCE_DIR}/escapes.pl.plate)
set(ESCAPES_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/escapes.pl)
set(INTRINSIC_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/intrinsics.pl)
set(INTRINSIC_ST_PLATE ${CMAKE_CURRENT_SOURCE_DIR}/intrinsics.star.plate)
set(INTRINSIC_PL_PLATE ${CMAKE_CURRENT_SOURCE_DIR}/intrinsics.pl.plate)
set(ASM_PL ${CMAKE_CURRENT_SOURCE_DIR}/../BootCompiler/assem.pl)
set(ASM_ST ${CMAKE_CURRENT_SOURCE_DIR}/../Compiler/assem.star)
set(OPERATORS_TEXI ${CMAKE_CURRENT_SOURCE_DIR}/../../Docs/Reference/operators.texi)
set(OPERATORS_EL ${CMAKE_CURRENT_SOURCE_DIR}/../Emacs/star-ops.el)

add_custom_target(texi.operators DEPENDS ops
        COMMAND ops -i -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.texi.plate ${OPERATORS_TEXI}
)

add_custom_target(el.operators DEPENDS ops
        COMMAND ops -e -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.el.plate ${OPERATORS_EL}
)

add_custom_target(pl.operators DEPENDS ops
        COMMAND ops -p -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.pl.plate ${OPERATORS_PL})

add_custom_command(OUTPUT ${OPERATORS_ST} DEPENDS ops ${OPERATORS}
        COMMAND ops -s -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.star.plate ${OPERATORS_ST}
        COMMENT "Creating star operators in  ${OPERATORS_ST}"
)

add_custom_target(star.operators DEPENDS ops
        COMMAND ops -s -t ${CMAKE_CURRENT_SOURCE_DIR}/starops.star.plate ${OPERATORS_ST})

add_custom_target(star.escapes DEPENDS genstaresc ${ESCAPES_ST}
        COMMAND genstaresc -t ${ESCAPES_ST_PLATE} ${ESCAPES_ST})

add_custom_target(pl.escapes DEPENDS genplesc ${ESCAPES_PL}
        COMMAND genplesc -t ${ESCAPES_PL_PLATE} ${ESCAPES_PL}
)

add_custom_target(star.intrinsics DEPENDS intrinsic
        COMMAND intrinsic -s -t ${INTRINSIC_ST_PLATE} ${INTRINSIC_ST}
)

add_custom_target(pl.intrinsics DEPENDS intrinsic
        COMMAND intrinsic -p -t ${INTRINSIC_PL_PLATE} ${INTRINSIC_PL}
)
add_custom_target(pl.memnonics DEPENDS plmnem
        COMMAND plmnem -t ${CMAKE_CURRENT_SOURCE_DIR}/assem.pl.plate ${ASM_PL}
        COMMENT "Creating boot assembler in ${ASM_PL}"
)

add_custom_target(star.memnonics DEPENDS starmnem
        COMMAND starmnem -t ${CMAKE_CURRENT_SOURCE_DIR}/assem.star.plate ${ASM_ST}
        COMMENT "Creating star assembler in ${ASM_ST}"
)

add_custom_target(generate DEPENDS star.operators star.escapes star.intrinsics star.memnonics
        pl.operators pl.escapes pl.intrinsics pl.memnonics
        el.operators texi.operators)

