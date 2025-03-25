cmake_minimum_required(VERSION 3.20)
project(arm_Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("../../Headers"
        "../../../Headers"
        "../../../../Util/Headers"
        "../../../../Engine/Headers"
        "../../../../Escapes/Headers"
        "../../../../Headers"
        "../../../Common/Headers"
        "../Headers"
        "../.."
        "..")

set(TEST_SOURCE_FILES ../../../../Util/unitTests.c
        ../assem_arm64.c
        ../assem_encode.c
        arm64Tests.c
        ../asmInfra.c
        ../macros.c
        test_infra.h
        test_infra.c
        armRegSetTest.c
        )
set(TEST_HEADER_FILES ../Headers/arm64.h
        ../Headers/arm64P.h
        ../Headers/assem_encode.h
        ../../../../Util/Headers/unitTests.h
        ../../../Headers/jitP.h
        ../../Headers/asminfra.h
        armRegSetTest.h)

add_executable(arm64Test EXCLUDE_FROM_ALL ${TEST_SOURCE_FILES} ${TEST_HEADER_FILES})
target_link_libraries(arm64Test ooio)

add_custom_target(test64Arm ALL DEPENDS arm64Test
        COMMAND arm64Test)


