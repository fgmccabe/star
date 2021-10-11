cmake_minimum_required(VERSION 3.5)
project(x86_64_Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("../Headers"
        "../../Headers"
        "../../../Util/Headers"
        "../../../Engine/Headers"
        "../../../Headers"
        "../")

set(TEST_SOURCE_FILES ../../../Util/unitTests.c x64Tests.c ../../jitinfra.c)
set(TEST_HEADER_FILES ../Headers/x86_64.h ../Headers/x86_64P.h ../../../Util/Headers/unitTests.h ../../Headers/jitP.h)

add_executable(x64Test EXCLUDE_FROM_ALL ${TEST_SOURCE_FILES} ${TEST_HEADER_FILES})
target_link_libraries(x64Test X86_64 ooio m)

add_custom_target(testX86 ALL DEPENDS x64Test
        COMMAND x64Test)


