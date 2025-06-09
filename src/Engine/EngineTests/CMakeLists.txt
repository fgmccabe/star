project(Engine_UnitTests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../../Headers" "../Headers" "Headers")

set(TEST_HEADER_FILES
        Headers/engineTests.h
        Headers/vectorTests.h
)

set(TEST_SOURCE_FILES
        engineTests.c
        ../../Util/unitTests.c
        ../heap.c
        ../char.c
        ../arith.c
        ../labels.c
        ../term.c
        ../vect.c
        ../cons.c
        ../ideal.c
        ../code.c
        vectorTests.c
        dummyGc.c)

add_executable(engineTest EXCLUDE_FROM_ALL ${TEST_SOURCE_FILES} ${TEST_HEADER_FILES})
target_link_libraries(engineTest ooio infra)

add_custom_target(testEngine ALL DEPENDS engineTest
        COMMAND engineTest)
