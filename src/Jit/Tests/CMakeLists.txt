cmake_minimum_required(VERSION 3.20)
project(shuffle_Tests)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("./Headers"
        "../Headers"
        "../../Engine/Headers"
        "../../Util/Headers"
        "../../Headers")

set(TEST_SOURCE_FILES ../../Util/unitTests.c
        sortTests.c
        ../regsort.c
        testInfra.c
)
set(TEST_HEADER_FILES
        ../../Util/Headers/unitTests.h
        ../Headers/jitP.h
        ../Headers/infra.h
        Headers/sortTests.h
        ./Headers/macros.h)

add_executable(shuffleTest ${TEST_SOURCE_FILES} ${TEST_HEADER_FILES})
target_link_libraries(shuffleTest ooio)

add_custom_target(testShuffle ALL DEPENDS shuffleTest
        COMMAND shuffleTest)


