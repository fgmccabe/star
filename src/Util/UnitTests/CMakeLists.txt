cmake_minimum_required(VERSION 3.7)
project(Util_UnitTests)

include(../findAIO.cmake)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")
# SET(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} -lpthread -lm")

include_directories("../../Headers" "../Headers" "Headers")

set(TEST_HEADER_FILES
        Headers/buddyTest.h
        Headers/multiTests.h
        Headers/matchTests.h
        Headers/timertests.h)

set(TEST_SOURCE_FILES
        buddyTest.c
        utilTests.c
        ../unitTests.c
        multiTest.c miscTest.c Headers/miscTest.h matchTests.c
        timers.c)

add_executable(utilTest EXCLUDE_FROM_ALL ${TEST_SOURCE_FILES} ${TEST_HEADER_FILES})
target_link_libraries(utilTest ooio m)

add_executable(aioTest aioTest.c)
target_link_libraries(aioTest ${AIO_LIBRARIES})
      
#add_custom_target(testUtils ALL DEPENDS utilTest
#        COMMAND utilTest)
