cmake_minimum_required(VERSION 3.20)
project(Unicode)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")
SET(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} -lm")

set(SOURCE_FILES parseUni.c)

set(UNICODE  ${CMAKE_CURRENT_SOURCE_DIR}/UnicodeData.txt)
set(UNITABLE ${CMAKE_CURRENT_SOURCE_DIR}/unitable.c)

add_executable(parseUni ${SOURCE_FILES})

add_custom_target(unitable DEPENDS parseUni
        COMMAND parseUni  ${UNICODE}  ${UNITABLE}
        COMMENT "Unicode table in ${UNITABLE}")
