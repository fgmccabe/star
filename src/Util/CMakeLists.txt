cmake_minimum_required(VERSION 3.7)
project(OOIO)

include(./findAIO.cmake)

find_package(BISON 3.2)
find_package(FLEX)

add_subdirectory("UnicodeStuff")
add_subdirectory("UnitTests")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../Headers" "Headers")

set(UNITABLE.C ${CMAKE_CURRENT_BINARY_DIR}/unitable.c)
set(UNICODE ${CMAKE_CURRENT_SOURCE_DIR}/UnicodeStuff/UnicodeData.txt)

set(PUBLIC_HEADERS
        Headers/array.h
        Headers/io.h
        Headers/file.h
        Headers/formio.h
        Headers/hash.h
        Headers/hosts.h
        Headers/integer.h
        Headers/sock.h
        Headers/jsonEvent.h
        Headers/lockable.h
        Headers/logical.h
        Headers/managed.h
        Headers/object.h
        Headers/pool.h
        Headers/retcode.h
        Headers/pipe.h
        Headers/byteBuffer.h
        Headers/wordBuffer.h
        Headers/stringBuffer.h
        Headers/timer.h
        Headers/unistr.h
        Headers/utils.h
        Headers/jsonEvent.h
        Headers/ooio.h
        Headers/cmdOptions.h
        Headers/template.h
        Headers/strng.h
        Headers/editline.h
        Headers/buddy.h
        Headers/base64.h
        Headers/multi.h
        Headers/signals.h)

set(HEADER_FILES
        Headers/arrayP.h
        Headers/ioP.h
        Headers/fileP.h
        Headers/formioP.h
        Headers/hostsP.h
        Headers/sockP.h
        Headers/lockableP.h
        Headers/managedP.h
        Headers/objectP.h
        Headers/pipeP.h
        Headers/byteBufferP.h
        Headers/wordBufferP.h
        Headers/stringBufferP.h
        Headers/topSort.h
        Headers/unistrP.h
        Headers/vector.h
        Headers/vectorP.h
        Headers/integerP.h
        Headers/strngP.h
        Headers/topSortP.h
        Headers/buddyP.h
        Headers/multi.h
        Headers/formatted.h
        Headers/multiP.h
        Headers/match.h
        Headers/quick.h
        Headers/byteBufferP.h)

set(SOURCE_FILES
        arrays.c
        buddy.c
        base64.c
        class.c
        cmdOptions.c
        editline.c
        file.c
        formatted.c
        formin.c
        formout.c
        hash.c
        hosts.c
        integer.c
        io.c
        formexts.c
        locked.c
        managed.c
        match.c
        multi.c
        object.c
        pipe.c
        pool.c
        quick.c
        signal.c
        sockets.c
        wordBuffer.c
        stringBuffer.c
        strng.c
        template.c
        timer.c
        topSort.c
        unichar.c
        unistr.c
        utils.c
        vector.c

        ${UNITABLE.C}

#        ${BISON_JsonParser_OUTPUTS}
#        ${FLEX_JsonTokeniser_OUTPUTS}

        ${PUBLIC_HEADERS}
        ${HEADER_FILES}
        byteBuffer.c)

add_custom_command(OUTPUT ${UNITABLE.C} DEPENDS parseUni ${UNICODE}
        COMMAND ${CMAKE_CURRENT_BINARY_DIR}/UnicodeStuff/parseUni ${UNICODE} ${UNITABLE.C}
        COMMENT "regenerating unitable.c ${UNITABLE.C}")

add_library(ooio STATIC ${SOURCE_FILES})
target_link_libraries(ooio pthread m ${AIO_LIBRARIES})


 
