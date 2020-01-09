cmake_minimum_required(VERSION 3.7)
project(OOIO)

find_package(BISON)
find_package(FLEX)

BISON_TARGET(JsonParser json.y ${CMAKE_CURRENT_BINARY_DIR}/jsonParser.c)
FLEX_TARGET(JsonTokeniser jtoken.l ${CMAKE_CURRENT_BINARY_DIR}/jsontoken.c)
ADD_FLEX_BISON_DEPENDENCY(JsonTokeniser JsonParser)

add_subdirectory("UnicodeStuff")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")
SET(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} -lpthread -lm")

include_directories("../Headers" "Headers")

set(UNITABLE.C ${CMAKE_CURRENT_BINARY_DIR}/UnicodeStuff/unitable.c)
set(UNICODE ${CMAKE_CURRENT_SOURCE_DIR}/UnicodeStuff/UnicodeData.txt)

set(PUBLIC_HEADERS
        Headers/io.h Headers/file.h Headers/formio.h
        Headers/hash.h Headers/hosts.h Headers/integer.h
        Headers/ioTcp.h Headers/iterate.h Headers/ixTree.h Headers/jsonEvent.h
        Headers/cons.h Headers/lockable.h Headers/logical.h
        Headers/managed.h Headers/object.h
        Headers/pair.h
        Headers/pool.h Headers/retcode.h Headers/pipe.h
        Headers/stringBuffer.h
        Headers/timer.h
        Headers/unistr.h Headers/utils.h
        Headers/jsonEvent.h Headers/udp.h Headers/ooio.h
        Headers/cmdOptions.h Headers/template.h Headers/pp.h Headers/ppP.h
        Headers/strng.h Headers/editline.h)

set(HEADER_FILES
        Headers/ioP.h Headers/fileP.h Headers/formioP.h
        Headers/hostsP.h
        Headers/iosockP.h Headers/ixTreeP.h
        Headers/consP.h Headers/lockableP.h
        Headers/managedP.h Headers/objectP.h
        Headers/pairP.h
        Headers/pipeP.h
        Headers/stringBufferP.h
        Headers/unistrP.h
        Headers/pp.h
        Headers/vector.h
        Headers/vectorP.h
        Headers/integerP.h
        Headers/strngP.h
        Headers/regexpP.h Headers/regexp.h)

set(SOURCE_FILES
        class.c
        file.c
        formin.c
        formout.c
        hash.c
        hosts.c
        io.c
        ixTree.c
        cons.c
        locked.c
        managed.c
        object.c
        cmdOptions.c
        pair.c
        pipe.c
        pool.c
        sockets.c
        stringBuffer.c
        template.c
        timer.c
        udp.c
        unichar.c
        unistr.c
        utils.c

        json.y
        jtoken.l

        ${UNITABLE.C}

        ${BISON_JsonParser_OUTPUTS}
        ${FLEX_JsonTokeniser_OUTPUTS}

        ${PUBLIC_HEADERS}
        ${HEADER_FILES}
        vector.c integer.c strng.c editline.c regexp.c)

add_library(ooio STATIC ${SOURCE_FILES})

add_custom_command(OUTPUT ${UNITABLE.C} DEPENDS parseUni ${UNICODE}
  COMMAND ${CMAKE_CURRENT_BINARY_DIR}/UnicodeStuff/parseUni ${UNICODE} ${UNITABLE.C}
  COMMENT "regenerating unitable.c ${UNITABLE.C}")

 
