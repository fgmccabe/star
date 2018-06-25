cmake_minimum_required(VERSION 3.7)
project(OOIO)

find_package(BISON)
find_package(FLEX)

BISON_TARGET(JsonParser json.y ${CMAKE_CURRENT_BINARY_DIR}/jsonParser.c)
FLEX_TARGET(JsonTokeniser jtoken.l ${CMAKE_CURRENT_BINARY_DIR}/jsontoken.c)
ADD_FLEX_BISON_DEPENDENCY(JsonTokeniser JsonParser)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")
SET(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} -lpthread -lm")


include_directories("../Headers" "Headers")

set(PUBLIC_HEADERS
        Headers/base64.h Headers/io.h Headers/file.h Headers/formio.h
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
        Headers/cmdOptions.h Headers/template.h Headers/pp.h Headers/ppP.h)

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
        )

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
        unitable.c
        utils.c

        json.y
        jtoken.l

        ${BISON_JsonParser_OUTPUTS}
        ${FLEX_JsonTokeniser_OUTPUTS}

        base64.c
        ${PUBLIC_HEADERS}
        ${HEADER_FILES})

add_library(ooio STATIC ${SOURCE_FILES})

install(TARGETS ooio DESTINATION lib)
install(FILES ${PUBLIC_HEADERS} DESTINATION include/star)
