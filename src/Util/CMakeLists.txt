cmake_minimum_required(VERSION 3.5)
project(OOIO)

find_package(BISON)
find_package(FLEX)

BISON_TARGET(JsonParser json.y ${CMAKE_CURRENT_BINARY_DIR}/jsonParser.c)
FLEX_TARGET(JsonTokeniser jtoken.l ${CMAKE_CURRENT_BINARY_DIR}/jsontoken.c)
ADD_FLEX_BISON_DEPENDENCY(JsonTokeniser JsonParser)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../Headers" "Headers")

set(SOURCE_FILES
        class.c
        file.c
        formin.c
        formout.c
        hash.c
        hosts.c
        io.c
        ixTree.c
        lists.c
        locked.c
        managed.c
        object.c
        pair.c
        pipe.c
        pool.c
        sockets.c
        stringBuffer.c
        template.c
        timer.c
        udp.c
        unichar.c
        unicode.c
        unitable.c
        utils.c

        ${BISON_JsonParser_OUTPUTS}
        ${FLEX_JsonTokeniser_OUTPUTS}

        base64.c Headers/base64.h Headers/io.h Headers/ioP.h Headers/file.h Headers/fileP.h Headers/formio.h Headers/formioP.h
        Headers/hash.h Headers/hosts.h Headers/hostsP.h Headers/integer.h
        Headers/iosockP.h Headers/ioTcp.h Headers/iterate.h Headers/ixTree.h Headers/ixTreeP.h Headers/jsonEvent.h
        Headers/lists.h Headers/listsP.h Headers/lockable.h Headers/lockableP.h Headers/logical.h
        Headers/managed.h Headers/managedP.h Headers/object.h Headers/objectP.h
        Headers/pool.h Headers/retcode.h Headers/pipe.h Headers/pipeP.h
        Headers/stringBuffer.h Headers/stringBufferP.h
        Headers/timer.h
        Headers/unicode.h Headers/unicodeP.h Headers/utils.h
        Headers/jsonEvent.h Headers/udp.h Headers/udpP.h Headers/ooio.h options.c Headers/options.h Headers/template.h)

add_library(ooio STATIC ${SOURCE_FILES})

install(TARGETS ooio DESTINATION lib)
install(FILES integer.h logical.h retcode.h unicode.h DESTINATION include)
