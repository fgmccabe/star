cmake_minimum_required(VERSION 3.5)
project(COMPILER)

find_package(BISON)
find_package(FLEX)

BISON_TARGET(BaseGrammar baseGrammar.y ${CMAKE_CURRENT_BINARY_DIR}/grammar.c)
FLEX_TARGET(BaseTokeniser baseToken.l ${CMAKE_CURRENT_BINARY_DIR}/token.c)
ADD_FLEX_BISON_DEPENDENCY(BaseTokeniser BaseGrammar)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../Headers" "Headers" "../Util/Headers")

set(HEADER_FILES Headers/sexp.h Headers/sexpP.h
        Headers/catalog.h Headers/catalogP.h)

set(SOURCE_FILES
        action.c
        arith.c
        case.c
        catalog.c
        catch.c
        cfun.c
        compile.c
        condition.c
        constructor.c
        convert.c
        dict.c
        errors.c
        escapes.c
        expression.c
        gc.c
        io.c
        labels.c
        location.c
        main.c
        memo.c
        meta.c
        method.c
        moves.c
        options.c
        package.c
        pattern.c
        peep.c
        program.c
        resource.c
        rewrite.c
        sexp.c
        theta.c
        thunk.c
        types.c

        ${HEADER_FILES}

        ${BISON_BaseGrammar_OUTPUTS}
        ${FLEX_BaseTokeniser_OUTPUTS}
 )

add_executable(compiler ${SOURCE_FILES})
target_link_libraries(compiler ooio)

install(TARGETS compiler DESTINATION lib)
