cmake_minimum_required(VERSION 3.20)
project(Reference)

include(../asciidoc.cmake)

add_adoc(ref intro lexical opg programs types grammar patterns expressions conditions queries actions standard numbers strings collections json rdf date catalog)
