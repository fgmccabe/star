cmake_minimum_required(VERSION 3.20)
project(noarch)

set(SOURCE_FILES none.c)

add_library(NONE STATIC ${SOURCE_FILES})
