cmake_minimum_required(VERSION 3.20)

add_subdirectory("Util")
add_subdirectory("HeaderGenerator")
add_subdirectory("Escapes")
add_subdirectory("BootCompiler")
add_subdirectory("Infra")
add_subdirectory("Engine")

if (NOT DEFINED NOJIT)
  add_subdirectory("Jit")
endif()

#add_subdirectory("Star")
add_subdirectory("Compiler")
add_subdirectory("Tests")
add_subdirectory("Emacs")
