# find emacs and complain if not found
find_program(EMACS_EXECUTABLE emacs)
if(NOT EMACS_EXECUTABLE)
  message(SEND_ERROR "Emacs could not be found")
endif()

function(add_emacs el)
  if(EMACS_EXECUTABLE)
    
    # copy source to binary tree
    configure_file(${el} ${CMAKE_CURRENT_BINARY_DIR}/${el})
    # add rule (i.e. command) how to generate the byte-compiled file
    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${el}c
      COMMAND ${EMACS_EXECUTABLE} -batch -f batch-byte-compile
      ${CMAKE_CURRENT_BINARY_DIR}/${el}
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${el}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT "Creating byte-compiled Emacs lisp ${CMAKE_CURRENT_BINARY_DIR}/${el}c")

    # add a top-level target to byte-compile all emacs-lisp sources
    add_custom_target(emacs_byte_compile ALL
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${el}c)

    # install the byte-compiled emacs-lisp sources
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${el}c
      DESTINATION share/emacs/site-lisp)
  endif(EMACS_EXECUTABLE)
endfunction(add_emacs)
