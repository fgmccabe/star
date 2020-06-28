# find emacs and complain if not found
find_program(EMACS_EXECUTABLE emacs)
if(NOT EMACS_EXECUTABLE)
  message(SEND_ERROR "Emacs could not be found")
endif()

function(add_emacs args)
  if(EMACS_EXECUTABLE)
    set(emacs_elc)
    foreach(v ${ARGV})
      set(emacs_tgt ${CMAKE_CURRENT_BINARY_DIR}/${v}.elc)
      set(emacs_src ${CMAKE_CURRENT_BINARY_DIR}/${v}.el)
      set(emacs_elc ${emacs_elc} ${emacs_tgt})
      configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${v}.el ${emacs_src})
      add_custom_command(OUTPUT ${emacs_tgt}
	COMMAND ${EMACS_EXECUTABLE} -Q --batch -L . -f batch-byte-compile
	${emacs_src}
	DEPENDS ${emacs_src}
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	COMMENT "Creating byte-compiled Emacs lisp ${emacs_tgt} from ${emacs_src}")
    endforeach()

    # add a top-level target to byte-compile all emacs-lisp sources
    add_custom_target(emacs_byte_compile ALL DEPENDS ${emacs_elc})

    # install the byte-compiled emacs-lisp sources
    install(FILES ${emacs_elc}
      DESTINATION share/emacs/site-lisp)
  endif(EMACS_EXECUTABLE)
endfunction(add_emacs)
