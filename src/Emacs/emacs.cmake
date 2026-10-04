# Byte-compile the Star Emacs mode.
#
# Emacs is optional. If it can't be found, or is found but doesn't run
# (e.g. a Homebrew build broken by an OS upgrade, or a stale cached path),
# the Emacs mode is skipped with a warning instead of breaking the build.
#
#   -DSTAR_BUILD_EMACS=OFF            skip it entirely
#   -DEMACS_EXECUTABLE=/path/to/emacs pick a specific Emacs

option(STAR_BUILD_EMACS "Byte-compile the Star Emacs mode" ON)
set(EMACS_OK FALSE)

if(STAR_BUILD_EMACS)
  find_program(EMACS_EXECUTABLE
    NAMES emacs
    HINTS /opt/homebrew/bin /usr/local/bin /Applications/Emacs.app/Contents/MacOS
    DOC "Emacs used to byte-compile the Star mode")

  if(EMACS_EXECUTABLE)
    # find_program trusts the cache, so check the Emacs we have actually runs.
    execute_process(
      COMMAND ${EMACS_EXECUTABLE} -Q --batch --eval "(princ emacs-version)"
      RESULT_VARIABLE emacs_status
      OUTPUT_VARIABLE EMACS_VERSION
      ERROR_VARIABLE emacs_error
      OUTPUT_STRIP_TRAILING_WHITESPACE
      TIMEOUT 30)
    if(emacs_status EQUAL 0)
      set(EMACS_OK TRUE)
      message(STATUS "Emacs ${EMACS_VERSION}: ${EMACS_EXECUTABLE}")
    else()
      message(WARNING
        "${EMACS_EXECUTABLE} failed to run (${emacs_status}); skipping the Emacs mode.\n"
        "${emacs_error}\n"
        "Fix or reinstall Emacs, or reconfigure with -DEMACS_EXECUTABLE=... "
        "(cmake -U EMACS_EXECUTABLE clears a stale cached path).")
    endif()
  else()
    message(WARNING "Emacs not found; skipping the Emacs mode.")
  endif()
endif()

# add_emacs(file ...) -- names without the .el suffix.
#
# All files are compiled by one Emacs process. They require one another, so
# compiling them separately lets make -j run one compile while another is
# still writing a .elc the first one loads. load-prefer-newer and removing
# the old .elc files first stop a stale .elc (e.g. from an older Emacs)
# being loaded in place of its source.
function(add_emacs)
  if(NOT EMACS_OK)
    return()
  endif()

  set(emacs_srcs)
  set(emacs_elcs)
  foreach(v ${ARGN})
    set(src ${CMAKE_CURRENT_BINARY_DIR}/${v}.el)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${v}.el ${src} COPYONLY)
    list(APPEND emacs_srcs ${src})
    list(APPEND emacs_elcs ${CMAKE_CURRENT_BINARY_DIR}/${v}.elc)
  endforeach()

  add_custom_command(
    OUTPUT ${emacs_elcs}
    COMMAND ${CMAKE_COMMAND} -E rm -f ${emacs_elcs}
    COMMAND ${EMACS_EXECUTABLE} -Q --batch
            -L ${CMAKE_CURRENT_BINARY_DIR}
            --eval "(setq load-prefer-newer t)"
            -f batch-byte-compile ${emacs_srcs}
    DEPENDS ${emacs_srcs}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Byte-compiling the Star Emacs mode"
    VERBATIM)

  add_custom_target(emacs_byte_compile ALL DEPENDS ${emacs_elcs})

  # Install the sources alongside the .elc files, so find-function works
  # and load-prefer-newer has something to prefer.
  install(FILES ${emacs_srcs} ${emacs_elcs}
    DESTINATION share/emacs/site-lisp)
endfunction()
