find_program(MAKEINFO makeinfo
	PATHS ${PATH} NODEFAULT)
message("Makeinfo = ${MAKEINFO} $ENV{PATH}")

function(add_info nm)
  if(MAKEINFO)
    set(info_texi ${CMAKE_CURRENT_SOURCE_DIR}/${nm}.texi)
    set(info_out ${CMAKE_CURRENT_BINARY_DIR}/${nm}.info)
    set(info_html ${CMAKE_CURRENT_BINARY_DIR}/${nm}.html)

    set(info_deps)

    foreach(v ${ARGV})
      set(info_deps ${info_deps} ${v}.texi)
    endforeach()

    add_custom_command(OUTPUT ${info_out}
      COMMAND ${MAKEINFO} -o ${info_out} ${info_texi}
      DEPENDS ${info_deps} 
      COMMENT "Creating Info file ${info_out}"
      VERBATIM)

    add_custom_target(info ALL DEPENDS ${info_out})

    add_custom_command(OUTPUT ${info_html}
      COMMAND ${MAKEINFO} --html --no-split -o ${info_html} ${info_texi}
      DEPENDS ${info_deps} 
      COMMENT "Creating HTML file ${info_html}"
      VERBATIM)

    add_custom_target(html ALL DEPENDS ${info_html})
  endif(MAKEINFO)
endfunction(add_info)
