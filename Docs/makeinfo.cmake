find_program(MAKEINFO makeinfo
        PATHS ${PATH} NODEFAULT)
#message("Makeinfo = ${MAKEINFO}")

function(add_info nm)
    if (MAKEINFO)
        set(info_texi ${CMAKE_CURRENT_SOURCE_DIR}/${nm}.texi)
        set(out_info ${CMAKE_CURRENT_BINARY_DIR}/${nm}.info)
        set(out_html ${CMAKE_CURRENT_BINARY_DIR}/${nm}.html)
        set(out_pdf ${CMAKE_CURRENT_BINARY_DIR}/${nm}.pdf)

        set(info_deps)

        foreach (v ${ARGV})
          set(info_deps ${info_deps} ${v}.texi)
        endforeach ()

        add_custom_command(OUTPUT ${out_info}
          COMMENT "Creating Info file ${out_info}"
	  COMMAND ${MAKEINFO} -o ${out_info} ${info_texi}
	  DEPENDS ${info_deps}
	  VERBATIM)

        add_custom_target(${nm}.info ALL DEPENDS ${out_info})

        add_custom_command(OUTPUT ${out_pdf}
           COMMENT "Creating PDF file ${out_pdf}"
	   COMMAND ${MAKEINFO} --pdf -o ${out_pdf} ${info_texi}
	   DEPENDS ${info_deps}
	   VERBATIM)

        add_custom_target(${nm}.pdf ALL DEPENDS ${out_pdf})

        add_custom_command(OUTPUT ${out_html}
           COMMENT "Creating HTML file ${out_html}"
	   COMMAND ${MAKEINFO} --html --no-split -o ${out_html} ${info_texi}
	   DEPENDS ${info_deps}
	   VERBATIM)

        add_custom_target(${nm}.html ALL DEPENDS ${out_html})
    endif (MAKEINFO)
endfunction(add_info)
