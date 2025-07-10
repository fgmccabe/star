find_program(ASCIIDOCTOR asciidoctor)
find_program(ASCIIDOCTORPDF asciidoctor-pdf)

message("AsciiDoctor = ${ASCIIDOCTOR}")
message("AsciiDoctor-pdf = ${ASCIIDOCTORPDF}")

mark_as_advanced(ASCIIDOCTOR)
mark_as_advanced(ASCIIDOCTORPDF)

function(add_adoc nm)
  set(info_adoc ${CMAKE_CURRENT_SOURCE_DIR}/${nm}.adoc)
  set(out_html ${CMAKE_CURRENT_BINARY_DIR}/${nm}.html)
  set(out_pdf ${CMAKE_CURRENT_BINARY_DIR}/${nm}.pdf)

  set(adoc_deps)
  foreach (v ${ARGV})
    set(adoc_deps ${adoc_deps} ${v}.adoc)
  endforeach ()

  message("adoc_deps = ${adoc_deps}")

  if(ASCIIDOCTORPDF)
    message("adding asciidoctor-pdf")
    add_custom_target(${nm}.pdf ALL
             COMMAND ${ASCIIDOCTORPDF} -o ${out_pdf} ${info_adoc}
             DEPENDS ${adoc_deps})
  endif(ASCIIDOCTORPDF)

  if(ASCIIDOCTOR)
    add_custom_target(${nm}.html ALL
       COMMAND ${ASCIIDOCTOR} -o ${out_html} ${info_adoc}    
       DEPENDS ${adoc_deps} )
    endif (ASCIIDOCTOR)
endfunction(add_adoc)



