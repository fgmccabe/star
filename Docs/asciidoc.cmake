find_program(ASCIIDOCTOR asciidoctor)
find_program(ASCIIDOCTORPDF asciidoctor-pdf)
find_program(ASCIIDOCTOREPUB asciidoctor-epub3)

message("AsciiDoctor = ${ASCIIDOCTOR}")
message("AsciiDoctor-pdf = ${ASCIIDOCTORPDF}")
message("AsciiDoctor-epub = ${ASCIIDOCTOREPUB}")

mark_as_advanced(ASCIIDOCTOR)
mark_as_advanced(ASCIIDOCTORPDF)
mark_as_advanced(ASCIIDOCTOREPUB)

function(add_adoc nm)
  set(info_adoc ${CMAKE_CURRENT_SOURCE_DIR}/${nm}.adoc)
  set(out_html ${CMAKE_CURRENT_BINARY_DIR}/${nm}.html)
  set(out_pdf ${CMAKE_CURRENT_BINARY_DIR}/${nm}.pdf)
  set(out_epub ${CMAKE_CURRENT_BINARY_DIR}/${nm}.epub)

  set(adoc_deps)
  foreach (v ${ARGV})
    set(adoc_deps ${adoc_deps} ${v}.adoc)
  endforeach ()

  message("adoc_deps = ${adoc_deps}")

  if(ASCIIDOCTORPDF)
    message("adding asciidoctor-pdf")
    add_custom_target(${nm}.pdf
             COMMAND ${ASCIIDOCTORPDF} -a VERSION="${version}" -o ${out_pdf} ${info_adoc}
             DEPENDS ${adoc_deps})
  endif(ASCIIDOCTORPDF)

  if(ASCIIDOCTOR)
    add_custom_target(${nm}.html
       COMMAND ${ASCIIDOCTOR} -a VERSION="${version}" -o ${out_html} ${info_adoc}    
       DEPENDS ${adoc_deps} )
    endif (ASCIIDOCTOR)

  if(ASCIIDOCTOREPUB)
    message("adding asciidoctor-epub3")
    add_custom_target(${nm}.epub
             COMMAND ${ASCIIDOCTOREPUB} -a VERSION="${version}" -o ${out_epub} ${info_adoc}
             DEPENDS ${adoc_deps})
  endif(ASCIIDOCTOREPUB)
endfunction(add_adoc)



