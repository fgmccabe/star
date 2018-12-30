star.compiler.misc{
  import star.

  public genSym:(string) => string.
  genSym(Pre) => _str_gen(Pre).

  public markerType ::= typeMark | valMark | conMark | overMark | pkgMark.

  public markerString:(markerType)=>string.
  markerString(typeMark)=>"*".
  markerString(valMark)=>"@".
  markerString(conMark)=>"#".
  markerString(overMark)=>"!".
  markerString(pkgMark) => "#".

}
