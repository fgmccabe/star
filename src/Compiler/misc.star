star.compiler.misc{
  import star.

  public genSym:(string) => string.
  genSym(Pre) => _str_gen(Pre).

  public genNewName:(string,string) => string.
  genNewName(Path,Prefix) => localName(Path,markerString(valMark),genSym(Prefix)).

  public markerType ::= typeMark | valMark | conMark | overMark | pkgMark.

  public markerString:(markerType)=>string.
  markerString(typeMark)=>"*".
  markerString(valMark)=>"@".
  markerString(conMark)=>"#".
  markerString(overMark)=>"!".
  markerString(pkgMark) => "#".

  public localName:(string,string,string) => string.
  localName(_,Glue,Nm) where Ix .= _str_find(Nm,Glue,0) && Ix>=0 => Nm.
  localName(Pth,Glue,Nm) => _str_concat(Pth,_str_concat(Glue,Nm)).
  
}
