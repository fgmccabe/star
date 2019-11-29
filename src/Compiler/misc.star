star.compiler.misc{
  import star.
  import star.pkg.

  public genSym:(string) => string.
  genSym(Pre) => _str_gen(Pre).

  public genNewName:(string,string) => string.
  genNewName(Path,Prefix) => qualifiedName(Path,markerString(valMark),genSym(Prefix)).

  public markerType ::= typeMark | valMark | conMark | overMark | pkgMark.

  public markerString:(markerType)=>string.
  markerString(typeMark)=>"*".
  markerString(valMark)=>"@".
  markerString(conMark)=>"#".
  markerString(overMark)=>"!".
  markerString(pkgMark) => "#".

  public qualifiedName:(string,string,string) => string.
  qualifiedName(_,Glue,Nm) where Ix .= _str_find(Nm,Glue,0) && Ix>=0 => Nm.
  qualifiedName(Pth,Glue,Nm) => _str_concat(Pth,_str_concat(Glue,Nm)).

  public localName:(string,markerType)=>string.
  localName(QNm,M) where Ix.=_str_find(QNm,markerString(M),0) &&
      Ix>=0 && MX .= _str_len(markerString(M)) =>
    _sub_str(QNm,Ix+MX,_str_len(QNm)-Ix-MX).

  public packageVar:(pkg)=>string.
  packageVar(pkg(P,_)) => qualifiedName(P,markerString(pkgMark),"").

  public packageName:(pkg)=>string.
  packageName(pkg(P,_))=>P.

  public pickFailures:all e,x ~~ (list[either[e,x]])=>either[e,list[x]].
  pickFailures(Ls) => let{
    pick:all e,x ~~ (list[either[e,x]],list[x])=>either[e,list[x]].
    pick([],L) => either(L).
    pick([either(X),..Els],L) => pick(Els,[L..,X]).
    pick([other(E),.._],_) => other(E).
  } in pick(Ls,[]).

  public mapOver:all s/1,m/1,a,b ~~ stream[s[a]->>a],sequence[s[b]->>b],monad[m] |:
    ((a)=>m[b],s[a])=>m[s[b]].
  mapOver(F,S)=>let{
    mpOver([],Sf)=> return Sf.
    mpOver([E,..Es],Sf) =>
      F(E) >>= (E1)=>mpOver(Es,[Sf..,E1]).
  } in mpOver(S,[]).
}
