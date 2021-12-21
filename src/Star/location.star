star.location{
  import star.
  import star.pkg.

  public locn ::= locn(string,integer,integer,integer,integer).

  public implementation display[locn] => {
    disp(locn(P,Line,Col,_,Ln)) => "#(P)\:$(Line)\:$(Col)($(Ln))".
  }

  public implementation coercion[locn,string] => {
    _coerce(Lc) => disp(Lc):?string
  }
    
  public implementation equality[locn] => {
    locn(P1,_,_,St1,Ln1) == locn(P2,_,_,St2,Ln2) => P1==P2 && St1==St2 && Ln1==Ln2.
  }

  public implementation hash[locn] => {
    hash(locn(P,_,_,Off,Len)) => (((hash(P)*37)+hash(Off))*37)+hash(Len).
  }

  public pkgLoc:(pkg)=>locn.
  pkgLoc(P) => locn(pkgName(P),1,0,0,0).

  
}
