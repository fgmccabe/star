star.compiler.canon{
  import star.

  import star.compiler.location.
  import star.compiler.types.

  public canon ::= vr(locn,string,tipe) |
    intLit(locn,integer) |
    floatLit(locn,float) |
    stringLit(locn,string) |
    enm(locn,string,tipe) |
    whr(locn,canon,canon) |
    abstraction(locn,canon,canon,canon,tipe) |
    search(locn,canon,canon,canon) |
    match(locn,canon,canon) |
    conj(locn,canon,canon) |
    disj(locn,canon,canon) |
    cond(locn,canon,canon,canon) |
    apply(locn,canon,canon,tipe) |
    tple(locn,list[canon]) |
    varRef(locn,canon,tipe) |
    lambda(locn,list[(locn,canon,canon,canon)],tipe) |
    letExp(locn,canon,canon) |
    theta(locn,string,boolean,list[list[canonDef]],list[canon],tipe) |
    record(locn,string,boolean,list[list[canonDef]],list[canon],tipe).

  public canonDef ::= varDef(locn,string,string,canon,list[constraint],tipe) |
      typeDef(locn,string,tipe,tipe) |
      conDef(locn,string,string,tipe) |
      cnsDef(locn,string,string,tipe) |
      funDef(locn,string,string,list[(locn,canon,canon,canon)],tipe,list[constraint]).

  public implementation hasType[canon] => {.
    typeOf(vr(_,_,T)) => T.
    typeOf(intLit(_,_)) => tipe("star.core*integer").
    typeOf(floatLit(_,_)) => tipe("star.core*float").
    typeOf(stringLit(_,_)) => tipe("star.core*string").
    typeOf(enm(_,_,Tp)) => Tp.

    typeOf(match(_,_,_)) => tipe("star.core*boolean").
    typeOf(conj(_,_,_)) => tipe("star.core*boolean").
    typeOf(disj(_,_,_)) => tipe("star.core*boolean").
    typeOf(search(_,_,_,_)) => tipe("star.core*boolean").
    typeOf(cond(_,_,L,_)) => typeOf(L).


  .}

  public implementation hasLoc[canon] => {.
    locOf(vr(Lc,_,_)) => Lc.
  .}
}
