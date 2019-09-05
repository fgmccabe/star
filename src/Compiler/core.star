star.compiler.core{
  import star.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.
  
  public crExp ::= crVar(locn,crVar)
    | crLit(locn,data,tipe)
    | crApply(locn,crExp,list[crExp])
    | crLet(locn,crVar,crExp,crExp)
    | crLam(locn,list[crVar],crExp)
    | crCase(locn,crExp,list[(locn,crExp)])
    | crType(locn,tipe).
  
  crVar ::= crId(string,tipe)
    | crTpVr(string,crKind).

  crKind ~> tipe.

  public implementation hasLoc[crExp] => {
    locOf(crVar(Lc,_)) => Lc.
    locOf(crLit(Lc,_,_)) => Lc.
    locOf(crApply(Lc,_,_)) => Lc.
    locOf(crLet(Lc,_,_,_)) => Lc.
    locOf(crLam(Lc,_,_)) => Lc.
    locOf(crCase(Lc,_,_)) => Lc.
    locOf(crType(Lc,_)) => Lc.
  }

  public implementation hasType[crExp] => {
    typeOf(crVar(_,Vr)) => Tp.
    typeOf(crLit(_,_,Tp)) => Tp.
    typeOf(crApply(_,F,_)) => Tp.
    typeOf(crLet(_,_,_,E)) => typeOf(E).
    typeOf(crLam(_,_,_)) => Tp.
    typeOf(crCase(Lc,_,_)) => Tp.
    typeOf(crType(_,Tp)) => Tp.
  }

}
