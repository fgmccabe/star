star.compiler.types{
  import star.

  import star.compiler.iden.

  public tipe ::=
    anonType |
    voidType |
    thisType |
    kVar(string) |
    tVar(tv,string) |
    tFun(tv,integer,string) |
    tipe(iden) |
    tpExp(tipe,integer,tipe) |
    refType(tipe) |
    tupleType(list[tipe]) |
    allType(string,tipe) |
    existType(string,tipe) |
    faceType(map[string,tipe]) |
    typeLambda(tipe,tipe) |
    constrainedType(tipe,constraint).

  public constraint ::=
    conConstraint(iden,list[tipe],list[tipe]) |
    implConstraint(tipe,tipe).

  tv ::= tv{
    binding : ref option[tipe].
    constraints : ref list[constraint].
  }

  public deRef:(tipe) => tipe.
  deRef(tVar(B,_)) where T^=B.binding! => deRef(T).
  deRef(tFun(B,_,_)) where T^=B.binding! => deRef(T).
  deRef(Tp) default => Tp.

  public newTypeVar:(string) => tipe.
  newTypeVar(Pre) => tVar(tv{binding := none. constraints := []. },genSym(Pre)).







}
