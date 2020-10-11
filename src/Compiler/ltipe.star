star.compiler.ltipe{
  import star.
  import star.multi.

  import star.compiler.types.

  public ltipe ::= .int64 |
    .flt64 |
    .bool |
    .ptr |
    funTipe(cons[ltipe],ltipe) |
    tplTipe(cons[ltipe]).

  public implementation display[ltipe] => let{
    showTp:(ltipe) => ss.
    showTp(.int64) => ss("int64").
    showTp(.flt64) => ss("flt64").
    showTp(.bool) => ss("bool").
    showTp(.ptr) => ss("ptr").
    showTp(funTipe(As,R)) => ssSeq([ss("("),showTp(tplTipe(As)),ss(")->"),showTp(R)]).
    showTp(tplTipe(As)) => ssSeq([ss("("),ssSeq(interleave(As//showTp,ss(","))),ss(")")]).
  } in {
    disp = showTp
  }

  public implementation equality[ltipe] => let{
    eq(.int64,.int64)=>.true.
    eq(.flt64,.flt64)=>.true.
    eq(.bool,.bool)=>.true.
    eq(.ptr,.ptr)=>.true.
    eq(funTipe(A1,R1),funTipe(A2,R2))=>
      eqs(A1,A2) && eq(R1,R2).
    eq(tplTipe(A1),tplTipe(A2))=>eqs(A1,A2).
    eq(_,_) default => .false.

    eqs([],[])=>.true.
    eqs([E1,..T1],[E2,..T2]) => eq(E1,E2) && eqs(T1,T2)
  } in {
    X==Y => eq(X,Y)
  }

  public implementation hash[ltipe] => let{
    hsh(.int64)=>hash("int64").
    hsh(.flt64)=>hash("flt64").
    hsh(.bool)=>hash("bool").
    hsh(.ptr)=>hash("ptr").
    hsh(funTipe(A1,R1))=>
      hshs(A1,hash("=>"))*37+hsh(R1).
    hsh(tplTipe(A1))=>hshs(A1,hash("()")).

    hshs([],H)=>H.
    hshs([E1,..T1],H) => hshs(T1,H*37+hsh(E1)).
  } in {
    hash(X) => hsh(X)
  }

  public implementation coercion[tipe,ltipe] => {.
    _coerce(T) => some(reduceTp(T))
  .}

  reduceTp:(tipe)=>ltipe.
  reduceTp(T) => redTp(deRef(T)).
  
  redTp(nomnal("star.core*integer")) => .int64.
  redTp(nomnal("star.core*float")) => .flt64.
  redTp(nomnal("star.core*boolean")) => .bool.
  redTp(nomnal(_)) => .ptr.
  redTp(Tp) where (A,R) ^= isFunType(Tp) && tupleType(As).=deRef(A) =>
    funTipe(As//reduceTp,reduceTp(R)).
  redTp(tupleType(A)) => tplTipe(A//reduceTp).
  redTp(_) => .ptr.
}
