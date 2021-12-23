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

  public implementation display[ltipe] => let{.
    showTp:(ltipe) => ss.
    showTp(.int64) => ss("int64").
    showTp(.flt64) => ss("flt64").
    showTp(.bool) => ss("bool").
    showTp(.ptr) => ss("ptr").
    showTp(funTipe(As,R)) => ssSeq([ss("("),showTp(tplTipe(As)),ss(")->"),showTp(R)]).
    showTp(tplTipe(As)) => ssSeq([ss("("),ssSeq(interleave(As//showTp,ss(","))),ss(")")]).
  .} in {
    disp = showTp
  }

  public implementation equality[ltipe] => let{.
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
  .} in {
    X==Y => eq(X,Y)
  }

  public implementation hash[ltipe] => let{.
    hsh(.int64)=>hash("int64").
    hsh(.flt64)=>hash("flt64").
    hsh(.bool)=>hash("bool").
    hsh(.ptr)=>hash("ptr").
    hsh(funTipe(A1,R1))=>
      hshs(A1,hash("=>"))*37+hsh(R1).
    hsh(tplTipe(A1))=>hshs(A1,hash("()")).

    hshs([],H)=>H.
    hshs([E1,..T1],H) => hshs(T1,H*37+hsh(E1)).
  .} in {
    hash(X) => hsh(X)
  }

  public implementation coercion[tipe,ltipe] => {
    _coerce(T) => some(reduceTp(T))
  }

  public implementation coercion[ltipe,multi[integer]] => {
    _coerce(LT) => some(encTp(LT))
  }

  public implementation coercion[ltipe,string] => {
    _coerce(LT) => some((encTp(LT)::cons[integer])::string)
  }

  encTp:(ltipe)=>multi[integer].
  encTp(.int64) => [0ci].
  encTp(.flt64) => [0cf].
  encTp(.bool) => [0cl].
  encTp(.ptr) => [0cp].
  encTp(funTipe(As,R)) => [0cF,..encTp(tplTipe(As))]++encTp(R).
  encTp(tplTipe(As)) => [0c(]++multi(As//encTp)++[0c)].


  decTp:(cons[integer])=>option[(ltipe,cons[integer])].
  decTp([0ci,..Cs]) => some((.int64,Cs)).
  decTp([0cf,..Cs]) => some((.flt64,Cs)).
  decTp([0cl,..Cs]) => some((.bool,Cs)).
  decTp([0cp,..Cs]) => some((.ptr,Cs)).
  decTp([0c\(,..Cs]) => let{.
    decTps:(cons[integer],multi[ltipe])=>option[(ltipe,cons[integer])].
    decTps([0c\),..Cs],So) => some((tplTipe(So::cons[ltipe]),Cs)).
    decTps(C,So) where (E,C1)^=decTp(Cs) => decTps(C1,So++[E]).
  .} in decTps(Cs,[]).
  decTp([0cF,..Cs]) where (tplTipe(As),C0)^=decTp(Cs) && (Rt,Cx) ^= decTp(C0) =>
    some((funTipe(As,Rt),Cx)).

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
