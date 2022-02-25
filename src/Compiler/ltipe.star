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
    showTp:(ltipe) => string.
    showTp(.int64) => "int64".
    showTp(.flt64) => "flt64".
    showTp(.bool) => "bool".
    showTp(.ptr) => "ptr".
    showTp(funTipe(As,R)) => "(#(showTp(tplTipe(As))))->#(showTp(R))".
    showTp(tplTipe(As)) => "(#(interleave(As//showTp,",")*))".
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

  public implementation hashable[ltipe] => let{.
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

  public implementation coercion[ltipe,multi[char]] => {
    _coerce(LT) => some(encTp(LT))
  }

  public implementation coercion[ltipe,string] => {
    _coerce(LT) => some((encTp(LT)::cons[char])::string)
  }

  encTp:(ltipe)=>multi[char].
  encTp(.int64) => [`i`].
  encTp(.flt64) => [`f`].
  encTp(.bool) => [`l`].
  encTp(.ptr) => [`p`].
  encTp(funTipe(As,R)) => [`F`,..encTp(tplTipe(As))]++encTp(R).
  encTp(tplTipe(As)) => [`(`]++multi(As//encTp)++[`)`].


  decTp:(cons[char])=>option[(ltipe,cons[char])].
  decTp([`i`,..Cs]) => some((.int64,Cs)).
  decTp([`f`,..Cs]) => some((.flt64,Cs)).
  decTp([`l`,..Cs]) => some((.bool,Cs)).
  decTp([`p`,..Cs]) => some((.ptr,Cs)).
  decTp([`(`,..Cs]) => let{.
    decTps:(cons[char],multi[ltipe])=>option[(ltipe,cons[char])].
    decTps([`)`,..Cs],So) => some((tplTipe(So::cons[ltipe]),Cs)).
    decTps(C,So) where (E,C1)^=decTp(Cs) => decTps(C1,So++[E]).
  .} in decTps(Cs,[]).
  decTp([`F`,..Cs]) where (tplTipe(As),C0)^=decTp(Cs) && (Rt,Cx) ^= decTp(C0) =>
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
