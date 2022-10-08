star.compiler.ltipe{
  import star.
  import star.multi.

  import star.compiler.types.

  public ltipe ::= .int64 |
    .flt64 |
    .bool |
    .ptr |
    .funTipe(cons[ltipe],ltipe) |
    .tplTipe(cons[ltipe]).

  public implementation display[ltipe] => let{.
    showTp:(ltipe) => string.
    showTp(Tp) => case Tp in {
      .int64 => "int64".
      .flt64 => "flt64".
      .bool => "bool".
      .ptr => "ptr".
      .funTipe(As,R) => "(#(showTp(tplTipe(As))))->#(showTp(R))".
      .tplTipe(As) => "(#(interleave(As//showTp,",")*))".
    }
  .} in {
    disp = showTp
  }

  public implementation equality[ltipe] => let{.
    eq(Tp1,Tp2) => case Tp1 in {
      .int64 => .int64.=Tp2.
      .flt64 => .flt64.=Tp2.
      .bool => .bool.=Tp2.
      .ptr => .ptr.=Tp2.
      .funTipe(A1,R1) => .funTipe(A2,R2).=Tp2 &&
	  eqs(A1,A2) && eq(R1,R2).
      .tplTipe(A1) => .tplTipe(A2).=Tp2 &&eqs(A1,A2).
    }

    eqs([],[])=>.true.
    eqs([E1,..T1],[E2,..T2]) => eq(E1,E2) && eqs(T1,T2)
  .} in {
    X==Y => eq(X,Y)
  }

  public implementation hashable[ltipe] => let{.
    hsh(Tp) => case Tp in {
      .int64 => hash("int64").
      .flt64 => hash("flt64").
      .bool => hash("bool").
      .ptr => hash("ptr").
      .funTipe(A1,R1)=> hshs(A1,hash("=>"))*37+hsh(R1).
      .tplTipe(A1)=>hshs(A1,hash("()")).
    }

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
  encTp(Tp) => case Tp in {
    .int64 => [`i`].
    .flt64 => [`f`].
    .bool => [`l`].
    .ptr => [`p`].
    .funTipe(As,R) => [`F`,..encTp(tplTipe(As))]++encTp(R).
    .tplTipe(As) => [`(`]++multi(As//encTp)++[`)`].
  }


  decTp:(cons[char])=>option[(ltipe,cons[char])].
  decTp([Ch,..Cs]) => case Ch in {
    `i` => .some((.int64,Cs)).
    `f` => .some((.flt64,Cs)).
    `l` => .some((.bool,Cs)).
    `p` => .some((.ptr,Cs)).
    `(` => let{.
      decTps:(cons[char],multi[ltipe])=>option[(ltipe,cons[char])].
      decTps([`)`,..Cs],So) => .some((.tplTipe(So::cons[ltipe]),Cs)).
      decTps(C,So) where (E,C1)^=decTp(Cs) => decTps(C1,So++[E]).
    .} in decTps(Cs,[]).
    `F` where (.tplTipe(As),C0)^=decTp(Cs) && (Rt,Cx) ^= decTp(C0) =>
      .some((funTipe(As,Rt),Cx)).
  }

  reduceTp:(tipe)=>ltipe.
  reduceTp(T) => redTp(deRef(T)).

  redTp(Tp) => case Tp in {
    .nomnal("star.core*integer") => .int64.
    .nomnal("star.core*float") => .flt64.
    .nomnal("star.core*boolean") => .bool.
    .nomnal(_) => .ptr.
    _ where (A,R) ^= isFunType(Tp) && .tupleType(As).=deRef(A) =>
      funTipe(As//reduceTp,reduceTp(R)).
    .tupleType(A) => tplTipe(A//reduceTp).
    _ default => .ptr.
  }
}
