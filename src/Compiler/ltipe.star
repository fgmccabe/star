star.compiler.ltipe{
  import star.
  import star.multi.

  import star.compiler.types.

  public ltipe ::= .int64 |
  .flt64 |
  .bool |
  .ptr |
  .fnTipe(cons[ltipe],ltipe) |
  .prTipe(cons[ltipe],ltipe) |
  .tplTipe(cons[ltipe]) |
  .vdTipe.

  public implementation display[ltipe] => let{.
    showTp:(ltipe) => string.
    showTp(Tp) => case Tp in {
      | .int64 => "int64"
      | .flt64 => "flt64"
      | .bool => "bool"
      | .ptr => "ptr"
      | .fnTipe(As,R) => "(#(showTp(.tplTipe(As))))->#(showTp(R))"
      | .prTipe(As,E) => "(#(showTp(.tplTipe(As)))){} throws #(showTp(E))"
      | .tplTipe(As) => "(#(interleave(As//showTp,",")*))"
      | .vdTipe => "v"
    }
  .} in {
    disp = showTp
  }

  public implementation equality[ltipe] => let{.
    eq(Tp1,Tp2) => case Tp1 in {
      | .int64 => .int64.=Tp2
      | .flt64 => .flt64.=Tp2
      | .bool => .bool.=Tp2
      | .ptr => .ptr.=Tp2
      | .fnTipe(A1,R1) => .fnTipe(A2,R2).=Tp2 &&
	  eqs(A1,A2) && eq(R1,R2)
      | .prTipe(A1,E1) => .prTipe(A2,E2).=Tp2 && eqs(A1,A2) && eq(E1,E2)
      | .tplTipe(A1) => .tplTipe(A2).=Tp2 &&eqs(A1,A2)
      | .vdTipe => .vdTipe.=Tp2
    }

    eqs([],[])=>.true.
    eqs([E1,..T1],[E2,..T2]) => eq(E1,E2) && eqs(T1,T2)
  .} in {
    X==Y => eq(X,Y)
  }

  public implementation hashable[ltipe] => let{.
    hsh(Tp) => case Tp in {
      | .int64 => hash("int64")
      | .flt64 => hash("flt64")
      | .bool => hash("bool")
      | .ptr => hash("ptr")
      | .fnTipe(A1,R1)=> hshs(A1,hash("=>"))*37+hsh(R1)
      | .prTipe(A1,E1)=> hshs(A1,hash("{}"))*37+hsh(E1)
      | .tplTipe(A1)=>hshs(A1,hash("()"))
      | .vdTipe => hash("v")
    }

    hshs([],H)=>H.
    hshs([E1,..T1],H) => hshs(T1,H*37+hsh(E1)).
  .} in {
    hash(X) => hsh(X)
  }

  public implementation coercion[tipe,ltipe] => {
    _coerce(T) => .some(reduceTp(T))
  }

  public implementation coercion[ltipe,multi[char]] => {
    _coerce(LT) => .some(encTp(LT))
  }

  public implementation coercion[ltipe,string] => {
    _coerce(LT) => .some((encTp(LT)::cons[char])::string)
  }

  encTp:(ltipe)=>multi[char].
  encTp(Tp) => case Tp in {
    | .int64 => [`i`]
    | .flt64 => [`f`]
    | .bool => [`l`]
    | .ptr => [`p`]
    | .fnTipe(As,R) => [`F`,..encTp(.tplTipe(As))]++encTp(R)
    | .prTipe(As,E) => [`P`,..encTp(.tplTipe(As))]++encTp(E)
    | .tplTipe(As) => [`(`]++.multi(As//encTp)++[`)`]
    | .vdTipe => [`v`]
  }


  decTp:(cons[char])=>option[(ltipe,cons[char])].
  decTp([Ch,..Cs]) => case Ch in {
    | `i` => .some((.int64,Cs))
    | `f` => .some((.flt64,Cs))
    | `l` => .some((.bool,Cs))
    | `p` => .some((.ptr,Cs))
    | `(` => let{.
      decTps:(cons[char],multi[ltipe])=>option[(ltipe,cons[char])].
      decTps([`)`,..Cs],So) => .some((.tplTipe(So::cons[ltipe]),Cs)).
      decTps(C,So) where (E,C1)?=decTp(Cs) => decTps(C1,So++[E]).
    .} in decTps(Cs,[])
    | `F` where (.tplTipe(As),C0)?=decTp(Cs) && (Rt,Cx) ?= decTp(C0) =>
      .some((.fnTipe(As,Rt),Cx))
    | `v` => .some((.vdTipe,Cs))
  }

  reduceTp:(tipe)=>ltipe.
  reduceTp(T) => redTp(deRef(T)).

  redTp(Tp) => case Tp in {
    | .nomnal("integer") => .int64
    | .nomnal("float") => .flt64
    | .nomnal("boolean") => .bool
    | .nomnal(_) => .ptr
    | _ where (A,R,_) ?= isFunType(Tp) && .tupleType(As).=deRef(A) =>
      .fnTipe(As//reduceTp,reduceTp(R))
    | _ where (A,E) ?= isPrType(Tp) && .tupleType(As).=deRef(A) =>
      .prTipe(As//reduceTp,reduceTp(E))
    | .tupleType(A) => .tplTipe(A//reduceTp)
    | .voidType => .vdTipe
    | _ default => .ptr
  }
}
