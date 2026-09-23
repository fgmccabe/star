star.compiler.ltipe{
  import star.
  import star.multi.

  import star.compiler.types.

  public ltipe ::= .int64 |
  .flt64 |
  .uni |
  .bool |
  .ptr |
  .fnTipe(cons[ltipe],ltipe,ltipe) |
  .prTipe(cons[ltipe],ltipe) |
  .tplTipe(cons[ltipe]) |
  .vdTipe.

  public implementation display[ltipe] => let{.
    showTp:(ltipe) => string.
    showTp(Tp) => case Tp in {
      | .int64 => "i64"
      | .flt64 => "f64"
      | .uni => "uni"
      | .bool => "bol"
      | .ptr => "ptr"
      | .fnTipe(As,R,.vdTipe) => "#(showTp(.tplTipe(As)))->#(showTp(R))"
      | .fnTipe(As,R,E) => "#(showTp(.tplTipe(As)))->#(showTp(R)) throws #(showTp(E))"
      | .prTipe(As,.vdTipe) => "#(showTp(.tplTipe(As))){}"
      | .prTipe(As,E) => "#(showTp(.tplTipe(As))){} throws #(showTp(E))"
      | .tplTipe(As) => "⟪#(interleave(As//showTp,",")*)⟫"
      | .vdTipe => "v"
    }
  .} in {
    disp = showTp
  }

  public implementation equality[ltipe] => let{.
    eq(Tp1,Tp2) => case Tp1 in {
      | .int64 => .int64.=Tp2
      | .flt64 => .flt64.=Tp2
      | .uni => .uni.=Tp2
      | .bool => .bool.=Tp2
      | .ptr => .ptr.=Tp2
      | .fnTipe(A1,R1,E1) => .fnTipe(A2,R2,E2).=Tp2 &&
	  eqs(A1,A2) && eq(R1,R2) && eq(E1,E2)
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
      | .uni => hash("unicode")
      | .int64 => hash("int64")
      | .flt64 => hash("flt64")
      | .bool => hash("bool")
      | .ptr => hash("ptr")
      | .fnTipe(A1,R1,E1)=> (hshs(A1,hash("=>"))*37+hsh(R1))*37+hsh(E1)
      | .prTipe(A1,E1)=> hshs(A1,hash("{}"))*37+hsh(E1)
      | .tplTipe(A1)=>hshs(A1,hash("()"))
      | .vdTipe => hash("v")
    }

    hshs([],H)=>H.
    hshs([E1,..T1],H) => hshs(T1,H*37+hsh(E1)).
  .} in {
    hash(X) => hsh(X)
  }

  public implementation coercion[tipe,ltipe->>_] => {
    _coerce(T) => reduceTp(T)
  }

  public implementation coercion[ltipe,multi[char]->>void] => {
    _coerce(LT) => encTp(LT)
  }

  public implementation coercion[ltipe,string->>void] => {
    _coerce(LT) => (encTp(LT)::cons[char])::string
  }

  public encTp:(ltipe)=>multi[char].
  encTp(Tp) => case Tp in {
    | .uni => [`c`]
    | .int64 => [`i`]
    | .flt64 => [`f`]
    | .bool => [`l`]
    | .ptr => [`p`]
    | .fnTipe(As,R,E) => [`F`,..encTp(.tplTipe(As))]++encTp(R)++encTp(E)
    | .prTipe(As,E) => [`P`,..encTp(.tplTipe(As))]++encTp(E)
    | .tplTipe(As) => [`(`]++.multi(As//encTp)++[`)`]
    | .vdTipe => [`v`]
  }

  public decTp:(cons[char])=>(ltipe,cons[char]) throws exception.
  decTp([Ch,..Cs]) => case Ch in {
    | `c` => (.uni,Cs)
    | `i` => (.int64,Cs)
    | `f` => (.flt64,Cs)
    | `l` => (.bool,Cs)
    | `p` => (.ptr,Cs)
    | `(` => let{.
      decTps:(cons[char],cons[ltipe])=>(ltipe,cons[char]) throws exception.
      decTps([`)`,..Cs],So) => (.tplTipe(reverse(So)),Cs).
      decTps(C,So) where (E,C1).=decTp(Cs) => decTps(C1,[E,..So]).
    .} in decTps(Cs,[])
    | `F` where (.tplTipe(As),C0).=decTp(Cs) && (Rt,C1) .= decTp(C0) && (Et,Cx) .= decTp(C1)  =>
      (.fnTipe(As,Rt,Et),Cx)
    | `v` => (.vdTipe,Cs)
    | _ default => throw .exception("invalid ltipe encoding: $(Ch)")
  }

  public reduceTp:(tipe)=>ltipe.
  reduceTp(T) => redTp(deRef(T)).

  redTp(Tp) => case Tp in {
    | .nomnal("char") => .uni
    | .nomnal("integer") => .int64
    | .nomnal("float") => .flt64
    | .nomnal("boolean") => .bool
    | .nomnal(_) => .ptr
    | _ where (A,R,E) ?= isFunType(Tp) && .tupleType(As).=deRef(A) =>
      .fnTipe(As//reduceTp,reduceTp(R),reduceTp(E))
    | _ where (A,E) ?= isPrType(Tp) && .tupleType(As).=deRef(A) =>
      .prTipe(As//reduceTp,reduceTp(E))
    | .tupleType(A) => .tplTipe(A//reduceTp)
    | .voidType => .vdTipe
    | .allType(_,BTp) => redTp(deRef(BTp))
    | .existType(_,BTp) => redTp(deRef(BTp))
    | .constrainedType(BTp,_) => redTp(deRef(BTp))
    | _ default => .ptr
  }

  public extendFunTipe:(ltipe,option[ltipe])=>ltipe.
  extendFunTipe(.fnTipe(As,Rs,Et),.some(T)) => .fnTipe([T,..As],Rs,Et).
  extendFunTipe(.prTipe(As,Et),.some(T)) => .prTipe([T,..As],Et).
  extendFunTipe(Tp,.none) => Tp.

  public isThrowingTipe:(ltipe) => boolean.
  isThrowingTipe(.fnTipe(_,_,Et)) => Et~=.vdTipe.
  isThrowingTipe(.prTipe(_,Et)) => Et~=.vdTipe.
  isThrowingTipe(_) default => .false.

  public tipeThrows:(ltipe) => ltipe.
  tipeThrows(.fnTipe(_,_,Et)) => Et.
  tipeThrows(.prTipe(_,Et)) => Et.

  public implementation measured[ltipe->>integer] => {
    [| .fnTipe(A,_,_) |] => size(A).
    [| .prTipe(A,_) |] => size(A).
    [| .tplTipe(A) |] => size(A).
  }

}
