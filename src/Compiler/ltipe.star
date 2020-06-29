star.compiler.ltipe{
  import star.
  import star.multi.

  import star.compiler.types.

  public ltipe ::= .int64 | .flt64 | .bool | .ptr | funTipe(cons[ltipe],ltipe) | tplTipe(cons[ltipe]) .

  public implementation coercion[ltipe,string] => {.
    _coerce(T) => (encLt(T)::cons[integer]):?string
  .}

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
  
  encLt:(ltipe)=>multi[integer].
  encLt(.int64) => [0ci].
  encLt(.flt64) => [0cf].
  encLt(.bool) => [0cl].
  encLt(.ptr) => [0cp].
  encLt(funTipe(As,R)) => [0cF,..encLt(tplTipe(As))]++encLt(R).
  encLt(tplTipe(As)) => ([0c\(]++multi(As//encLt)++[0c\)]).
}
