test.co{
  import star.core.

  public
  contract all s,d ~~ coercion[s,d] ::= {
    _coerce:(s)=>d.
  }

  public implementation all a,b,e,f ~~ coercion[a,b], coercion[e,f] |: coercion[(a,e),(b,f)] => {
    _coerce(T) => coerceTuple(T).
  }

  private coerceTuple:all a,b,e,f ~~ coercion[a,b], coercion[e,f] |: ((a,e)) => (b,f).
  coerceTuple((A,E)) => (_coerce(A),_coerce(E)).
}
