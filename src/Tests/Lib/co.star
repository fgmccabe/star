test.co{
  import star.core.

  public
  contract all s,d ~~ coercion[s,d] ::= {
    _coerce:(s)=>option[d].
  }

  public implementation all a,b,e,f ~~ coercion[a,b], coercion[e,f] |: coercion[(a,e),(b,f)] => let{
    coercePair(?A,?B) => ?(A,B).
    coercePair(_,_) default => .none
  } in {
    _coerce((AA,BB)) => coercePair(_coerce(AA),_coerce(BB)).
  }
}
