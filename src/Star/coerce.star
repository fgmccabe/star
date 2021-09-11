star.coerce{
  import star.core.

  public
  contract all s,d ~~ coercion[s,d] ::= {
    _coerce:(s)=>option[d].
  }

  public implementation coercion[integer,string] => {
    _coerce(Ix) => some(chrs_(_int2str(Ix,10,0,0)))
  }

  public implementation coercion[float,string] => {
    _coerce(Dx) => some(chrs_(_flt2str(Dx,0,8,0cg,.false)))
  }

  public implementation coercion[string,float] => {
    _coerce(Sx) => _str2flt(_str_fltn(Sx)).
  }

  public implementation coercion[string,integer] => {
    _coerce(Sx) => _str2int(_str_fltn(Sx)).
  }

  public implementation coercion[integer,float] => {
    _coerce(Ix) => some(_int2flt(Ix)).
  }

  public implementation coercion[float,integer] => {
    _coerce(Dx) => some(_flt2int(Dx)).
  }

  public implementation all a,b,e,f ~~ coercion[a,b], coercion[e,f] |: coercion[(a,e),(b,f)] => let{.
    private coercePair(some(A),some(B)) => some((A,B)).
    coercePair(_,_) default => .none
  .} in {.
    _coerce((A,B)) => coercePair(_coerce(A),_coerce(B)).
  .}
}
