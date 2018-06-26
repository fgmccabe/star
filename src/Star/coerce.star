star.coerce{
  import star.core.

  public
  contract all s,d ~~ coercion[s,d] ::= {
    _coerce:(s)=>d.
  }

  public implementation coercion[integer,string] => {
    _coerce(Ix) => _int2str(Ix,10,0,0)
  }

  public implementation coercion[float,string] => {
    _coerce(Dx) => _flt2str(Dx,0,8,0cg,false)
  }

  public implementation coercion[string,float] => {
    _coerce(Sx) => _str2flt(Sx).
  }

  public implementation coercion[string,integer] => {
    _coerce(Sx) => _str2int(Sx).
  }

  public implementation coercion[integer,float] => {
    _coerce(Ix) => _int2flt(Ix).
  }

  public implementation coercion[float,integer] => {
    _coerce(Dx) => _flt2int(Dx).
  }

  public implementation all a,b,e,f ~~ coercion[a,b], coercion[e,f] |: coercion[(a,e),(b,f)] => {
    _coerce(T) => coerceTuple(T).
  }

  private coerceTuple:all a,b,e,f ~~ coercion[a,b], coercion[e,f] |: ((a,e)) => (b,f).
  coerceTuple((A,E)) => (_coerce(A),_coerce(E)).
}
