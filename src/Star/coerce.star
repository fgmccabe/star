star.coerce{
  import star.core.

  public contract all s,d ~~ coercion[s,d] ::= {
    _coerce:(s)=>option[d].
  }

  public implementation coercion[integer,string] => {
    _coerce(Ix) => .some(_int2str(Ix))
  }

  public implementation coercion[float,string] => {
    _coerce(Dx) => .some(_flt2str(Dx,8,`g`,.false))
  }

  public implementation coercion[string,float] => {
    _coerce(Sx) => (try .some(_str2flt(Sx)) catch { _ => .none}).
  }

  public implementation coercion[string,integer] => {
    _coerce(Sx) => (try .some(_str2int(Sx)) catch { _ => .none}).
  }

  public implementation coercion[integer,integer] => {
    _coerce(X) => .some(X)
  }

  public implementation coercion[integer,float] => {
    _coerce(Ix) => .some(_int2flt(Ix)).
  }

  public implementation coercion[float,integer] => {
    _coerce(Dx) => .some(_flt2int(Dx)).
  }

  public implementation coercion[float,float] => {
    _coerce(X) => .some(X)
  }

  public implementation coercion[bigint,string] => {
    _coerce(B) => .some(_big2str(B)).
  }

  public implementation coercion[string,bigint] => {
    _coerce(S) => (try .some(_str2big(S)) catch { _ => .none}).
  }

  public implementation coercion[integer,bigint] => {
    _coerce(Ix) => .some(_int2big(Ix)).
  }

  public implementation coercion[bigint,integer] => {
    _coerce(Bx) => (try .some(_big2int(Bx)) catch { _ => .none}).
  }

  public implementation all a,b,e,f ~~ coercion[a,b], coercion[e,f] |= coercion[(a,e),(b,f)] => let{
    coercePair(.some(A),.some(B)) => .some((A,B)).
    coercePair(_,_) default => .none
  } in {
    _coerce((A,B)) => coercePair(_coerce(A),_coerce(B)).
  }
}
