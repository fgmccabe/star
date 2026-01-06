star.coerce{
  import star.core.

  public contract all s,d,e ~~ coercion[s,d->>e] ::= {
    _coerce:(s)=>d throws e.
  }

  public implementation coercion[integer,string->>void] => {
    _coerce(Ix) => _int2str(Ix)
  }

  public implementation coercion[float,string->>void] => {
    _coerce(Dx) => _flt2str(Dx,8,`g`,.false)
  }

  public implementation coercion[string,float->>exception] => {
    _coerce(Sx) => (try
      _str2flt(Sx)
      catch { _ => throw .exception("failed to parse [#(Sx)] as float")}
    ).
  }

  public implementation coercion[string,integer->>exception] => {
    _coerce(Sx) => (try
      _str2int(Sx)
      catch { _ => throw .exception("failed to parse [#(Sx)] as integer")}
    ).
  }

  public implementation coercion[integer,integer->>void] => {
    _coerce(X) => X.
  }

  public implementation coercion[integer,float->>exception] => {
    _coerce(Ix) => (try
      _int2flt(Ix)
      catch { _ => throw .exception("#(_int2str(Ix)) out of range as float")}
    ).
  }

  public implementation coercion[float,integer->>exception] => {
    _coerce(Dx) => (try
      _flt2int(Dx)
      catch {
	| .eINVAL => throw .exception("#(_flt2str(Dx,8,`g`,.false)) not integral")
	| .eRANGE => throw .exception("#(_flt2str(Dx,8,`g`,.false)) out of range as integer")
      }
    )
  }

  public implementation coercion[float,float->>void] => {
    _coerce(X) => X
  }

  public implementation coercion[bigint,string->>void] => {
    _coerce(B) => _big2str(B).
  }

  public implementation coercion[string,bigint->>exception] => {
    _coerce(S) => ( try _str2big(S)
      catch { _ => throw .exception("failed to parse [#(S)] as bigint")}
    )
  }

  public implementation coercion[integer,bigint->>void] => {
    _coerce(Ix) => _int2big(Ix).
  }

  public implementation coercion[bigint,integer->>exception] => {
    _coerce(Bx) => (try _big2int(Bx)
      catch { _ => throw .exception("#(_big2str(Bx)) cannot be coerced to integer")
      })
  }

  public implementation all a,b,e,f,x ~~ coercion[a,b->>x], coercion[e,f->>x] |= coercion[(a,e),(b,f)->>x] => {
    _coerce((A,B)) => (_coerce(A),_coerce(B)).
  }
}
