star.arith{
  import star.core.

  public contract all x ~~ arith[x] ::= {
    (+): (x,x)=>x.
    (-): (x,x)=>x.
    __minus: (x)=>x.
    zero: x.
    (*): (x,x)=>x.
    (/): (x,x)=>x throws exception.
    (%): (x,x)=>x throws exception.
    one:x.
  }.

  -- implement standard contracts for integers
  public implementation arith[integer] => {
    X+Y => _int_plus(X,Y).
    X-Y => _int_minus(X,Y).
    zero = 0.
    X*Y => _int_times(X,Y).
    X/Y => (try _int_div(X,Y) catch {_ => throw .exception("divide by zero")}).
    X%Y => (try _int_mod(X,Y) catch {_ => throw .exception("divide by zero")}).
    one = 1.
    __minus(Ix) => _int_minus(0,Ix).
  }.

  public implementation equality[integer] => {
    X == Y => _int_eq(X,Y).
  }

  public implementation hashable[integer] => {
    hash(X) => _int_hash(X).
  }

  public implementation comp[integer] => {
    X<Y => _int_lt(X,Y).
    X>=Y => _int_ge(X,Y).
  }

  public implementation measured[integer->>integer] => {
    [|_|] => 1.
  }

  public implementation display[integer] => {
    disp(X) => _int2str(X).
  }

  public implementation format[integer] => {
    _format(X,F) => (try _int_format(X,F) catch {_ => "bad format"}).
  }

  public implementation arith[bigint] => {
    X+Y => _big_plus(X,Y).
    X-Y => _big_minus(X,Y).
    zero = 0b0.
    X*Y => _big_times(X,Y).
    X/Y => valof{
      try{
	(p,_) = _big_div(X,Y);
	valis p
      } catch {
	.divZero => throw .exception("divide by zero")
      }
    }
    X%Y => valof{
      try{
	(_,q) = _big_div(X,Y);
	valis q
      } catch {
	.divZero => throw .exception("divide by zero")
      }
    }
    one = 0b1.
    __minus(Ix) => _big_minus(0b0,Ix).
  }
  
  public implementation equality[bigint] => {
    X == Y => _big_eq(X,Y).
  }

  public implementation hashable[bigint] => {
    hash(X) => _big_hash(X).
  }

  public implementation comp[bigint] => {
    X<Y => _big_lt(X,Y).
    X>=Y => _big_ge(X,Y).
  }

  public implementation measured[bigint->>integer] => {
    [|_|] => 1.
  }

  public implementation display[bigint] => {
    disp(B) => _big2str(B).
  }

  public implementation format[bigint] => {
    _format(X,F) => (try _big_format(X,F) catch {_ => "bad format"}).
  }

  -- implement standard contracts for floats
  public implementation arith[float] => {
    X+Y => _flt_plus(X,Y).
    X-Y => _flt_minus(X,Y).
    zero = 0.0.

    X*Y => _flt_times(X,Y).
    X/Y => (try _flt_div(X,Y) catch {_ => throw .exception("divide by zero")}).
    X%Y => (try _flt_mod(X,Y) catch {_ => throw .exception("divide by zero")}).
    one = 1.0.
    __minus(Dx) => _flt_minus(0.0,Dx).
  }

  public implementation equality[float] => {
    X == Y => _flt_eq(X,Y).
  }

  public implementation hashable[float] => {
    hash(X) => _flt_hash(X).
  }

  public implementation comp[float] => {
    X<Y => _flt_lt(X,Y).
    X>=Y => _flt_ge(X,Y).
  }

  public (>) : all t ~~ comp[t] |: (t,t)=>boolean.
  X > Y => Y<X.

  public (=<): all t ~~ comp[t] |: (t,t)=>boolean.
  X =< Y => Y>=X.

  public min: all t ~~ comp[t] |: (t,t)=>t.
  min(X,Y) => X<Y ?? X || Y.

  public max: all t ~~ comp[t] |: (t,t)=>t.
  max(X,Y) => X>Y ?? X || Y.

  public abs: all t ~~ comp[t],arith[t] |: (t)=>t.
  abs(X) => X<zero ?? __minus(X) || X.

  public implementation display[float] => {
    disp(X) => _flt2str(X,8,`g`,.false).
  }

  public implementation format[float] => {
    _format(X,F) => (try _flt_format(X,F) catch {_ => "bad format"}).
  }

  public contract all e ~~ pwr[e] ::= {
    (**) : (e,e)=>e
  }

  public implementation pwr[float] => {
    X**Y => _flt_pwr(X,Y)
  }
}
