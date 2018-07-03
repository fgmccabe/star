star.arith{
  import star.core.

  public contract all x ~~ arith[x] ::= {
    (+): (x,x)=>x.
    (-): (x,x)=>x.
    __minus: (x)=>x.
    zero: x.
    (*): (x,x)=>x.
    (/): (x,x)=>x.
    (%): (x,x)=>x.
    one:x.
  }

  -- implement standard contracts for integers
  public implementation arith[integer] => {
    X+Y => _int_plus(X,Y).
    X-Y => _int_minus(X,Y).
    zero = 0.
    X*Y => _int_times(X,Y).
    X/Y => _int_div(X,Y).
    X%Y => _int_mod(X,Y).
    one = 1.
    __minus(Ix) => _int_minus(0,Ix).
  }

  public implementation equality[integer] => {
    X == Y => _int_eq(X,Y).
  }

  public implementation hash[integer] => {
    hash(X) => _int_hash(X).
  }

  public implementation comp[integer] => {
    X<Y => _int_lt(X,Y).
    X>=Y => _int_ge(X,Y).
  }

  public implementation display[integer] => {
    disp(X) => ss(_int2str(X,10,0,0c )).
  }

  public implementation format[integer] => {
    frmt(X,F) => ss(_int_format(X,F)).
  }

  -- implement standard contracts for floats
  public implementation arith[float] => {
    X+Y => _flt_plus(X,Y).
    X-Y => _flt_minus(X,Y).
    zero = 0.0.

    X*Y => _flt_times(X,Y).
    X/Y => _flt_div(X,Y).
    X%Y => _flt_mod(X,Y).
    one = 1.0.
    __minus(Ix) => _flt_minus(0.0,Ix).
  }

  public implementation equality[float] => {
    X == Y => _flt_eq(X,Y,1.0e-20).
  }

  public implementation hash[float] => {
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
  min(X,Y) where X<Y => X.
  min(_,Y) => Y.

  public max: all t ~~ comp[t] |: (t,t)=>t.
  max(X,Y) where X>Y => X.
  max(_,Y) => Y.

  public abs: all t ~~ comp[t],arith[t] |: (t)=>t.
  abs(X) where X<zero => __minus(X).
  abs(X) default => X.

  public implementation display[float] => {
    disp(X) => ss(_flt2str(X,0,8,0cg,false)).
  }

  public implementation format[float] => {
    frmt(X,F) => ss(_flt_format(X,F)).
  }

  public (**):(float,float)=>float.
  X**Y => _flt_pwr(X,Y).
}
