test.ar{
  import star.
--  import star.script.

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
  }.
}
