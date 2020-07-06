test.ar{
  import star.
  import star.script.

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

  plus(X,Y) => _int_plus(X,Y).
  minus(X,Y) => _int_minus(X,Y).
  times(X,Y) => _int_times(X,Y).

  ff(0)=>1.
  ff(N) => times(N,ff(minus(N,1))).

  sample = times(plus(2,3),one).

  public main:()=>action[(),()].
  main()=>do{
    assert sample==6;
    show ff(5)
  }
}
