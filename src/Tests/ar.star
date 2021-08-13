test.ar{
  import star.
  import star.script.

  -- implement standard contracts for integers
  contract all a ~~ four[a] ::= {
    plus:(a,a)=>a.
    minus:(a,a)=>a.
    times:(a,a)=>a.
    div:(a,a)=>a.
  }
  
  implementation four[integer] => {
    plus(X,Y) => _int_plus(X,Y).
    minus(X,Y) => _int_minus(X,Y).
    times(X,Y) => _int_times(X,Y).
    div(X,Y) => _int_div(X,Y).
  }.

  ff(0)=>1.
  ff(N) => times(N,ff(minus(N,1))).

  sample = times(plus(2,3),one).

  public main:()=>action[(),()].
  main()=>action{
    assert sample==5;
    show ff(5)
  }
}
