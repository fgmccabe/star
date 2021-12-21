test.ar{
  import star.
  import star.script.

  -- implement standard contracts for integers
  contract all a ~~ four[a] ::= {
    plus:(a,a)=>a.
    minus:(a,a)=>a.
    times:(a,a)=>a.
    div:(a,a)=>a.
    zer:a.
    unum:a.
  }
  
  implementation four[integer] => {
    plus(X,Y) => _int_plus(X,Y).
    minus(X,Y) => _int_minus(X,Y).
    times(X,Y) => _int_times(X,Y).
    div(X,Y) => _int_div(X,Y).
    zer = 0.
    unum = 1.
  }.

  implementation four[bigint] => {
    plus(X,Y) => _big_plus(X,Y).
    minus(X,Y) => _big_minus(X,Y).
    times(X,Y) => _big_times(X,Y).
    div(X,Y) where (q,_) .= _big_div(X,Y) => q.
    zer = 0b.
    unum = 1b.
  }

  ff:all x ~~ four[x],equality[x] |:(x)=>x.
  ff(zer)=>unum.
  ff(N) => times(N,ff(minus(N,unum))).

  sample = times(plus(2,3),unum).

  gcd(A,B) => _big_gcd(A,B).

  public main:()=>action[(),()].
  main()=>action{
    assert sample==5;
    show ff(5);
    show ff(50b);
    assert ff(50b) == 30414093201713378043612608166064768844377641568960512000000000000b;
    assert gcd(1071b,462b)==21b
  }
}
