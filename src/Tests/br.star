test.br{
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
    div(X,Y) => (try _int_div(X,Y) catch errorCode in {_ => 0}).
    zer = 0.
    unum = 1.
  }.

  implementation four[bigint] => {
    plus(X,Y) => _big_plus(X,Y).
    minus(X,Y) => _big_minus(X,Y).
    times(X,Y) => _big_times(X,Y).
    div(X,Y) => (try
      _big_div(X,Y).0
      catch errorCode in {
	_ => 0b0
      }).
    zer = 0b0.
    unum = 0b1.
  }

  ff:all x ~~ four[x],equality[x] |:(x)=>x.
  ff(zer)=>unum.
  ff(N) => times(N,ff(minus(N,unum))).

  sample = times(plus(2,3),unum).

  gcd(A,B) => (try _big_gcd(A,B) catch errorCode in {_ => A}).

  public main:()=>().
  main()=>valof{
    assert sample==5;
    show ff(5);
    show ff(0b50);
    assert ff(0b50) == 0b30414093201713378043612608166064768844377641568960512000000000000;
    assert gcd(0b1071,0b462)==0b21;
    valis ()
  }
}
