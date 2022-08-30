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

  contract all e ~~ eqq[e] ::= {
    eqq:(e,e)=>boolean.
  }
  
  implementation four[integer] => {
    plus(X,Y) => _int_plus(X,Y).
    minus(X,Y) => _int_minus(X,Y).
    times(X,Y) => _int_times(X,Y).
    div(X,Y) => _int_div(X,Y).
    zer = 0.
    unum = 1.
  }.

  implementation eqq[integer] => {
    eqq(X,Y) => _int_eq(X,Y)
  }

/*  person ::= someOne{
    name:string
  }

  ff:all x ~~ four[x],eqq[x] |:(x)=>x.
  ff(X) where eqq(X,zer) =>unum.
  ff(N) => times(N,ff(minus(N,unum))).
  */

  fi:(integer)=>integer.
  fi(X) where eqq(X,zer) =>unum.
  fi(N) => times(N,fi(minus(N,unum))).

--  sample = times(plus(2,3),unum).

  public main:()=>().
  main()=>valof{
--    Peter = someOne{name="peter"};
--    show Peter.name;
--    assert sample==5;
    show fi(5);
    valis ()
  }
}
