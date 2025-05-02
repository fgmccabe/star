test.art{
  import star.
  import star.assert.

  -- Use tuples instead of records for contracts for integers
  -- (plus,minus,times,div,zer,unum)
  all a ~~ four[a] ~> ((a,a)=>a,(a,a)=>a,(a,a)=>a,(a,a)=>a,a,a).
  plus:all a ~~ (four[a])=>(a,a)=>a.
  plus(F) => F.0.
  
  minus:all a ~~ (four[a])=>(a,a)=>a.
  minus(F) => F.1.

  times:all a ~~ (four[a])=>(a,a)=>a.
  times(F) => F.2.

  div:all a ~~ (four[a])=>(a,a)=>a.
  div(F) => F.3.

  zer:all a ~~ (four[a])=>a.
  zer(F) => F.4.

  unum:all a ~~ (four[a])=>a.
  unum(F) => F.5.
  
  -- Equality contract tuple
  -- (eq)
  all a ~~ eqq[a] ~> (((a,a)=>boolean)).

  eq:all a ~~ (eqq[a])=>(a,a)=>boolean.
  eq(E) => E.0.

  integer_four:four[integer].
  integer_four = ((X,Y)=>_int_plus(X,Y),
    (X,Y)=>_int_minus(X,Y),
    (X,Y)=>_int_times(X,Y),
    (X,Y) => (try _int_div(X,Y) catch {_ => 0}),
    0,
    1).

  integer_eqq:eqq[integer].
  integer_eqq = (((X,Y)=>_int_eq(X,Y))).

  float_four:four[float].
  float_four = ((X,Y)=>_flt_plus(X,Y),
    (X,Y)=>_flt_minus(X,Y),
    (X,Y)=>_flt_times(X,Y),
    (X,Y) => (try _flt_div(X,Y) catch {_ => 0.0}),
    0.0,
    1.0).

  float_eqq:eqq[float].
  float_eqq = (((X,Y)=>_flt_eq(X,Y))).
  
  ff:all x ~~ (F:four[x]),(Q:eqq[x]) |:(x)=>x.
  ff(X) where eq(Q)(X,zer(F)) =>unum(F).
  ff(N) => times(F)(N,ff(minus(F)(N,unum(F)))).

  fi:(integer)=>integer.
  fi(X) where eq(integer_eqq)(X,zer(integer_four)) =>unum(integer_four).
  fi(N) => times(integer_four)(N,fi(minus(integer_four)(N,unum(integer_four)))).

  ft:(float)=>float.
  ft(X) where eq(float_eqq)(X,zer(float_four)) =>unum(float_four).
  ft(N) => times(float_four)(N,ft(minus(float_four)(N,unum(float_four)))).

--  sample = times(plus(2,3),unum).

  public main:()=>().
  main()=>valof{
    show fi(5);
    show (let{
      F=float_four.
      Q=float_eqq
      } in ff(5.0));
    assert fi(5)==120;
    assert ft(5.0)==120.0;
    valis ()
  }
}
  
