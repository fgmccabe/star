test.c0{
 -- Test program that does not depend on any other package

  public boolean ::= .true | .false.

  public pp[a] ::= pp{C:integer} |
    pq{C:integer. A:a}.

  kk ::= kk{C:integer}.

  aa ::= aa{A:integer}.

  contract all a,b ~~ lc[a->>b] ::= {
    ll:(a)=>b
  }

  implementation all e ~~ lc[pp[e]->>integer] => {
    ll(pp{C=XX}) => XX.
    ll(pq{C=YY}) => YY.
  }

  tt ::= .tt(integer).

  unTT(.tt(X)) => X.

  public cont:(integer)=>pp[()].
  cont(C) => pp{
    C=C
  }

  0 < 1 => .true.
  _ < _ default => .false.
  
  x + y => _int_plus(x,y).

  x - y => _int_minus(x,y).

  x * y => _int_times(x,y).

  fact:(integer)=>integer.
  fact(X) => valof{
    F := 1;
    I := 0;
    while I! < X do{
      F := F!*I!;
      I := I!+1;
    };
    valis F!
  }

  rcf:(integer)=>integer.
  rcf(1) => 1.
  rcf(X) => X*rcf(X-1).

--  main:()=>().
--  main()=>().
}
