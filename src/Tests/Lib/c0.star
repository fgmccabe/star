test.c0{
 -- Test program that does not depend on any other package

  public boolean ::= .true | .false.

  public cons[e] ::= .nil | .cons(e,cons[e]).

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

  public 0 < 1 => .true.
  _ < _ default => .false.
  
  public x + y => _int_plus(x,y).
  
  public x - y => _int_minus(x,y).

  public x * y => _int_times(x,y).

  public (•):all a,b,c ~~ ((b)=>c,(a)=>b)=>(a)=>c.
  F • G => (x)=>F(G(x)).

  inc(X) => X+1.

  public _main(_) => valof{
    FF = inc • inc;

    XX = FF(3);

    valis ()
  }
    
    

--  main:()=>().
--  main()=>().
}
