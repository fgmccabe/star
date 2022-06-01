test.c0{
 -- Test program that does not depend on any other package

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

  tt ::= tt(integer).

  unTT(tt(X)) => X.

  public cont:(integer)=>pp[()].
  cont(C) => pp{
    C=C
  }
}
