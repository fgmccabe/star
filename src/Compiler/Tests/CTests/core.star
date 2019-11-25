star.core{
  -- special core to use in emergency
  public boolean ::= true | false.

  public contract all x ~~ equality[x] ::= {
    (==): (x,x)=>boolean.
  }

  public implementation equality[integer] => {
    X == Y => _int_eq(X,Y).
  }

  public (=!=):all x ~~ equality[x] |: (x,x)=>boolean.
  x =!= y => \+ x==y.

  public maybe[t] ::= impossible | just(t).

  public notEq:all x ~~ equality[x] |: (x,x)=>boolean.
  notEq(u,v) => u=!=v.

  notOne:(integer)=>boolean.
  notOne(X) => X=!=1.
}
