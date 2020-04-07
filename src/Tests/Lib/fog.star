test.fog{
  boolean ::= .true | .false.

  public id:all a ~~ (a)=>a.
  id(X) => X.

  public (•):all a,b,c ~~ ((b)=>c,(a)=>b)=>(a)=>c.
  F • G => (x)=>F(G(x)).

  K:all a ~~ (a) => ()=>a.
  K(C) => ()=>C.
}
