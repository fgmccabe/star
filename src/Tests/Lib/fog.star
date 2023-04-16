test.fog{
  public id:all a ~~ (a)=>a.
  id(X) => X.

  public (•):all a,b,c ~~ ((b)=>c,(a)=>b)=>(a)=>c.
  F • G => (x)=>F(G(x)).

  public K:all a,b ~~ (a) => (b)=>a.
  K(C) => (_)=>C.

  public fog:all a,b,c ~~ ((b)=>c,(a)=>b)=>(a)=>c.
  fog = (•)
}
