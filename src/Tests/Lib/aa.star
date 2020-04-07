test.aa{
  import star.core.
  
  public (•):all a,b,c ~~ ((b)=>c,(a)=>b)=>(a)=>c.
  F • G => (x)=>F(G(x)).

  _main(.nil)=>().
}
