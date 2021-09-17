test.m{
  import star.
  import star.script.

  i:all x ~~ (x)=>x.
  i(x) => x.

  fog:all x,y,z ~~ ((y)=>z,(x)=>y) => (x)=>z.
  fog(F,G) => (x)=>F(G(x)).

  double:(integer)=>integer.
  double(x)=>x+x.
  
  main:()=>action[(),()].
  main()=>action{
    assert fog(id,double)(4) == double(4)
  }
}
