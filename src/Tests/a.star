test.a{
  import star.
  import star.script.

  o : action[integer,()].
  o = action{
    valis 1
  }

  p : action[integer,()].


  double:(integer) => action[integer,()].
  double(I) => action{
    valis I+I
  }

  main:()=>action[(),()].
  main()=>do{
    assert valof o==1;

    assert _perform(p) == 4;
    assert valof a == 4
  }
  
  a = do{
    x .= valof p;
    valis x
  }
}
