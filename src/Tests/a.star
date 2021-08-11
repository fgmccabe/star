test.a{
  import star.
  import star.script.

/*  o : action[(),integer].
  o = action{
    valis 1
  }

  p : action[(),integer].
  p = o >>= double.


  double:(integer) => action[(),integer].
  double(I) => action{
    valis I+I
  }
*/

  main:()=>action[(),()].
  main()=>action{
    assert 3=~=2
--    assert valof o==1
/*
    assert valof p == 4;
    assert valof a == 4
*/
  }
  
/*  a = do{
    x .= valof p;
    valis x
  }
*/
}
