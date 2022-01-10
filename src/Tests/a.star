test.a{
  import star.
  import star.script.

  o : action[(),integer].
  o = action{
    valis 1
  }

  p : action[(),integer].
  p = (o >>= double) >>= double.


  double:(integer) => action[(),integer].
  double(I) => action{
    valis I+I
  }


  main:()=>action[(),()].
  main()=>action{
    assert 3~=2;
    show valof o;
    assert valof o==1;

    show valof p;
    show valof a;
    assert valof p == 4;
    assert valof a == 4
  }

  a:result[(),integer].
  a = do{
    x .= valof p;
    valis x
  }
}
