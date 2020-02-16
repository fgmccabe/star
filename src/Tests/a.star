test.a{
  import star.
  import star.script.

  o : action[string,integer].
  o = return 1.

  p : action[string,integer].
  p = (o >>= double) >>= double.

  double:(integer) => action[string,integer].
  double(I) => return (I+I).

  main:()=>action[(),()].
  main()=>do{
    assert valof o==1;

    assert _perform(p) == 4;
    assert valof a == 4
  }
  
  a = do{
    x <- p;
    valis x
  }
}
