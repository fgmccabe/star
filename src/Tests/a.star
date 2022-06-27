test.a{
  import star.
  import star.script.

  o : result[void,integer].
  o = do{
    valis 1
  }

  double:(integer) => result[void,integer].
  double(I) => do{
    valis I+I
  }

  main:()=>().
  main()=>valof{
    try{
      assert 3~=2;
      show valof o;
      assert valof o==1;
      
      show valof a;
      assert valof a == 1;
      valis ()
    } catch {
      _ => { valis () }
    };
  }

  a:result[void,integer].
  a = do{
    x .= valof o;
    valis x
  }
}
