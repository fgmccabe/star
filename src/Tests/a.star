test.a{
  import star.
  import star.script.

  o : integer.
  o = valof{
    valis 1
  }

  double:(integer) => integer.
  double(I) => valof{
    valis I+I
  }

  main:()=>().
  main()=>valof{
    try{
      assert 3~=2;
      show o;
      assert o==1;
      
      valis ()
    } catch {
      _ => { valis () }
    };
  }
}
