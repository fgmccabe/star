test.a{
  import star.
  import star.assert.

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

      logMsg("\e[33m#("yellow")\e[0m");
      logMsg("\e[31mred\e[0m");
      
      raise .exception("bong");
    } catch exception in {
      .exception(Msg) => { logMsg("out with a #(Msg)"); valis () }
    };
  }
}
