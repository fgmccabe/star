test.ex0{
  import star.
  import star.assert.

  main:()=>().
  main()=>valof{
    try{
      assert 3~=2;
      
      throw .exception("bong");
    } catch {
      .exception(Msg) => { showMsg("out with a #(Msg)"); valis () }
    };
  }
}
