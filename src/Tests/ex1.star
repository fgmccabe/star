test.ex1{
  import star.
  import star.assert.

  throwOne:(integer) => either[boolean,exception].
  throwOne(X) => ?? ( X>=0 ?? .true || throw .exception("bing")).

  main:()=>().
  main()=>valof{
    try{
      assert ?throwOne(-1);
    } catch {
      .exception(Msg) => { showMsg("out with a #(Msg)"); valis () }
    };
  }
}
