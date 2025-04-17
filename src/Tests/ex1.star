test.ex1{
  import star.
  import star.assert.

  throwOne:()=>boolean throws exception.
  throwOne() =>
    throw .exception("bing").

  main:()=>().
  main()=>valof{
    try{
      assert throwOne();
    } catch {
      .exception(Msg) => { showMsg("out with a #(Msg)"); valis () }
    };
  }
}
