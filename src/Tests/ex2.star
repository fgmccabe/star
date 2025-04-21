test.ex2{
  import star.
  import star.assert.

  main:()=>().
  main()=>valof{
    try{
      show sqrt(10.4);
      assert ?sqrt(-1.0) == 0.0;
    } catch {
      ErrCode => { showMsg("out with a $(ErrCode)"); valis () }
    };
  }
}
