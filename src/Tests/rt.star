test.rt{
  import star.
  import star.assert.

  -- Test indexed tuple access

  public main:(){}.
  main(){
    T = ("alpha",1,5.3,());

    show T.0;

    assert T.1==1;
  }
}
