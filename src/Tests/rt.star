test.rt{
  import star.
  import star.script.

  -- Test indexed tuple access

  public main:()=>().
  main()=>valof{
    T = ("alpha",1,5.3,());

    show T.0;

    assert T.1==1;
    
    valis ()
  }

}
