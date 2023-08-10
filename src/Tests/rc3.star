test.rc3{
  import star.
  import star.script.

  -- Test anonymous brace tuples

  public main:()=>().
  main() => valof{
    AA = {A=10. ii=[1->.false, 2->.true]. private b="hi". c=.nil};

    show AA.A;
    show AA.ii;

    assert isEmpty(AA.c);
  }
}
    
