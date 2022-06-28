test.chr{
  import star.
  import star.script.

  C1 = `a`.
  C2 = `\n`.

  main:()=>().
  main()=>valof{
    show C1;
    assert C2==`\n`;
    valis ()
  }
}
