test.u2{
  import star.
  import star.script.
  import star.uri.
  import star.uri.grammar.

  main:()=>().
  main()=>valof{
    show _stringOf(parseU("https://fred@foo.bar.com:9090/local/path?query"::cons[char]),10);
    valis ()
  }
}
