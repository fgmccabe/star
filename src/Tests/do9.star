test.do9{
  import star.
  import star.script.

  qName:(string) => integer.
  qName(N) => size(N).

  t ::= f(integer).

  gen:(string,cons[t])=>cons[integer].
  gen(P,D) where MM .= qName(P) && {? f(MM) in D ?} => [].

  main:()=>().
  main()=>valof{
    show qName("one");
    show gen("one",[f(3),f(5)]);
    valis ()
  }
}
