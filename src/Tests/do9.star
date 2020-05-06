test.do9{
  import star.

  qName:(string) => integer.
  qName(_) => 0.

  t ::= f(integer).

  gen:(string,cons[t])=>cons[integer].
  gen(P,D) where MM .= qName(P) && f(MM) in D => [].
}
