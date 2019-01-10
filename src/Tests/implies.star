test.implies{
  import star.
  import star.iterable.

  pars : list[(string,string)].
  pars = [("a","b"),("c","b"),("b","d"),("e","d"),("f","e")].

  ms : list[string].
  ms = ["b","a"].

  onlySons(P) => (P,S) in pars *> S in ms.

  assert onlySons("a").

  show disp(onlySons("a")).
  show disp(onlySons("f")).

  assert \+onlySons("f").

  show disp([X | (X,C) in pars *> C in ms]).

  _main:(list[string])=>().
  _main([])=>().
}
