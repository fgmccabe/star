test.q2{
  import star.
  import star.iterable.
  import star.script.

  -- Test simple query rules

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab")].

  pm:cons[string].
  pm = {X | (X,Y) in parent && "ab".=Y}.

  main:() => ().
  main() => valof{
    show pm;
    valis ()
  }
}
