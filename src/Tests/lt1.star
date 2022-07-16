test.lt1{
  import star.
  import star.script.

  l1[integer] ::= l1(integer).

  ff:(integer) => (integer)=>integer.
  ff(X) => let{.
    f(0) => 0.
    f(A) => g(X,l1(A)).

    exec(l1(Fr)) => f(Fr-1).

    g(U,FF) => exec(FF)
  .} in f.

  main:()=>().
  main() => valof{
    K = ff(2);

    assert K(3)==0;
    valis ()
  }
}
