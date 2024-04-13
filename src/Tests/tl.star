test.tl{
  import star.
  import star.assert.
--  import star.treelist.

  -- Test the treelist implementation

  T1 : cons[integer].
  T1 = foldRight((Ix,L)=>.cons(Ix,L),[],1..<100).

  main:() => ().
  main() => valof{
    show T1;
    valis ()
  }
}
