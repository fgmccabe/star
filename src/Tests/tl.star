test.tl{
  import star.
  import star.assert.
  
  -- Test the treelist implementation

  T1 : cons[integer].
  T1 = foldRight((Ix,L)=>.cons(Ix,L),[],1..<5).

  main:() => ().
  main() => valof{
    show T1;
    valis ()
  }
}
