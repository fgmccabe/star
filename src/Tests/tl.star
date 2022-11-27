test.tl{
  import star.
  import star.range.
  import star.script.
--  import star.treelist.

  -- Test the treelist implementation

  T1 : cons[integer].
  T1 = foldRight((Ix,L)=>.cons(Ix,L),[],.range(1,1,100)).

  main:() => ().
  main() => valof{
    show T1;
    valis ()
  }
}
