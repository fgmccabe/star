test.lb{
  import star.
  import star.script.

  count(A,B) => let{.
    R : ref integer.
    R = ref 0.

    inc() => valof{
      R := R!+1;
      valis ()
    }

    reset() => valof{
      R := 0;
      valis ()
    }
  .} in let{.
    ii(N) where N>=0 => valof{
      try{
	inc();
      } catch () in {
	_ => {}
      };
      valis .true
    }
    ii(N) where N<0 => valof{
      reset();
      valis .false
    }
  .} in ii(A).

  main:()=>().
  main()=>valof{
    assert count(10,0);
    valis ()
  }
}
  
  
      
