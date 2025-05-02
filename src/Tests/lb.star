test.lb{
  import star.
  import star.assert.

  count(A,B) => let{.
    R : ref integer.
    R = ref 0.

    inc() => valof{
      R := R!+1;
      valis ()
    }

    reSet() => valof{
      R := 0;
      valis ()
    }
  .} in let{.
    ii(N) where N>=0 => valof{
      try{
	inc();
      } catch {
	_ => {}
      };
      valis .true
    }
    ii(N) where N<0 => valof{
      reSet();
      valis .false
    }
  .} in ii(A).

  main:()=>().
  main()=>valof{
    assert count(10,0);
    valis ()
  }
}
  
  
      
