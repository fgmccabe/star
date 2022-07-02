test.lb{
  import star.
  import star.script.

  count(A,B) => let{.
    R : ref integer.
    R = ref 0.

    inc() => do{
      R := R!+1;
      valis ()
    }

    reset() => do{
      R := 0;
      valis ()
    }
  .} in let{.
    ii(N) where N>=0 => valof{
      try{
	do inc();
      } catch {
	_ => {}
      };
      valis .true
    }
    ii(N) where N<0 => valof{
      try{
	_ .= valof reset();
      } catch { _ => logMsg("bad happening")};
      valis .false
    }
  .} in ii(A).

  main:()=>().
  main()=>valof{
    assert count(10,0);
    valis ()
  }
}
  
  
      
