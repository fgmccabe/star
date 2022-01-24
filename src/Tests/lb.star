test.lb{
  import star.
  import star.script.

  count(A,B) => let{.
    R : ref integer.
    R = ref 0.

    inc() => result{
      R := R!+1;
      valis ()
    }

    reset() => action{
      R := 0;
      valis ()
    }
  .} in let{.
    ii(N) where N>=0 => valof{
      inc();
      valis .true
    }
    ii(N) where N<0 => valof{
      _ .= valof reset();
      valis .false
    }
  .} in ii(A).

  main:()=>action[(),()].
  main()=>action{
    assert count(10,0)
  }
}
  
  
      
