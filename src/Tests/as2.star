test.as2{
  import star.
  import star.script.

  checkLists() => let{.
    foo : ref integer.
    foo = ref 0.

    reset:()=>boolean.
    reset() => valof{
      foo := 0;
      valis .false
    }

    mark:(integer)=>boolean.
    mark(X) => valof{
      foo := X;
      valis .true
    }
  .} in let{.
    check:(cons[integer],cons[integer])=>boolean.
    check([],[]) => .true.
    check([E,..Es],[D,..Ds]) =>
      ( try D==E ??
	  mark(D) && check(Es,Ds) ||
	  reset()
	catch {
	  _ => .false
	}
      ).
    check(_,_) default => reset().
  .} in check.

  notMuch:()=>() throws ().
  notMuch()=>valof{
    logMsg("hello there");
    throw ()
  }

  main:()=>().
  main()=>valof{
    try{
      assert checkLists()([1,2],[1,2]);
      assert ~checkLists()([],[1]);
      show notMuch();
    } catch {
      _ => logMsg("as expected")
    };
    valis ()
  }
}
