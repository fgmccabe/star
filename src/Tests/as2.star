test.as2{
  import star.
  import star.assert.

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
	catch () in {
	  _ => .false
	}
      ).
    check(_,_) default => reset().
  .} in check.

  notMuch:raises () |: ()=>().
  notMuch()=>valof{
    showMsg("hello there");
    raise ()
  }

  main:()=>().
  main()=>valof{
    try{
      assert checkLists()([1,2],[1,2]);
      assert ~checkLists()([],[1]);
      show notMuch();
    } catch  () in {
      _ => showMsg("as expected")
    };
    valis ()
  }
}
