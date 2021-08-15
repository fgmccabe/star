test.as2{
  import star.
  import star.script.

  checkLists() => let{
    foo : ref integer.
    foo := 0.

    reset = action{
      foo := 0;
      valis .false
    }

    mark(X) => action{
      foo := X;
      valis .true
    }
  } in let{
    check:(cons[integer],cons[integer])=>boolean.
    check([],[]) => .true.
    check([E,..Es],[D,..Ds]) =>
      ( D==E ?
	  valof mark(D) && check(Es,Ds) ||
	  valof reset
      ).
    check(_,_) default => valof reset.
  } in check.

  main:()=>action[(),()].
  main()=>action{
    assert checkLists()([1,2],[1,2]);
    assert ~checkLists()([],[1])
  }
}
