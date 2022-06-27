test.as2{
  import star.
  import star.script.

  checkLists() => let{.
    foo : ref integer.
    foo = ref 0.

    reset:result[(),boolean].
    reset = do{
      foo := 0;
      valis .false
    }

    mark:(integer)=>result[(),boolean].
    mark(X) => do{
      foo := X;
      valis .true
    }
  .} in let{.
    check:(cons[integer],cons[integer])=>boolean.
    check([],[]) => .true.
    check([E,..Es],[D,..Ds]) =>
      ( D==E ?
	  valof mark(D) && check(Es,Ds) ||
	  valof reset
      ).
    check(_,_) default => valof reset.
  .} in check.

  main:()=>().
  main()=>valof{
    assert checkLists()([1,2],[1,2]);
    assert ~checkLists()([],[1]);
    valis ()
  }
}
