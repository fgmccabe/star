test.tms{
  import star.
  import star.script.

  public term ::= intgr(integer)
    | term(string,cons[term]).

  main:()=>action[(),()].
  main()=> do{
    logMsg("start");
    show term("foo",[intgr(0),intgr(1)]) == intgr(2)
  }

  public implementation equality[term] => let{
    eq(intgr(X),intgr(Y)) => X==Y.
    eq(term(O1,A1),term(O2,A2)) => O1==O2 && eqList(A1,A2).
    eq(_,_) default => .false.

    eqList([],[]) => .true.
    eqList([E1,..L1],[E2,..L2]) => eq(E1,E2) && eqList(L1,L2).

  } in {.
    X==Y => eq(X,Y).
  .}

}
