test.bench.list{
  import star.
  import star.assert.
  import test.lib.timer.

  list[e] ::= .null | .elem(e,list[e]). -- cloned from definition of cons.

  makeList:(integer) => list[integer].
  makeList(0) => .null.
  makeList(N) => .elem(N,makeList(N-1)).

  next(.elem(_,L)) => L.

  length:all e ~~ (list[e])=>integer.
  length(.null) => 0.
  length(.elem(_,L)) => length(L)+1.

  isShorterThan:all e ~~ (list[e],list[e])=>boolean.
  isShorterThan(.null,.elem(_,_)) => .true.
  isShorterThan(.elem(_,X),.elem(_,Y)) => isShorterThan(X,Y).
  isShorterThan(_,_) default => .false.

  tail:all e ~~ (list[e],list[e],list[e]) => list[e].
  tail(x,y,z) => (isShorterThan(y,x) ??
    tail(tail(next(x),y,z),tail(next(y),z,x),tail(next(z),x,y)) ||
    z).

  implementation all e ~~ display[e] |= display[list[e]] => let{.
    listDisp(.null,L) => L.
    listDisp(.elem(X,.null),L) => .cons(disp(X), L).
    listDisp(.elem(X,R),L) => .cons(disp(X), .cons(",", listDisp(R,L))).
 .} in {
    disp(L) => _str_multicat(.cons("[",listDisp(L,.cons("]",.nil))))
  }
  
  test() => valof{
    Result = tail(makeList(15), makeList(10), makeList(6));
    valis length(Result)
  }

  public listBenchTest:()=>float.
  listBenchTest() => timeOf((){
      Count = 40000;
      for I in 0..<Count do{
	test()==10
      }}).

  main:(integer){}.
  main(Count){
    timer = timer_start(Count, "List tail benchmark");

    for I in 0..<Count do{
      test()==10
    }

    timer_finish(timer);
  }
}
