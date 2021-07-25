test.cnc{
  -- Test set operations
  import star.core.
  import star.arith.
  import star.strings.
  import star.display.
  import star.option.
  import star.coerce.
  import test.cn2.

  -- stream & sequence contracts
  public implementation all x ~~ stream[cons[x] ->> x] => {
    _eof(.nil) => .true.
    _eof(cons(_,_)) => .false.
    
    _hdtl(cons(H,T)) => some((H,T)).
    _hdtl(.nil) => .none.
  }

  public implementation all x ~~ sequence[cons[x] ->> x] => {
    _cons(E,S) => cons(E,S).
    _nil = .nil.
  }

  public implementation all e ~~ display[e] |: display[cons[e]] => let{
    consDisp(.nil) => ss("").
    consDisp(cons(X,.nil)) => disp(X).
    consDisp(cons(X,R)) => ssSeq([disp(X), ss(","), consDisp(R)]).
  } in {
    disp(L) => ssSeq([ss("["), consDisp(L),ss("]")]).
  }

  public implementation all x,y ~~ display[x], display[y] |: display[(x,y)] =>
    {.
      disp((a,b)) => ssSeq([ss("("),disp(a),ss(" , "),disp(b),ss(")")]).
    .}

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
    ("de","abc"),("d","de"),("e","de"),
    ("f","a"),("g","f")].

  -- First a hacky way (i.e. direct recursive) way of finding grandparents
  findPs:(cons[(string,string)],string,cons[string]) => cons[string].
  findPs([],_,Cs) => Cs.
  findPs([(P,C),..Ps],C,Cs) => findPs(Ps,C,addP(P,Cs)).
  findPs([_,..Ps],C,Cs) => findPs(Ps,C,Cs).

  addP:(string,cons[string]) => cons[string].
  addP(P,[]) => [P].
  addP(P,[P,..X]) => [P,..X].
  addP(P,[O,..X]) => [O,..addP(P,X)].

  findGs:(cons[string],cons[string]) => cons[string].
  findGs([],S) => S.
  findGs([P,..Ps],S) => findGs(Ps,findPs(parent,P,S)).

  gp0:(string) => cons[string].
  gp0(GC) => findGs(findPs(parent,GC,[]),[]).

  main:()=>action[(),()].
  main() => action{
    logM("parent=$(parent)");
    logM("gps=$(gp0("abc"))");
    valis ()
  }
}
  
