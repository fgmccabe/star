test.cnc{
  -- Test set operations
  import star.

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

  main:()=>().
  main() => valof{
    logMsg("parent=$(parent)");
    logMsg("gps=$(gp0("abc"))");
    valis ()
  }
}
  
