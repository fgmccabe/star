test.gp{
  import star.
  import star.iterable.

  -- Various ways of doing grandparents

  parent:list[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  show disp(parent).

  -- First a hacky way (i.e. direct recursive) way of finding grandparents
  findPs:(list[(string,string)],string,list[string]) => list[string].
  findPs([],_,Cs) => Cs.
  findPs([(P,C),..Ps],C,Cs) => findPs(Ps,C,addP(P,Cs)).
  findPs([_,..Ps],C,Cs) => findPs(Ps,C,Cs).

  addP:(string,list[string]) => list[string].
  addP(P,[]) => [P].
  addP(P,[P,..X]) => [P,..X].
  addP(P,[O,..X]) => [O,..addP(P,X)].

  findGs:(list[string],list[string]) => list[string].
  findGs([],S) => S.
  findGs([P,..Ps],S) => findGs(Ps,findPs(parent,P,S)).

  gp0:(string) => list[string].
  gp0(GC) => findGs(findPs(parent,GC,[]),[]).

  show disp(gp0("abc")).

  -- Using fold over lists
  fPs:(list[(string,string)],string,list[string]) => list[string].
  fPs(Ps,Ch,S) => foldRight(let{
    rightPr((P,Ch),So) => addP(P,So).
    rightPr(_,So) => So.
  } in rightPr,S,Ps).

  fGps:(list[string],list[string]) => list[string].
  fGps(Ps,S) => foldRight((P,So)=>fPs(parent,P,So),S,Ps).

  gpF:(string) => list[string].
  gpF(GC) => fGps(fPs(parent,GC,[]),[]).

  show disp(gpF("abc")).

  -- Translated from the query rule:
  -- gp(X,Y) <- parent(X,Z) && parent(Z,Y).

  qP1:()=>set[(string,string)].
  qP1() => { (X,Y) | (X,Z) in parent && (Z,Y) in parent }.

  show disp(qP1()).

  -- As though translated from the query rule:
  -- gc(X) given (Y) <- parent(X,Z) && parent(Z,Y).

  qC2:(string) => list[string].
  qC2(Y) => { X | (X,Z) in parent && (Z,Y) in parent }.

  show disp(qC2("abc")).

  -- By changing the order of the calls, it is more efficient

  qC3:(string)=>list[string].
  qC3(Y) => { X | (Z,Y) in parent && (X,Z) in parent }.

  show disp(qC3("abc")).
}
