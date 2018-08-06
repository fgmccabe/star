sample.gp{
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

  -- Using iterable contract
  iPs:(list[(string,string)],string,iterState[list[string]]) => iterState[list[string]].
  iPs(Ps,Ch,St) => _iterate(Ps,let{
    rightP((P,Ch),continueWith(So)) => continueWith(addP(P,So)).
    rightP((P,Ch),noneFound) => continueWith([P]).
    rightP(_,So) => So.
  } in rightP,St).

  iGp:(iterState[list[string]],iterState[list[string]]) => iterState[list[string]].
  iGp(noneFound,_) => noneFound.
  iGp(continueWith(Ps),St) => _iterate(Ps,(P,So)=>iPs(parent,P,So),St).

  gpI:(string) => iterState[list[string]].
  gpI(GC) => iGp(iPs(parent,GC,noneFound),noneFound).

  show disp(gpI("abc")).
}
