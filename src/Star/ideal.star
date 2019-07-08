star.ideal{
  import star.core.
  import star.arith.
  import star.bits.
  import star.cons.
  import star.lists.
  import star.coerce.
  import star.collection.
  import star.iterable.
  import star.tuples.
  import star.monad.

  -- See "Ideal Hash Trees" by Phil Bagwell

  public  all k,v ~~ map[k,v] ::=   -- Expose the type only
    private ihEmpty | -- Empty dictionary
    -- Leaf dictionary, all entries have the same hash
    private ihLeaf(integer,cons[keyval[k,v]]) | 
    private ihNode(map[k,v],map[k,v],map[k,v],map[k,v]). -- non-leaf case
  
  findIdeal: all k,v ~~ equality[k],hash[k] |: (map[k,v],k) => option[v].
  findIdeal(Tr,Ky) => findInTree(0,hash(Ky),Ky,Tr).

  findInTree:all k,v ~~ equality[k] |: (integer,integer,k,map[k,v]) => option[v].
  findInTree(_,Hash,Ky,ihLeaf(Hash,Els)) => findMember(Ky,Els).
  findInTree(Dpth,Hash,Ky,Sub) where ihNode(_,_,_,_) .= Sub =>
    findInTree(Dpth+2,Hash,Ky,pickSub(Sub,subKey(Hash,Dpth))).
  findInTree(_,_,_,_) default => none.

  findMember:all k,v ~~ equality[k] |: (k,cons[keyval[k,v]])=>option[v].
  findMember(K,cons(Ky->V,_)) where K==Ky => some(V).
  findMember(K,cons(_,L)) => findMember(K,L).
  findMember(_,nil) => none.

  insertIdeal:all k,v ~~ equality[k],hash[k] |: (map[k,v],k,v)=>map[k,v].
  insertIdeal(Tr,K,V) => insertTree(0,hash(K),Tr,K,V).

  insertTree:all k,v ~~ equality[k] |: (integer,integer,map[k,v],k,v) => map[k,v].
  insertTree(Dpth,Hash,ihEmpty,Ky,Vl) => ihLeaf(Hash,cons(Ky->Vl,nil)).
  insertTree(Dpth,Hash,ihLeaf(Hash,Els),Ky,Vl) => ihLeaf(Hash,mergeLf(Els,Ky,Vl)).
  insertTree(Dpth,Hash,ihLeaf(H,Els),Ky,Vl) =>
    insertTree(Dpth,Hash,singleVec(subKey(H,Dpth),ihLeaf(H,Els)),Ky,Vl).
  insertTree(Dpth,Hash,Sub,Ky,Vl) where Ix.=subKey(Hash,Dpth) =>
    patchVec(Sub,Ix,(Sb)=>insertTree(Dpth+2,Hash,Sb,Ky,Vl)).

  mergeLf:all k,v ~~ equality[k] |: (cons[keyval[k,v]],k,v)=>cons[keyval[k,v]].
  mergeLf(nil,K,V) => cons(K->V,nil).
  mergeLf(cons(K->V,T),K,Vl) => cons(K->Vl,T).
  mergeLf(cons(Pr,T),Ky,Vl) => cons(Pr,mergeLf(T,Ky,Vl)).

  removeIdeal:all k,v ~~ equality[k],hash[k] |: (map[k,v],k)=>map[k,v].
  removeIdeal(Tr,K) => deleteTree(Tr,0,hash(K),K).

  deleteTree:all k,v ~~ equality[k] |: (map[k,v],integer,integer,k)=>map[k,v].
  deleteTree(ihEmpty,_,_,_) => ihEmpty.
  deleteTree(ihLeaf(Hash,Els),_,Hash,K) => reformTree(ihLeaf(Hash,removeMember(K,Els))).
  deleteTree(Sub,Dpth,Hash,K) where Ix.=subKey(Hash,Dpth) =>
    reformTree(patchVec(Sub,Ix,(Sb)=>deleteTree(Sb,Dpth+2,Hash,K))).

  removeMember:all k,v ~~ equality[k] |: (k,cons[keyval[k,v]])=>cons[keyval[k,v]].
  removeMember(K,cons(Ky->_,T)) where K==Ky => T.
  removeMember(K,cons(El,L)) => cons(El,removeMember(K,L)).
  removeMember(_,nil) => nil.

  reformTree:all k,v ~~ (map[k,v]) => map[k,v].
  reformTree(ihEmpty) => ihEmpty.
  reformTree(ihLeaf(_,nil)) => ihEmpty.
  reformTree(ihNode(Tr,ihEmpty,ihEmpty,ihEmpty)) where \+ ihNode(_,_,_,_).=Tr => Tr.
  reformTree(ihNode(ihEmpty,Tr,ihEmpty,ihEmpty)) where \+ ihNode(_,_,_,_).=Tr => Tr.
  reformTree(ihNode(ihEmpty,ihEmpty,Tr,ihEmpty)) where \+ ihNode(_,_,_,_).=Tr => Tr.
  reformTree(ihNode(ihEmpty,ihEmpty,ihEmpty,Tr)) where \+ ihNode(_,_,_,_).=Tr => Tr.
  reformTree(Tr) default => Tr.

  replaceIdeal: all k,v ~~ equality[k],hash[k] |: (map[k,v],k,v) => map[k,v].
  replaceIdeal(Tr,Ky,Vl) => let{
    replaceInTree(ihEmpty,_,Hash) => ihLeaf(Hash,cons(Ky->Vl,nil)).
    replaceInTree(ihLeaf(Hash,Els),_,Hash) => ihLeaf(Hash,replaceInCons(Els)).
    replaceInTree(ihLeaf(H,Els),Dpth,Hash) =>
      insertTree(Dpth,Hash,ihLeaf(H,Els),Ky,Vl).
    replaceInTree(Vect,Dpth,Hash) =>
      patchVec(Vect,subKey(Hash,Dpth),(Sb)=>replaceInTree(Sb,Dpth+2,Hash)).

    replaceInCons(nil) => cons(Ky->Vl,nil).
    replaceInCons(cons(Ky->_,T)) => cons(Ky->Vl,T).
    replaceInCons(cons(P,T)) => cons(P,replaceInCons(T)).
  } in replaceInTree(Tr,0,hash(Ky)).

  pickSub:all k,v ~~ (map[k,v],integer)=>map[k,v].
  pickSub(ihNode(El,_,_,_),0) => El.
  pickSub(ihNode(_,El,_,_),1) => El.
  pickSub(ihNode(_,_,El,_),2) => El.
  pickSub(ihNode(_,_,_,El),3) => El.

  patchVec:all k,v ~~ (map[k,v],integer,(map[k,v])=>map[k,v])=>map[k,v].
  patchVec(ihNode(E0,E1,E2,E3),0,F) => ihNode(F(E0),E1,E2,E3).
  patchVec(ihNode(E0,E1,E2,E3),1,F) => ihNode(E0,F(E1),E2,E3).
  patchVec(ihNode(E0,E1,E2,E3),2,F) => ihNode(E0,E1,F(E2),E3).
  patchVec(ihNode(E0,E1,E2,E3),3,F) => ihNode(E0,E1,E2,F(E3)).

  singleVec:all k,v ~~ (integer,map[k,v]) => map[k,v].
  singleVec(Ix,Tr) => patchVec(emptyVec,Ix,(_)=>Tr).

  emptyVec:all k,v ~~ map[k,v].
  emptyVec = ihNode(ihEmpty,ihEmpty,ihEmpty,ihEmpty).

  subKey:(integer,integer) => integer.
  subKey(Hash,Depth) => _band(_blsr(Hash,Depth),0x3).

  -- Implement some standard contracts

  implementation all k,v ~~ equality[k],equality[v] |: equality[keyval[k,v]] => {.
    K1->V1 == K2->V2 => K1==K2 && V1==V2
  .}

  public implementation all k,v ~~ equality[k],equality[v] |: equality[map[k,v]] => let {
    mapPairs(ihEmpty,L) => L.
    mapPairs(ihLeaf(_,Lf),L) => Lf++L.
    mapPairs(ihNode(A1,A2,A3,A4),L) => mapPairs(A4,mapPairs(A3,mapPairs(A2,mapPairs(A1,L)))).
  } in {.
    T1==T2 => mapPairs(T1,nil)==mapPairs(T2,nil).
  .}

  public implementation all k,v ~~ sizeable[map[k,v]] => let{
    countEls(ihEmpty,Cnt) => Cnt.
    countEls(ihLeaf(_,Els),Cnt) => countCons(Els,Cnt).
    countEls(ihNode(A1,A2,A3,A4),Cnt) => countEls(A4,countEls(A3,countEls(A2,countEls(A1,Cnt)))).

    countCons(nil,Cnt) => Cnt.
    countCons(cons(_,T),Cnt) => countCons(T,Cnt+1).
  } in {
    size(M) => countEls(M,0).
    isEmpty(ihEmpty) => true.
    isEmpty(M) => countEls(M,0)==0.
  }

  public implementation all k,v ~~ display[k],display[v] |: display[map[k,v]] => let{
    dispTree(ihEmpty) => ssSeq([]).
    dispTree(ihLeaf(_,Els)) => ssSeq(dispEls(Els)).
    dispTree(ihNode(A1,A2,A3,A4)) => ssSeq([dispTree(A1),dispTree(A2),dispTree(A3),dispTree(A4)]).

    dispEls(nil)=>[].
    dispEls(cons(K->V,T)) => [disp(K),ss("->"),disp(V),ss(","),..dispEls(T)].
  } in {
     disp(Tr) => ssSeq([ss("["),dispTree(Tr),ss("]")]).
  }

  public implementation all k,v ~~ dump[k],dump[v] |: dump[map[k,v]] => let{
    dispTree:all k,v ~~ dump[k],dump[v] |: (map[k,v],integer) => ss.
    dispTree(ihEmpty,Dp) => ssSeq([spaces(Dp)..,ss("ε"),ss("\n")]).
    dispTree(ihLeaf(H,Els),Dp) => ssSeq([spaces(Dp)..,disp(H),ss(":"),dispEls(Els),ss("\n")]).
    dispTree(ihNode(A1,A2,A3,A4),Dp) =>
      ssSeq([spaces(Dp)..,ss("@"),disp(Dp),ss("\n"),
	  dispTree(A1,Dp),dispTree(A2,Dp),dispTree(A3,Dp),dispTree(A4,Dp)]).

    dispEls:all k,v ~~ dump[k],dump[v] |: (cons[keyval[k,v]]) => ss.
    dispEls(nil) => ss(".").
    dispEls(cons(K->V,T)) => ssSeq([dump(K),ss("->"),dump(V),dispMoreEls(T)]).

    dispMoreEls:all k,v ~~ dump[k],dump[v] |: (cons[keyval[k,v]]) => ss.
    dispMoreEls(nil) => ss(".").
    dispMoreEls(cons(K->V,T)) => ssSeq([ss(","),dump(K),ss("->"),dump(V),dispMoreEls(T)]).

    spaces:(integer) => list[ss].
    spaces(0) => [].
    spaces(X) => [ss(" "),..spaces(X-1)].
  } in {.
    dump(T) => dispTree(T,0).
  .}

  public implementation all k,v ~~ equality[k],hash[k] |: indexed[map[k,v]->>k,v] => {
    _index(Tr,Ky) => findIdeal(Tr,Ky).
    _put(Tr,Ky,Vl) => insertIdeal(Tr,Ky,Vl).
    _remove(Tr,Ky) => removeIdeal(Tr,Ky).
    _empty = ihEmpty.
  }

  public implementation all k,v ~~ equality[k],hash[k] |: coercion[list[(k,v)],map[k,v]] => {
    _coerce(L) => foldRight(((K,V),M)=>insertIdeal(M,K,V),ihEmpty,L).
  }

  public implementation all k,v ~~ coercion[map[k,v],list[keyval[k,v]]] => let{
    pairs(ihEmpty,L) => L.
    pairs(ihLeaf(_,Els),L) => consPairs(Els,L).
    pairs(ihNode(A1,A2,A3,A4),L) => pairs(A4,pairs(A3,pairs(A2,pairs(A1,L)))).

    consPairs(nil,L) => L.
    consPairs(cons(Pr,T),L) => consPairs(T,[L..,Pr]).
  } in {
    _coerce(Tr) => pairs(Tr,[])
  }

  idealFold:all k,v,u ~~ ((k,v,u)=>u,u,map[k,v]) => u.
  idealFold(_,u,ihEmpty) => u.
  idealFold(f,u,ihLeaf(_,Els)) => foldLeafs(Els,f,u).
  idealFold(f,u,ihNode(A1,A2,A3,A4)) => idealFold(f,idealFold(f,idealFold(f,idealFold(f,u,A1),A2),A3),A4).

  foldLeafs:all m,k,v,f,u ~~ (cons[keyval[k,v]],(k,v,u)=>u,u) => u.
  foldLeafs(nil,_,u)=>u.
  foldLeafs(cons(k->v,l),f,u) => foldLeafs(l,f,f(k,v,u)).

  public implementation ixmap[map] => let {
    ixMap:all k,v,w ~~ (map[k,v],(k,v)=>w) => map[k,w].
    ixMap(ihEmpty,_) => ihEmpty.
    ixMap(ihNode(A1,A2,A3,A4),f) => ihNode(ixMap(A1,f),ixMap(A2,f),ixMap(A3,f),ixMap(A4,f)).
    ixMap(ihLeaf(Hash,Els),f) => ihLeaf(Hash,applyF(Els,f)).

    private applyF:all k,v,w ~~ (cons[keyval[k,v]],(k,v)=>w)=>cons[keyval[k,w]].
    applyF(nil,_) => nil.
    applyF(cons(K->V,L),f) => cons(K->f(K,V),applyF(L,f)).
  }
  in{
    (M///f) => ixMap(M,f).
  }

  public implementation all k,v ~~ equality[k],hash[k] |: ixfilter[map[k,v]->>k,v] => {
    M^//p => ixFilter(M,p).
  }

  ixFilter:all k,v ~~ equality[k],hash[k] |: (map[k,v],(k,v)=>boolean) => map[k,v].
  ixFilter(M,P) => idealFold((k,v,N)=>checkEntry(k,v,N,P),ihEmpty,M).

  checkEntry:all k,v ~~ equality[k],hash[k] |: (k,v,map[k,v],(k,v)=>boolean) => map[k,v].
  checkEntry(K,V,So,P) where P(K,V) => insertIdeal(So,K,V).
  checkEntry(_,_,So,_) => So.

  public implementation all k,v ~~ ixfold[map[k,v]->>k,v] => let{
    idealRight(F,U,ihEmpty) => U.
    idealRight(F,U,ihLeaf(_,Els)) => consIxRight(F,U,Els).
    idealRight(F,U,ihNode(A1,A2,A3,A4)) => idealRight(F,idealRight(F,idealRight(F,idealRight(F,U,A4),A3),A2),A1).

    consIxRight(F,U,nil) => U.
    consIxRight(F,U,cons(K->V,T)) => F(K,V,consIxRight(F,U,T)).

    idealLeft(F,U,ihEmpty) => U.
    idealLeft(F,U,ihLeaf(_,Els)) => consIxLeft(F,U,Els).
    idealLeft(F,U,ihNode(A1,A2,A3,A4)) => idealLeft(F,idealLeft(F,idealLeft(F,idealLeft(F,U,A1),A2),A3),A4).

    consIxLeft(F,U,nil) => U.
    consIxLeft(F,U,cons(K->V,T)) => consIxLeft(F,F(U,K,V),T).
  } in {.
    ixRight = idealRight.
    ixLeft = idealLeft.
  .}

  public implementation all k,v ~~ iter[map[k,v]->>keyval[k,v]] => let{
    iter:all k,v,m/2,e,x ~~ execution[m] |: (map[k,v],m[e,x],(keyval[k,v],x)=>m[e,x])=>m[e,x].
    iter(ihEmpty,St,_) => St.
    iter(ihLeaf(_,Els),St,Fn) => consIter(Els,St,Fn).
    iter(ihNode(A1,A2,A3,A4),St,Fn) =>
      iter(A4,iter(A3,iter(A2,iter(A1,St,Fn),Fn),Fn),Fn).

    consIter:all el,m/2,e,x ~~ execution[m] |: (cons[el],m[e,x],(el,x)=>m[e,x])=>m[e,x].
    consIter(nil,S,_) => S.
    consIter(cons(E,T),S,F) => _sequence(S,(SS)=>consIter(T,F(E,SS),F)).
  } in {
    _iter(Tr,St,Fn) => iter(Tr,St,Fn)
  }

  public implementation all k,v ~~ hash[k], equality[k] |: sequence[map[k,v] ->> keyval[k,v]] => {
    _nil = ihEmpty.
    _cons(K->V,Tr) => insertIdeal(Tr,K,V).
    _apnd(Tr,K->V) => insertIdeal(Tr,K,V).
  }

  public implementation all k ~~ hash[k],equality[k] |: functor[map[k]] => let{
    fm:all a,b ~~ ((a)=>b,map[k,a]) => map[k,b].
    fm(_,ihEmpty) => ihEmpty.
    fm(F,ihLeaf(H,Els)) => ihLeaf(H,Els//((K->V)=>(K->F(V)))).
    fm(F,ihNode(A1,A2,A3,A4)) => ihNode(fm(F,A1),fm(F,A2),fm(F,A3),fm(F,A4)).
  } in {.
    fmap = fm.
    C <$ L => fm((_)=>C,L).
  .}

  public implementation all k,v ~~ stream[map[k,v]->>keyval[k,v]] => let{
    drop(ihLeaf(H,[_])) => ihEmpty.
    drop(ihLeaf(H,[_,..Es])) => ihLeaf(H,Es).
    drop(ihNode(ihEmpty,ihEmpty,ihEmpty,A4)) => drop(A4).
    drop(ihNode(ihEmpty,ihEmpty,A3,A4)) => ihNode(ihEmpty,ihEmpty,drop(A3),A4).
    drop(ihNode(ihEmpty,A2,A3,A4)) => ihNode(ihEmpty,drop(A2),A3,A4).
    drop(ihNode(A1,A2,A3,A4)) => ihNode(drop(A1),A2,A3,A4).

    hd(ihLeaf(_,[E,.._])) => some(E).
    hd(ihNode(ihEmpty,ihEmpty,ihEmpty,A4)) => hd(A4).
    hd(ihNode(ihEmpty,ihEmpty,A3,A4)) => hd(A3).
    hd(ihNode(ihEmpty,A2,A3,A4)) => hd(A2).
    hd(ihNode(A1,A2,A3,A4)) => hd(A1).
    hd(_) default => none.

    hdtl(Tr) where H^=hd(Tr) => some((H,drop(Tr))).
    hdtl(_) default => none.
  } in {.
    _eof(ihEmpty) => true.
    _eof(_) default => false.

    _hdtl(Tr) => hdtl(Tr).

    _back(Tr) where (H,T)^=hdtl(Tr) => some((T,H)).
    _back(_) default => none.
  .}
}
