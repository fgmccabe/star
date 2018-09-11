star.ideal{
  import star.core.
  import star.arith.
  import star.bits.
  import star.cons.
  import star.lists.
  import star.collection.
  import star.tuples.

  -- See "Ideal Hash Trees" by Phil Bagwell

  all k,v ~~ subVect[k,v] ~> (ideal[k,v],ideal[k,v],ideal[k,v],ideal[k,v]).

  public
  all k,v ~~ ideal[k,v] ::=
      ihEmpty                       -- empty dictionary
    | ihLeaf(integer,cons[(k,v)])   -- leaf entries, all have the same hash
    | ihNode(subVect[k,v]).         -- non-leaf case

  public findIdeal: all k,v ~~ equality[k],hash[k] |: (ideal[k,v],k) => option[v].
  findIdeal(Tr,Ky) => findInTree(0,hash(Ky),Ky,Tr).

  findInTree:all k,v ~~ equality[k] |: (integer,integer,k,ideal[k,v]) => option[v].
  findInTree(_,Hash,Ky,ihLeaf(Hash,Els)) => findMember(Ky,Els).
  findInTree(Dpth,Hash,Ky,ihNode(Sub)) =>
    findInTree(Dpth+2,Hash,Ky,pickSub(Sub,subKey(Hash,Dpth))).
  findInTree(_,_,_,_) default => none.

  findMember:all k,v ~~ equality[k] |: (k,cons[(k,v)])=>option[v].
  findMember(K,cons((Ky,V),_)) where K==Ky => some(V).
  findMember(K,cons(_,L)) => findMember(K,L).
  findMember(_,nil) => none.

  public insertIdeal:all k,v ~~ equality[k],hash[k] |: (ideal[k,v],k,v)=>ideal[k,v].
  insertIdeal(Tr,K,V) => insertTree(0,hash(K),Tr,K,V).

  insertTree:all k,v ~~ equality[k] |: (integer,integer,ideal[k,v],k,v) => ideal[k,v].
  insertTree(Dpth,Hash,ihEmpty,Ky,Vl) => ihLeaf(Hash,cons((Ky,Vl),nil)).
  insertTree(Dpth,Hash,ihLeaf(Hash,Els),Ky,Vl) => ihLeaf(Hash,mergeLf(Els,Ky,Vl)).
  insertTree(Dpth,Hash,ihLeaf(H,Els),Ky,Vl) =>
    insertTree(Dpth,Hash,ihNode(singleVec(subKey(H,Dpth),ihLeaf(H,Els))),Ky,Vl).
  insertTree(Dpth,Hash,ihNode(Sub),Ky,Vl) where Ix.=subKey(Hash,Dpth) =>
    ihNode(patchVec(Sub,Ix,(Sb)=>insertTree(Dpth+2,Hash,Sb,Ky,Vl))).

  mergeLf:all k,v ~~ equality[k] |: (cons[(k,v)],k,v)=>cons[(k,v)].
  mergeLf(nil,K,V) => cons((K,V),nil).
  mergeLf(cons((K,V),T),K,Vl) => cons((K,Vl),T).
  mergeLf(cons((K,V),T),Ky,Vl) => cons((K,V),mergeLf(T,Ky,Vl)).

  public removeIdeal:all k,v ~~ equality[k],hash[k] |: (ideal[k,v],k)=>ideal[k,v].
  removeIdeal(Tr,K) => deleteTree(Tr,0,hash(K),K).

  deleteTree:all k,v ~~ equality[k] |: (ideal[k,v],integer,integer,k)=>ideal[k,v].
  deleteTree(ihEmpty,_,_,_) => ihEmpty.
  deleteTree(ihLeaf(Hash,Els),_,Hash,K) => reformTree(ihLeaf(Hash,removeMember(K,Els))).
  deleteTree(ihNode(Sub),Dpth,Hash,K) where Ix.=subKey(Hash,Dpth) =>
    reformTree(ihNode(patchVec(Sub,Ix,(Sb)=>deleteTree(Sb,Dpth+2,Hash,K)))).

  removeMember:all k,v ~~ equality[k] |: (k,cons[(k,v)])=>cons[(k,v)].
  removeMember(K,cons((Ky,_),T)) where K==Ky => T.
  removeMember(K,cons(Ky,L)) => cons(Ky,removeMember(K,L)).
  removeMember(_,nil) => nil.

  reformTree:all k,v ~~ (ideal[k,v]) => ideal[k,v].
  reformTree(ihEmpty) => ihEmpty.
  reformTree(ihLeaf(_,nil)) => ihEmpty.
  reformTree(ihNode((Tr,ihEmpty,ihEmpty,ihEmpty))) where \+ ihNode(_).=Tr => Tr.
  reformTree(ihNode((ihEmpty,Tr,ihEmpty,ihEmpty))) where \+ ihNode(_).=Tr => Tr.
  reformTree(ihNode((ihEmpty,ihEmpty,Tr,ihEmpty))) where \+ ihNode(_).=Tr => Tr.
  reformTree(ihNode((ihEmpty,ihEmpty,ihEmpty,Tr))) where \+ ihNode(_).=Tr => Tr.
  reformTree(Tr) default => Tr.

  pickSub:all k,v ~~ (subVect[k,v],integer)=>ideal[k,v].
  pickSub((El,_,_,_),0) => El.
  pickSub((_,El,_,_),1) => El.
  pickSub((_,_,El,_),2) => El.
  pickSub((_,_,_,El),3) => El.

  patchVec:all k,v ~~ (subVect[k,v],integer,(ideal[k,v])=>ideal[k,v])=>subVect[k,v].
  patchVec((E0,E1,E2,E3),0,F) => (F(E0),E1,E2,E3).
  patchVec((E0,E1,E2,E3),1,F) => (E0,F(E1),E2,E3).
  patchVec((E0,E1,E2,E3),2,F) => (E0,E1,F(E2),E3).
  patchVec((E0,E1,E2,E3),3,F) => (E0,E1,E2,F(E3)).

  singleVec:all k,v ~~ (integer,ideal[k,v]) => subVect[k,v].
  singleVec(Ix,Tr) => patchVec(emptyVec,Ix,(_)=>Tr).

  emptyVec:all k,v ~~ subVect[k,v].
  emptyVec = (ihEmpty,ihEmpty,ihEmpty,ihEmpty).

  subKey:(integer,integer) => integer.
  subKey(Hash,Depth) => _band(_blsr(Hash,Depth),0x3).

  public implementation all k,v ~~ display[k],display[v] |: display[ideal[k,v]] => let{
    dispTree:all k,v ~~ display[k],display[v] |: (ideal[k,v],integer) => ss.
    dispTree(ihEmpty,Dp) => ssSeq([spaces(Dp)..,ss("ε"),ss("\n")]).
    dispTree(ihLeaf(H,Els),Dp) => ssSeq([spaces(Dp)..,disp(H),ss(":"),dispEls(Els),ss("\n")]).
    dispTree(ihNode(V),Dp) => ssSeq([spaces(Dp)..,ss("@"),disp(Dp),ss("\n"),dispVec(V,Dp+2)]).

    dispVec:all k,v ~~ display[k],display[v] |: (subVect[k,v],integer) => ss.
    dispVec((A1,A2,A3,A4),Dp) => ssSeq([dispTree(A1,Dp),dispTree(A2,Dp),dispTree(A3,Dp),dispTree(A4,Dp)]).

    dispEls:all k,v ~~ display[k],display[v] |: (cons[(k,v)]) => ss.
    dispEls(nil) => ss(".").
    dispEls(cons((K,V),T)) => ssSeq([disp(K),ss("->"),disp(V),dispMoreEls(T)]).

    dispMoreEls:all k,v ~~ display[k],display[v] |: (cons[(k,v)]) => ss.
    dispMoreEls(nil) => ss(".").
    dispMoreEls(cons((K,V),T)) => ssSeq([ss(","),disp(K),ss("->"),disp(V),dispMoreEls(T)]).

    spaces:(integer) => list[ss].
    spaces(0) => [].
    spaces(X) => [ss(" "),..spaces(X-1)].
  } in {.
    disp(T) => dispTree(T,0).
  .}

}
