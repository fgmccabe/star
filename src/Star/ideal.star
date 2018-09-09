star.ideal{
  -- Ideal hash trees
  import star.core.
  import star.arith.
  import star.cons.
  import star.lists.
  import star.collection.
  import star.tuples.

  all k,v ~~ subVect[k,v] ~> (ideal[k,v],ideal[k,v],ideal[k,v],ideal[k,v]).

  public
  all k,v ~~ /* equality[k] |: */ideal[k,v] ::=
      ihEmpty                               -- empty dictionary
    | ihLeaf(integer,cons[(k,v)])           -- leaf entries
    | ihNode(integer,integer,subVect[k,v]). -- non-leaf case

  public treeFind: all k,v ~~ equality[k],hash[k] |: (ideal[k,v],k) => option[v].
  treeFind(Tr,Ky) => findInTree(hash(Ky),Ky,Tr).

  findInTree:all k,v ~~ equality[k] |: (integer,k,ideal[k,v]) => option[v].
  findInTree(Hash,Ky,ihLeaf(Hash,Els)) => findMember(Ky,Els).
  findInTree(Hash,Ky,ihNode(_,Depth,Sub)) =>
    findInTree(Hash,Ky,pickSub(Sub,subKey(Hash,Depth))).
  findInTree(_,_,_) default => none.

  pickSub:all k,v ~~ (subVect[k,v],integer)=>ideal[k,v].
  pickSub((El,_,_,_),0) => El.
  pickSub((_,El,_,_),1) => El.
  pickSub((_,_,El,_),2) => El.
  pickSub((_,_,_,El),3) => El.

  patchVec:all k,v ~~ (subVect[k,v],integer,ideal[k,v])=>subVect[k,v].
  patchVec((_,E1,E2,E3),0,E) => (E,E1,E2,E3).
  patchVec((E0,_,E2,E3),1,E) => (E0,E,E2,E3).
  patchVec((E0,E1,_,E3),2,E) => (E0,E1,E,E3).
  patchVec((E0,E1,E2,_),3,E) => (E0,E1,E2,E).

  singleVec:all k,v ~~ (integer,ideal[k,v]) => subVect[k,v].
  singleVec(Ix,Tr) => patchVec(emptyVec,Ix,Tr).

  emptyVec:all k,v ~~ subVect[k,v].
  emptyVec = (ihEmpty,ihEmpty,ihEmpty,ihEmpty).

  findMember:all k,v ~~ equality[k] |: (k,cons[(k,v)])=>option[v].
  findMember(K,cons((Ky,V),_)) where K==Ky => some(V).
  findMember(K,cons(_,L)) => findMember(K,L).
  findMember(_,nil) => none.

  public insertIdeal:all k,v ~~ equality[k],hash[k] |: (ideal[k,v],k,v)=>ideal[k,v].
  insertIdeal(I,Ky,Vl) => mergeTree(I,ihLeaf(hash(Ky),cons((Ky,Vl),nil))).

  public mergeTree:all k,v ~~ equality[k] |: (ideal[k,v],ideal[k,v]) => ideal[k,v].
  mergeTree(ihEmpty,T) => T.
  mergeTree(T,ihEmpty) => T.
  mergeTree(ihLeaf(H1,El1),ihLeaf(H2,El2)) =>
    mergeLeafs(H1,El1,H2,El2).
  mergeTree(ihNode(M1,D1,Sub1),ihLeaf(H2,El2)) where
      Sx.=suffix(H2,D1) =>
    (Sx==M1 && Ix.=subKey(H2,D1) ?
      ihNode(M1,D1,patchVec(Sub1,Ix,mergeTree(pickSub(Sub1,Ix),ihLeaf(H2,El2)))) |
      mergeTree(pseudoParent(M1,D1,ihNode(M1,D1,Sub1)),ihLeaf(H2,El2))).
  mergeTree(ihLeaf(H1,El1),ihNode(M2,D2,Sub2)) where
      Sx.=suffix(H1,D2) =>
    (Sx==M2 && Ix.=subKey(H1,D2) ?
      ihNode(M2,D2,patchVec(Sub2,Ix,mergeTree(ihLeaf(H1,El1),pickSub(Sub2,Ix)))) |
      mergeTree(ihLeaf(H1,El1),pseudoParent(M2,D2,ihNode(M2,D2,Sub2)))).
  mergeTree(ihNode(M1,D1,Sub1),ihNode(M2,D2,Sub2)) =>
    (D1==D2 ?
      (M1==M2 ?
        ihNode(M1,D1,mergeVects(Sub1,Sub2)) |
        mergeTree(pseudoParent(M1,D1,ihNode(M1,D1,Sub1)),
                  pseudoParent(M2,D2,ihNode(M2,D2,Sub2)))
      ) |
    D1<D2 ?
        mergeTree(ihNode(M1,D1,Sub1),
                  pseudoParent(M2,D2,ihNode(M2,D2,Sub2))) |
        mergeTree(pseudoParent(M1,D1,ihNode(M1,D1,Sub1)),
                  ihNode(M2,D2,Sub2))).

  pseudoParent:all k,v ~~ (integer,integer,ideal[k,v]) => ideal[k,v].
  pseudoParent(Msk,Dpth,Tr) where D2.=Dpth-2 => ihNode(suffix(Msk,D2),D2,singleVec(subKey(Msk,D2),Tr)).

  mergeVects:all k,v ~~ equality[k] |: (subVect[k,v],subVect[k,v])=>subVect[k,v].
  mergeVects((A1,A2,A3,A4),(B1,B2,B3,B4)) =>
      (mergeTree(A1,B1),mergeTree(A2,B2),mergeTree(A3,B3),mergeTree(A4,B4)).

  mergeLeafs:all k,v ~~ equality[k] |:
      (integer,cons[(k,v)],integer,cons[(k,v)])=>ideal[k,v].
  mergeLeafs(H,El1,H,El2) => ihLeaf(H,merge(El1,El2)).
  mergeLeafs(H1,El1,H2,El2) where D.=commonDepth(H1,H2) =>
    ihNode(suffix(H1,D),D,
      patchVec(patchVec(emptyVec,subKey(H1,D),ihLeaf(H1,El1)),
        subKey(H2,D),ihLeaf(H2,El2))).

  merge:all k,v ~~ equality[k] |: (cons[(k,v)],cons[(k,v)])=>cons[(k,v)].
  merge(nil,L) => L.
  merge(cons((K,V),T),L) where keyPresent(K,L) => merge(T,L).
  merge(cons((K,V),T),L) => cons((K,V),merge(T,L)).

  keyPresent:all k,v ~~ equality[k] |: (k,cons[(k,v)])=>boolean.
  keyPresent(_,nil) default => false.
  keyPresent(K,cons((K,_),_)) => true.
  keyPresent(K,cons(_,T)) => keyPresent(K,T).

  -- how common are two hashes (counting from the least significant bit)
  commonDepth:(integer,integer)=>integer.
  commonDepth(H,H) => 64.
  commonDepth(H1,H2) default => let{
    cmDpth(D) where D2.=D+2 && suffix(H1,D2) == suffix(H2,D2) => cmDpth(D2).
    cmDpth(D) default => D
  } in cmDpth(0).


  suffix(H,D) => _band(H,_blsl(1,D)-1).

  subKey:(integer,integer) => integer.
  subKey(Hash,Depth) => _band(_blsr(Hash,Depth),0x3).

  public implementation all k,v ~~ display[k],display[v] |: display[ideal[k,v]] => {.
    disp(T) => dispTree(T,0).
  .}

  dispTree:all k,v ~~ display[k],display[v] |: (ideal[k,v],integer) => ss.
  dispTree(ihEmpty,Dp) => ssSeq([spaces(Dp)..,ss("ε"),ss("\n")]).
  dispTree(ihLeaf(H,Els),Dp) => ssSeq([spaces(Dp)..,disp(H),ss(":"),dispEls(Els),ss("\n")]).
  dispTree(ihNode(H,D,V),Dp) => ssSeq([spaces(Dp)..,disp(H),ss("@"),disp(D),ss("\n"),dispVec(V,Dp+2)]).

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
}
