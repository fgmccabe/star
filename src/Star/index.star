star.index{
  -- Implement an index tree structure

  import star.core.
  import star.arith.
  import star.lists.
  import star.coerce.
  import star.collection.
  import star.tuples.

  /**
   * The map type describes the core dictionary mapping.
   * Implemented as an index hash tree.
   */

  map@has_param(k, "The type of a key. map is generic in both key type and value type").
  map@has_param(v, "The type of a value.").

  public
  all k,v ~~ /* equality[k] |: */map[k,v] ::=
      trEmpty                                       -- @enum empty dictionary
    | trLeaf(integer,list[(k,v)])                   -- @con leaf entries
    | trNode(integer,integer,map[k,v],map[k,v]).    -- @con non-leaf dictionary

  public implementation all k,v ~~ equality[k], hash[k] |: indexed[map[k,v] ->> k,v] => {
    present(M,K) => lookIn(hash(K),M,K).
    _remove(M,K) => rmve(hash(K),K,M).
    _put(M,K,V) => insrt(K,V,M).
    keys(M) => keyMap(M,[]).
    pairs(M) => mapPairs(M,[]).
    values(M) => mapValues(M,[]).
    _empty = trEmpty.
  }

  public implementation all k,v ~~ equality[k], equality[v] |: equality[map[k,v]] => {.
    M1 == M2 => (mapPairs(M1,[]):list[(k,v)])==mapPairs(M2,[]).
  .}

  public implementation all k,v ~~ sizeable[map[k,v]] => {
    size(M) => countEls(M,0).
    isEmpty(trEmpty) => true.
    isEmpty(M) => countEls(M,0)==0.
  }

  countEls:all k,v ~~ (map[k,v],integer) => integer.
  countEls(trEmpty,C) => C.
  countEls(trLeaf(_,L),C) => C+size(L).
  countEls(trNode(_,_,L,R),C) => countEls(R,countEls(L,C)).

  -- public
  -- implementation all k,v ~~ equality[k] |: setops[map[k,v]] => {
  --   _union(M1,M2) => addTree(M1,M2).
  --   _intersect(M1,M2) => intersectTree(M1,M2).
  --   _difference(M1,M2) => subtractTree(M1,M2).
  -- }

  public find:all m,k,v ~~ equality[k], hash[k], indexed[m->>k,v] |: (m,k) => v.
  find(M,K) where some(V).= present(M,K) => V.

  public foldMap:all k,v,u ~~ ((k,v,u)=>u,u,map[k,v]) => u.
  foldMap(_,u,trEmpty) => u.
  foldMap(f,u,trLeaf(_,Els)) => foldLeafs(Els,f,u).
  foldMap(f,u,trNode(_,_,Left,Right)) => foldMap(f,foldMap(f,u,Left),Right).

  foldLeafs:all m,k,v,f,u ~~ (list[(k,v)],(k,v,u)=>u,u) => u.
  foldLeafs([],_,u)=>u.
  foldLeafs([(k,v),..l],f,u) => foldLeafs(l,f,f(k,v,u)).

  public implementation all k,v ~~ folding[map[k,v]->>v] => {
    foldRight(F,U,M) => fldRight(M,F,U).
    foldLeft(F,U,M) => fldLeft(M,F,U).
  }

  private fldRight:all k,v,u ~~ (map[k,v],(v,u)=>u,u) => u.
  fldRight(trLeaf(_,Els),f,u) => rightLeafs(Els,f,u).
  fldRight(trNode(_,_,Left,Right),f,u) => fldRight(Right,f,fldRight(Left,f,u)).

  rightLeafs:all k,v,u ~~ (list[(k,v)],(v,u)=>u,u) => u.
  rightLeafs([],_,u)=>u.
  rightLeafs([(_,v),..l],f,u) => rightLeafs(l,f,f(v,u)).

  private fldLeft:all k,v,u ~~ (map[k,v],(u,v)=>u,u) => u.
  fldLeft(trLeaf(_,Els),f,u) => leftLeafs(Els,f,u).
  fldLeft(trNode(_,_,Left,Right),f,u) => fldLeft(Left,f,fldLeft(Right,f,u)).

  leftLeafs:all k,v,u ~~ (list[(k,v)],(u,v)=>u,u) => u.
  leftLeafs([],_,u)=>u.
  leftLeafs([(_,v),..l],f,u) => f(leftLeafs(l,f,u),v).

  public implementation ixmap[map] => {
    M///f => ixMap(M,f).
  }

  ixMap:all k,v,w ~~ (map[k,v],(k,v)=>w) => map[k,w].
  ixMap(trEmpty,_) => trEmpty.
  ixMap(trNode(Hsh,Len,L,R),f) => trNode(Hsh,Len,ixMap(L,f),ixMap(R,f)).
  ixMap(trLeaf(Hash,Els),f) => trLeaf(Hash,applyF(Els,f)).

  private applyF:all k,v,w ~~ (list[(k,v)],(k,v)=>w)=>list[(k,w)].
  applyF([],_) => [].
  applyF([(K,V),..L],f) => [(K,f(K,V)),..applyF(L,f)].

  public implementation all k,v ~~ equality[k],hash[k] |: ixfilter[map->>k,v] => {
    M^//p => ixFilter(M,p).
  }

  ixFilter:all k,v ~~ equality[k],hash[k] |: (map[k,v],(k,v)=>boolean) => map[k,v].
  ixFilter(M,P) => foldMap((k,v,N)=>checkEntry(k,v,N,P),trEmpty,M).

  checkEntry:all k,v ~~ equality[k],hash[k] |: (k,v,map[k,v],(k,v)=>boolean) => map[k,v].
  checkEntry(K,V,So,P) where P(K,V) => insrt(K,V,So).
  checkEntry(_,_,So,_) => So.

  lookIn: all k,v ~~ equality[k] |: (integer,map[k,v],k)=>option[v].
  lookIn(H,trLeaf(H,Els),K) => findMember(K,Els).
  lookIn(H,trNode(Msk,Ln,Left,Right),K) where commonMask(H,Ln)==Msk =>
    (nthBit(H,Ln) ? lookIn(H,Right,K) | lookIn(H,Left,K)).
  lookIn(_,_,_) => none.

  findMember:all k,v ~~ equality[k] |: (k,list[(k,v)])=>option[v].
  findMember(K,[(Ky,V),.._]) where K==Ky => some(V).
  findMember(K,[_,..L]) => findMember(K,L).
  findMember(_,[]) => none.

  insrt:all k,v ~~ equality[k],hash[k] |: (k,v,map[k,v])=>map[k,v].
  insrt(K,V,T) => mergeTree(T,trLeaf(hash(K),[(K,V)])).

  mergeTree:all k,v ~~ equality[k] |: (map[k,v],map[k,v])=>map[k,v].
  mergeTree(trEmpty,T) => T.
  mergeTree(T,trEmpty) => T.
  mergeTree(T1,T2) => mergeNodes(T1,T2).

  mergeLeafs:all k,v ~~ equality[k] |: (integer,list[(k,v)],integer,list[(k,v)])=>map[k,v].
  mergeLeafs(H,L1,H,L2) => trLeaf(H,mergePairs(L1,L2)).
  mergeLeafs(H1,L1,H2,L2) where
        CML .= commonMaskLen(H1,H2,HashLen) &&
        CM .= commonMask(H1,CML) =>
      (nthBit(H1,CML) ?
        trNode(CM,CML,trLeaf(H2,L2),trLeaf(H1,L1)) |
        trNode(CM,CML,trLeaf(H1,L1),trLeaf(H2,L2))).

  private mergePairs: all k,v ~~ equality[k] |: (list[(k,v)],list[(k,v)])=>list[(k,v)].
  mergePairs([],L) => L.
  mergePairs([(K,V),..L1],L) where keyPresent(L,K) => mergePairs(L1,L).
  mergePairs([E,..L],L1) => [E,..mergePairs(L,L1)].

  private keyPresent:all k,v ~~ equality[k] |: (list[(k,v)],k)=>boolean.
  keyPresent(L,K) => let{
    kyPrsnt:(integer,integer)=>boolean.
    kyPrsnt(Ix,Mx) where Ix>=Mx => false.
    kyPrsnt(Ix,Mx) where (K,_) .= _list_nth(L,Ix) => true.
    kyPrsnt(Ix,Mx) => kyPrsnt(Ix+1,Mx).
  } in kyPrsnt(0,size(L)).

  mergeNodes:all k,v ~~ equality[k] |: (map[k,v],map[k,v])=>map[k,v].
  mergeNodes(trLeaf(H1,L1),trLeaf(H2,L2)) => mergeLeafs(H1,L1,H2,L2).
  mergeNodes(T1,T2) where
      trNode(Msk1,Ln1,L1,R1).=T1 &&
      trLeaf(Msk2,_) .= T2 &&
      CML .= min(commonMaskLen(Msk1,Msk2,HashLen),Ln1) &&
      CM .= commonMask(Msk1,CML) =>
      (CML<Ln1 ?
        ( nthBit(Msk2,CML) ?
          trNode(CM,CML,T1,T2) |
          trNode(CM,CML,T2,T1))  |
        ( nthBit(Msk2,CML) ?
          trNode(CM,CML,L1,mergeNodes(R1,T2)) |
          trNode(CM,CML,mergeNodes(L1,T2),R1))).
  mergeNodes(T1, T2) where
      T1 =. trLeaf(Msk1,_) &&
      T2 =. trNode(Msk2,Ln2,L2,R2) &&
      CML .= min(commonMaskLen(Msk1,Msk2,HashLen),Ln2) &&
      CM .= commonMask(Msk2,CML) =>
      (CML < Ln2 ?
        ( nthBit(Msk1,CML) ?
          trNode(CM,CML,T1,T2) |
          trNode(CM,CML,T2,T1))  |
        ( nthBit(Msk1,CML) ?
          trNode(CM,CML,L2,mergeNodes(R2,T1)) |
          trNode(CM,CML,mergeNodes(L2,T1),R2)
          )).
  mergeNodes(T1,T2) where
      T1 =. trNode(Msk1,Ln1,L1,R1) &&
      T2 =. trNode(Msk2,Ln2,L2,R2) &&
      CML .= min(min(commonMaskLen(Msk1,Msk2,HashLen),Ln1),Ln2) &&
      CM .= commonMask(Msk1,CML) =>
      (CML < Ln1 ?
        (nthBit(Msk2,CML) ?
          trNode(CM,CML,L1,mergeNodes(R1,T2)) |
          trNode(CM,CML,mergeNodes(L1,T2),R1))  |
      CML < Ln2 ?
        (nthBit(Msk1,CML) ?
          trNode(CM,CML,L2,mergeNodes(T1,R2)) |
          trNode(CM,CML,mergeNodes(L2,T1),R2))  |
        trNode(CM,CML,mergeNodes(L1,L2),mergeNodes(R1,R2))
      ).

  rmve:all k,v ~~ equality[k] |: (integer,k,map[k,v]) => map[k,v].
  rmve(_,_,trEmpty) => trEmpty.
  rmve(H,K,trLeaf(H1,L)) => (H==H1 ? reformLeaf(H,dropEntry(L,K)) | trLeaf(H1,L)).
  rmve(H,K,T) where trNode(M,Ln,L,R) .= T && CM.= commonMask(H,Ln) =>
    ( CM == M ?
      ( nthBit(H,Ln) ?
        reformNode(trNode(M,Ln,L,rmve(H,K,R))) |
        reformNode(trNode(M,Ln,rmve(H,K,L),R))
      ) |  -- not present
      T).

  private dropEntry:all k,v~~ equality[k] |: (list[(k,v)],k)=>list[(k,v)].
  dropEntry([(k,_),..l],k) => l.
  dropEntry([f,..l],k) =>[f,..dropEntry(l,k)].
  dropEntry([],_) => [].

  reformLeaf:all  k,v ~~ equality[k] |: (integer,list[(k,v)]) => map[k,v].
  reformLeaf(H,[]) => trEmpty.
  reformLeaf(H,L) => trLeaf(H,L).

  reformNode:all k,v ~~ equality[k] |: (map[k,v]) => map[k,v].
  reformNode(trNode(_,_,trEmpty,R)) => R.
  reformNode(trNode(_,_,L,trEmpty)) => L.
  reformNode(N) => N.

  addTree:all k,v ~~ equality[k],hash[k] |: (map[k,v],map[k,v])=>map[k,v].
  addTree(trEmpty,T) => T.
  addTree(T,trEmpty) => T.
  addTree(T1,T2) => addNodes(T1,T2).

  addNodes:all k,v ~~ equality[k],hash[k] |: (map[k,v],map[k,v])=>map[k,v].
  addNodes(T1,trLeaf(H,Leaves)) => addLeafs(T1,Leaves).
  addNodes(T1,trNode(Msk2,Ln2,L2,R2)) =>
    addNodes(addNodes(T1,L2),R2).

  addLeafs:all k,v ~~ equality[k],hash[k] |: (map[k,v],list[(k,v)])=>map[k,v].
  addLeafs(T,[]) => T.
  addLeafs(T,[(K,V),..Lvs]) =>
    addLeafs(insrt(K,V,T),Lvs).

  subtractTree:all k,v ~~ equality[k],hash[k] |: (map[k,v],map[k,v])=>map[k,v].
  subtractTree(trEmpty,T) => T.
  subtractTree(T,trEmpty) => T.
  subtractTree(T1,T2) => subtractNodes(T1,T2).

  subtractNodes:all k,v ~~ equality[k],hash[k] |: (map[k,v],map[k,v])=>map[k,v].
  subtractNodes(T1,trLeaf(_,Leaves)) => subtractLeafs(T1,Leaves).
  subtractNodes(T1,trNode(Msk2,Ln2,L2,R2)) =>
    subtractNodes(subtractNodes(T1,L2),R2).

  subtractLeafs:all k,v ~~ equality[k],hash[k] |: (map[k,v],list[(k,v)])=>map[k,v].
  subtractLeafs(T,[]) => T.
  subtractLeafs(T,[(K,_),..Lvs]) =>
    subtractLeafs(rmve(hash(K),K,T),Lvs).

  private keyMap:all k,v ~~ (map[k,v],list[k]) => list[k].
  keyMap(trEmpty,L) => L.
  keyMap(trLeaf(_,Leaves),L) => leafKeys(Leaves,L).
  keyMap(trNode(_,_,Lf,Rg),L) => keyMap(Rg,keyMap(Lf,L)).

  private leafKeys:all k,v ~~ (list[(k,v)],list[k]) => list[k].
  leafKeys([],L) => L.
  leafKeys([(K1,V1),..M],L) => leafKeys(M,[K1,..L]).

  private mapPairs:all k,v ~~ (map[k,v],list[(k,v)]) => list[(k,v)].
  mapPairs(trEmpty,L) => L.
  mapPairs(trLeaf(_,Lf),L) => Lf++L.
  mapPairs(trNode(_,_,Lf,Rg),L) => mapPairs(Rg,mapPairs(Lf,L)).

  public mapMap:all k,v,w ~~ (map[k,v],(v)=>w) => map[k,w].
  mapMap(trEmpty,_) => trEmpty.
  mapMap(trLeaf(Hsh,L),F) => trLeaf(Hsh,mapLeaves(L,F)).
  mapMap(trNode(Hsh,Ln,L,R),F) => trNode(Hsh,Ln,mapMap(L,F),mapMap(R,F)).

  private mapLeaves:all k,v,w ~~ (list[(k,v)],(v)=>w) => list[(k,w)].
  mapLeaves([],_) => [].
  mapLeaves([(k,v),..l],f) => [(k,f(v)),..mapLeaves(l,f)].

  private mapValues:all k,v ~~ (map[k,v],list[v]) => list[v].
  mapValues(trEmpty,L) => L.
  mapValues(trLeaf(_,Lf),L) => projectValues(Lf,L).
  mapValues(trNode(_,_,Lf,Rg),L) => mapValues(Rg,mapValues(Lf,L)).

  private projectValues:all k,v ~~ (list[(k,v)],list[v]) => list[v].
  projectValues([],So) => So.
  projectValues([(_,V),..L],So) => projectValues(L,[V,..So]).

  public
  implementation all k,v ~~ display[k], display[v] |: display[map[k,v]] => {
    disp(Tree) => ssSeq([ss("["),ssSeq(dispEls(Tree)),ss("]")]).
  }

  dispEls:all k,v ~~ display[k], display[v] |: (map[k,v])=>list[ss].
  dispEls(Tree) => let{
    dspPair(K,V,(0,L)) => (1,[L..,disp(K),ss("->"),disp(V)]).
    dspPair(K,V,(_,L)) => (1,[L..,ss(", "),disp(K),ss("->"),disp(V)]).
  } in snd(foldMap(dspPair,(0,[]),Tree)).

  HashLen:integer.
  HashLen = 64.

  commonMaskLen:(integer,integer,integer) => integer.
  commonMaskLen(H1,H2,C) where C>0 && H1=!=H2 => commonMaskLen(_blsr(H1,1),_blsr(H2,1),C-1).
  commonMaskLen(_,_,C) => C.

  commonMask:(integer,integer)=>integer.
  commonMask(_,0) => 0.
  commonMask(M1,ML) where CML.=HashLen-ML=> _band(_blsl(_blsr(-1,CML),CML),M1).

  nthBit:(integer,integer)=>boolean.
  nthBit(X,N) => _nthb(X,63-N).

  public implementation all k,v ~~ equality[k],hash[k] |: stream[map[k,v] ->> (k,v)] => {
    _eof(trEmpty) => true.
    _eof(_) => false.

    _hdtl(M) => fstEl(M).
    _back(M) => lstEl(M).

    _cons((K,V),M) => insrt(K,V,M).
    _apnd(M,(K,V)) => insrt(K,V,M).

    _nil = trEmpty.
  }

  fstEl:all k,v ~~ equality[k] |: (map[k,v]) => option[((k,v),map[k,v])].
  fstEl(trLeaf(_,[(k,v)])) => some(((k,v),trEmpty)).
  fstEl(trLeaf(Msk,[(k,v),..Lvs])) => some(((k,v),trLeaf(Msk,Lvs))).
  fstEl(trNode(Msk,Len,trEmpty,R)) => fstEl(R).
  fstEl(trNode(Msk,Len,L,R)) where some((P,LL)).=fstEl(L) =>
    some((P,(trEmpty.=LL ? R | trNode(Msk,Len,LL,R)))).

  lstEl:all k,v ~~ equality[k] |: (map[k,v]) => option[(map[k,v],(k,v))].
  lstEl(trLeaf(_,[(k,v)])) => some((trEmpty,(k,v))).
  lstEl(trLeaf(Msk,[(k,v),..Lvs])) => some((trLeaf(Msk,Lvs),(k,v))).
  lstEl(trNode(Msk,Len,L,trEmpty)) => lstEl(L).
  lstEl(trNode(Msk,Len,L,R)) where some((RR,P)).=lstEl(R) =>
    some(((trEmpty.=RR ? L | trNode(Msk,Len,L,RR)),P)).

  public dumpTree:all k,v ~~ display[k] |: (map[k,v],integer) => ss.
  dumpTree(trLeaf(Msk,Entries),Lvl) => ssSeq([ssSeq(spaces(Lvl)),ss(Msk::string),ss(":"),ssSeq(showKeys(Entries)),ss("\n")]).
  dumpTree(trNode(Msk,Len,L,R),Lvl) =>
    ssSeq([ssSeq(spaces(Lvl)),ss(Msk::string),ss(":"),ss(Len::string),ss("\n"),
    dumpTree(L,Lvl+1),
    dumpTree(R,Lvl+1)]).
  dumpTree(trEmpty,Lvl) => ssSeq([ssSeq(spaces(Lvl)),ss("*\n")]).

  showKeys:all k,v ~~ display[k] |: (list[(k,v)]) => list[ss].
  showKeys([]) => [].
  showKeys([(K,_),..L]) => [disp(K),ss(", "),..showKeys(L)].

  spaces:(integer) => list[ss].
  spaces(0) => [].
  spaces(X) => [ss(" "),..spaces(X-1)].
}
