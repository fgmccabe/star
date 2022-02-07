star.redblack{
  import star.core.
  import star.coerce.
  import star.arith.
  import star.collection.
  import star.cons.
  import star.ideal.
  import star.index.
  import star.iterable.
  import star.monad.
  import star.option.
  import star.tuples.

  -- red/black trees

  -- Based on Okasaki's book, with deletions by Matt Might

  color ::= .Red | .Black | .BBlack | .NBlack .

  implementation display[color]=>{
    disp(.Red) => "red".
    disp(.Black) => "blk".
    disp(.BBlack) => "bblk".
    disp(.NBlack) => "nblk"
  }

  -- Only the type itself is exposed
  public rbtree[k,v] ::=
    private .leaf |
      private .bleaf |
      private bbkNd(k,v,rbtree[k,v],rbtree[k,v]) |
      private nbkNd(k,v,rbtree[k,v],rbtree[k,v]) |
      private bkNd(k,v,rbtree[k,v],rbtree[k,v]) |
      private rdNd(k,v,rbtree[k,v],rbtree[k,v]).

  find:all k,v ~~ comp[k], equality[k] |:(rbtree[k,v],k)=> option[v].
  find(.leaf,_) => .none.
  find(nbkNd(K,V,_,_),K) => some(V).
  find(bbkNd(K,V,_,_),K) => some(V).
  find(bkNd(K1,_,L,_),K) where K<K1 => find(L,K).
  find(rdNd(K1,_,L,_),K) where K<K1 => find(L,K).

  find(nbkNd(_,_,_,R),K) => find(R,K).
  find(bbkNd(_,_,_,R),K) => find(R,K).
  find(bkNd(_,_,_,R),K) => find(R,K).
  find(rdNd(_,_,_,R),K) => find(R,K).

  insert:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k,v) => rbtree[k,v].
  insert(T,K,V) => add(T,K,V).

  add:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k,v) => rbtree[k,v].
  add(.leaf,K,V) => rdNd(K,V,.leaf,.leaf).
  add(nbkNd(K,_,L,R),K,V) => nbkNd(K,V,L,R).
  add(bbkNd(K,_,L,R),K,V) => bbkNd(K,V,L,R).
  add(bkNd(K,_,L,R),K,V) => bkNd(K,V,L,R).
  add(rdNd(K,_,L,R),K,V) => rdNd(K,V,L,R).
  add(bkNd(K1,V1,L,R),K,V) where K<K1 =>
    balance(.Black,K1,V1,add(L,K,V),R).
  add(bkNd(K1,V1,L,R),K,V) =>
    balance(.Black,K1,V1,L,add(R,K,V)).
  add(rdNd(K1,V1,L,R),K,V) where K<K1 =>
    balance(.Red,K1,V1,add(L,K,V),R).
  add(rdNd(K1,V1,L,R),K,V) =>
    balance(.Red,K1,V1,L,add(R,K,V)).
  

  balance:all k,v ~~ comp[k],equality[k] |:
    (color,k,v,rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  balance(Cl,Kz,Vz,rdNd(Ky,Vy,rdNd(Kx,Vx,A,B),C),D) where isBlack(Cl) =>
    whiten(Cl,Ky,Vy,bkNd(Kx,Vx,A,B),bkNd(Kz,Vz,C,D)).
  balance(Cl where isBlack(Cl),Kz,Vz,rdNd(Kx,Vx,A,rdNd(Ky,Vy,B,C)),D) =>
    whiten(Cl,Ky,Vy,bkNd(Kx,Vx,A,B),bkNd(Kz,Vz,C,D)).
  balance(Cl where isBlack(Cl),Kx,Vx,A,rdNd(Kz,Vz,rdNd(Ky,Vy,B,C),D)) =>
    whiten(Cl,Ky,Vy,bkNd(Kx,Vx,A,B),bkNd(Kz,Vz,C,D)).
  balance(Cl where isBlack(Cl),Kx,Vx,A,rdNd(Ky,Vy,B,rdNd(Kz,Vz,C,D))) =>
    whiten(Cl,Ky,Vy,bkNd(Kx,Vx,A,B),bkNd(Kz,Vz,C,D)).
  balance(.BBlack,Kz,Vz,nbkNd(Kx,Vx,bkNd(Kw,Vw,A,B),bkNd(Ky,Vy,C,D)),E) =>
    bkNd(Ky,Vy,balance(.Black,Ky,Vy,redden(bkNd(Kw,Vw,A,B)),C),bkNd(Kz,Vz,D,E)).
  balance(.BBlack,Kx,Vx,A,nbkNd(Kz,Vz,bkNd(Ky,Vy,B,C),bkNd(Kw,Vw,D,E)))=>
    bkNd(Ky,Vy,bkNd(Kx,Vx,A,B),balance(.Black,Kz,Vz,C,redden(bkNd(Kw,Vw,D,E)))).

  balance(.NBlack,K,V,L,R) => nbkNd(K,V,L,R).
  balance(.BBlack,K,V,L,R) => bbkNd(K,V,L,R).
  balance(.Black,K,V,L,R) => bkNd(K,V,L,R).
  balance(.Red,K,V,L,R) => rdNd(K,V,L,R).

  delete:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k)=>rbtree[k,v].
  delete(T,K) => blacken(del(T,K)).
  
  del:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k)=>rbtree[k,v].
  del(.leaf,_) => .leaf.
  del(nbkNd(K,_,L,R),K) => remove(.NBlack,L,R).
  del(nbkNd(K,_,L,R),K) => remove(.BBlack,L,R).
  del(bkNd(K,_,L,R),K) => remove(.Black,L,R).
  del(rdNd(K,_,L,R),K) => remove(.Red,L,R).
  del(nbkNd(K1,V1,A,B),K) where K<K1 => bubble(.NBlack,(K1,V1),del(A,K),B).
  del(bbkNd(K1,V1,A,B),K) where K<K1 => bubble(.BBlack,(K1,V1),del(A,K),B).
  del(bkNd(K1,V1,A,B),K) where K<K1 => bubble(.Black,(K1,V1),del(A,K),B).
  del(rdNd(K1,V1,A,B),K) where K<K1 => bubble(.Red,(K1,V1),del(A,K),B).
  del(nbkNd(K1,V1,A,B),K) where K>K1 => bubble(.NBlack,(K1,V1),A,del(B,K)).
  del(bbkNd(K1,V1,A,B),K) where K>K1 => bubble(.BBlack,(K1,V1),A,del(B,K)).
  del(bkNd(K1,V1,A,B),K) where K>K1 => bubble(.Black,(K1,V1),A,del(B,K)).
  del(rdNd(K1,V1,A,B),K) where K>K1 => bubble(.Red,(K1,V1),A,del(B,K)).

  remove:all k,v ~~ comp[k],equality[k] |: (color,rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  remove(.Red,.leaf,.leaf)=>.leaf.
  remove(.Black,.leaf,.leaf)=>.bleaf.
  remove(.Black,rdNd(K,V,L,R),.leaf) => bkNd(K,V,L,R).
  remove(.Black,.leaf,rdNd(K,V,L,R)) => bkNd(K,V,L,R).
  remove(C,A,B) => bubble(C,rightMost(A),removeMax(A),B).

  removeMax:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v])=>rbtree[k,v].
  removeMax(nbkNd(_,_,L,.leaf)) => remove(.NBlack,L,.leaf).
  removeMax(bbkNd(_,_,L,.leaf)) => remove(.BBlack,L,.leaf).
  removeMax(bkNd(_,_,L,.leaf)) => remove(.Black,L,.leaf).
  removeMax(rdNd(_,_,L,.leaf)) => remove(.Red,L,.leaf).
  removeMax(nbkNd(K,V,L,R)) => bubble(.NBlack,(K,V),L,removeMax(R)).
  removeMax(bbkNd(K,V,L,R)) => bubble(.BBlack,(K,V),L,removeMax(R)).
  removeMax(bkNd(K,V,L,R)) => bubble(.Black,(K,V),L,removeMax(R)).
  removeMax(rdNd(K,V,L,R)) => bubble(.Red,(K,V),L,removeMax(R)).

  bubble:all k,v ~~ comp[k],equality[k] |: (color,(k,v),rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  bubble(C,(K,V),L,R) where (isBB(L) || isBB(R)) =>
    balance(darken(C),K,V,redder(L),redder(R)).
  bubble(C,(K,V),L,R) => balance(C,K,V,L,R).

  rightMost(nbkNd(K,V,_,.leaf)) => (K,V).
  rightMost(bbkNd(K,V,_,.leaf)) => (K,V).
  rightMost(bkNd(K,V,_,.leaf)) => (K,V).
  rightMost(rdNd(K,V,_,.leaf)) => (K,V).
  rightMost(nbkNd(_,_,_,T)) => rightMost(T).
  rightMost(bbkNd(_,_,_,T)) => rightMost(T).
  rightMost(bkNd(_,_,_,T)) => rightMost(T).
  rightMost(rdNd(_,_,_,T)) => rightMost(T).

  isBB:all k,v ~~ (rbtree[k,v])=>boolean.
  isBB(.bleaf)=>.true.
  isBB(bbkNd(_,_,_,_))=>.true.
  isBB(_) default => .false.
  
  isBlack:(color)=>boolean.
  isBlack(.Black)=>.true.
  isBlack(.BBlack)=>.true.
  isBlack(_) default => .false.

  darken:(color)=>color.
  darken(.Red)=>.Black.
  darken(.Black)=>.BBlack.
  darken(.NBlack)=>.Red.

  whiten:all k,v ~~ (color,k,v,rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  whiten(.Red,K,V,L,R) => nbkNd(K,V,L,R).
  whiten(.Black,K,V,L,R) => rdNd(K,V,L,R).
  whiten(.Red,K,V,L,R) => nbkNd(K,V,L,R).
  whiten(.BBlack,K,V,L,R) => bkNd(K,V,L,R).

  redden:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  redden(nbkNd(K,V,L,R))=>rdNd(K,V,L,R).
  redden(bbkNd(K,V,L,R))=>rdNd(K,V,L,R).
  redden(bkNd(K,V,L,R))=>rdNd(K,V,L,R).
  redden(rdNd(K,V,L,R))=>rdNd(K,V,L,R).

  blacken:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  blacken(.leaf) => .leaf.
  blacken(.bleaf) => .leaf.
  blacken(nbkNd(K,V,L,R))=>bkNd(K,V,L,R).
  blacken(bbkNd(K,V,L,R))=>bkNd(K,V,L,R).
  blacken(bkNd(K,V,L,R))=>bkNd(K,V,L,R).
  blacken(rdNd(K,V,L,R))=>bkNd(K,V,L,R).

  redder:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  redder(.bleaf)=>.leaf.
  redder(nbkNd(K,V,L,R))=>whiten(.NBlack,K,V,L,R).
  redder(bbkNd(K,V,L,R))=>whiten(.BBlack,K,V,L,R).
  redder(bkNd(K,V,L,R))=>whiten(.Black,K,V,L,R).
  redder(rdNd(K,V,L,R))=>whiten(.Red,K,V,L,R).
  
  -- Implement some of the standard contracts
  pairs:all k,v ~~ (rbtree[k,v],cons[keyval[k,v]]) => cons[keyval[k,v]].
  pairs(.leaf,Ps) => Ps.
  pairs(.bleaf,Ps) => Ps.
  pairs(bbkNd(K,V,L,R),Ps) => pairs(L,[K->V,..pairs(R,Ps)]).
  pairs(nbkNd(K,V,L,R),Ps) => pairs(L,[K->V,..pairs(R,Ps)]).
  pairs(bkNd(K,V,L,R),Ps) => pairs(L,[K->V,..pairs(R,Ps)]).
  pairs(rdNd(K,V,L,R),Ps) => pairs(L,[K->V,..pairs(R,Ps)]).

  dispTree:all k,v ~~ display[k], display[v] |: (rbtree[k,v])=>string.
  dispTree(.leaf)=>"leaf".
  dispTree(.bleaf)=>"bleaf".
  dispTree(nbkNd(K,V,L,R)) => "(nblk #(dispTree(L))\:$(disp(K))->$(V)\:#(dispTree(R)))".
  dispTree(bbkNd(K,V,L,R)) => "(bblk #(dispTree(L))\:$(disp(K))->$(V)\:#(dispTree(R)))".
  dispTree(bkNd(K,V,L,R)) => "(blk #(dispTree(L))\:$(disp(K))->$(V)\:#(dispTree(R)))".
  dispTree(rdNd(K,V,L,R)) => "(red #(dispTree(L))\:$(disp(K))->$(V)\:#(dispTree(R)))".

  public validRb:all k,v ~~ (rbtree[k,v])=>boolean.
  validRb(T) => validReds(T) && allSame(blackHeights(T,0,[])).

  validReds:all k,v ~~ (rbtree[k,v])=>boolean.
  validReds(.leaf)=>.true.
  validReds(rdNd(_,_,L,R)) =>
    validReds(L) && validReds(R) && ~isRedNode(L) && ~isRedNode(R).
  validReds(bkNd(_,_,L,R)) => validReds(L) && validReds(R).
  validReds(_) default => .false.

  isRedNode:all k,v ~~ (rbtree[k,v])=>boolean.
  isRedNode(rdNd(_,_,_,_)) => .true.
  isRedNode(_) default => .false.
	
  blackHeights:all k,v ~~ (rbtree[k,v],integer,cons[integer])=>cons[integer].
  blackHeights(.leaf,H,Hs) => [H,..Hs].
  blackHeights(bkNd(_,_,L,R),H,Hs) =>
    blackHeights(L,H+1,blackHeights(R,H+1,Hs)).
  blackHeights(rdNd(_,_,L,R),H,Hs) =>
    blackHeights(L,H,blackHeights(R,H,Hs)).

  allSame:(cons[integer])=>boolean.
  allSame([])=>.true.
  allSame([Ix,..Is]) => let{.
    sme([])=>.true.
    sme([Ix,..II])=>sme(II).
    sme(_) default => .false
 .} in sme(Is).
  
  public implementation all k,v ~~ display[k],display[v] |:
    display[rbtree[k,v]] => {
      disp(T) => disp(pairs(T,[]))
    }.

  public implementation all k,v ~~ equality[k],equality[v] |:
    equality[rbtree[k,v]] => {
      T1==T2 => pairs(T1,[])==pairs(T2,[])
    }.

  public implementation all k,v ~~ equality[k],comp[k] |:
    indexed[rbtree[k,v]->>k,v] => {
      _index(Tr,Ky) => find(Tr,Ky).
      _put(Tr,Ky,Vl) => insert(Tr,Ky,Vl).
      _remove(Tr,Ky) => delete(Tr,Ky).
      _empty = .leaf.
    }.

  public implementation all k,v ~~ comp[k], equality[k] |:
    sequence[rbtree[k,v] ->> keyval[k,v]] => {
      _nil = .leaf.
      _cons(K->V,Tr) => add(Tr,K,V).
    }.

  public implementation all k,v ~~ equality[k],comp[k] |:
    stream[rbtree[k,v]->>keyval[k,v]] => let{.
      hdtl(T) where H^=hd(T) => some((H,drop(T,H))).
      hdtl(_) default => .none.

      private
      hd(.leaf)=>.none.
      hd(nbkNd(K,V,.leaf,_)) => some(K->V).
      hd(bbkNd(K,V,.leaf,_)) => some(K->V).
      hd(bkNd(K,V,.leaf,_)) => some(K->V).
      hd(rdNd(K,V,.leaf,_)) => some(K->V).
      hd(nbkNd(_,_,L,_)) => hd(L).
      hd(bbkNd(_,_,L,_)) => hd(L).
      hd(bkNd(_,_,L,_)) => hd(L).
      hd(rdNd(_,_,L,_)) => hd(L).

      drop(T,K->_) => delete(T,K)
    .} in {
      _eof(.leaf) => .true.
      _eof(_) default => .false.

      _hdtl(T) => hdtl(T)
    }.

  public implementation all k,v ~~ ixfold[rbtree[k,v]->>k,v] => let{.
    right(F,U,.leaf) => U.
    right(F,U,.bleaf) => U.
    right(F,U,nbkNd(K,V,L,R)) => right(F,F(K,V,right(F,U,R)),L).
    right(F,U,bbkNd(K,V,L,R)) => right(F,F(K,V,right(F,U,R)),L).
    right(F,U,bkNd(K,V,L,R)) => right(F,F(K,V,right(F,U,R)),L).
    right(F,U,rdNd(K,V,L,R)) => right(F,F(K,V,right(F,U,R)),L).


    left(F,U,.leaf) => U.
    left(F,U,nbkNd(K,V,L,R)) => left(F,F(K,V,left(F,U,L)),R).
    left(F,U,bbkNd(K,V,L,R)) => left(F,F(K,V,left(F,U,L)),R).
    left(F,U,bkNd(K,V,L,R)) => left(F,F(K,V,left(F,U,L)),R).
    left(F,U,rdNd(K,V,L,R)) => left(F,F(K,V,left(F,U,L)),R).
  .} in {
    ixRight = right.
    ixLeft = left.
  }

  public implementation all k,v ~~ iter[rbtree[k,v]->>keyval[k,v]] => let{.
    iter:all x ~~ (rbtree[k,v],x,(keyval[k,v],x)=>x)=>x.
    iter(.leaf,St,_) => St.
    iter(nbkNd(K,V,L,R),St,F) =>
      iter(R,F(K->V,iter(L,St,F)),F).
    iter(bbkNd(K,V,L,R),St,F) =>
      iter(R,F(K->V,iter(L,St,F)),F).
    iter(bkNd(K,V,L,R),St,F) =>
      iter(R,F(K->V,iter(L,St,F)),F).
    iter(rdNd(K,V,L,R),St,F) =>
      iter(R,F(K->V,iter(L,St,F)),F).
  .} in {
    _iter(Tr,St,Fn) => iter(Tr,St,Fn)
  }

  public implementation all k,v ~~ sizeable[rbtree[k,v]] => let{.
    count(.leaf,Cx)=>Cx.
    count(nbkNd(_,_,L,R),Cx)=>count(R,count(L,Cx+1)).
    count(bbkNd(_,_,L,R),Cx)=>count(R,count(L,Cx+1)).
    count(bkNd(_,_,L,R),Cx)=>count(R,count(L,Cx+1)).
    count(rdNd(_,_,L,R),Cx)=>count(R,count(L,Cx+1)).
  .} in {
    size(T)=>count(T,0).
    isEmpty(.leaf)=>.true.
    isEmpty(.bleaf)=>.true.
    isEmpty(_) default => .false
  }

  public implementation all k,v ~~ equality[k],comp[k] |:
    coercion[cons[keyval[k,v]],rbtree[k,v]] => {
      _coerce(L) => some(foldRight((K->V,M)=>add(M,K,V),.leaf,L)).
    }.

  public implementation all k,v ~~ equality[k],comp[k] |:
    coercion[rbtree[k,v],cons[keyval[k,v]]] => {
      _coerce(T) => some(pairs(T,[]))
    }.

  public implementation ixmap[rbtree] => let{.
    ixMap:all k,v,w ~~ (rbtree[k,v],(k,v)=>w) => rbtree[k,w].
    ixMap(.leaf,_) => .leaf.
    ixMap(nbkNd(K,V,L,R),f) => nbkNd(K,f(K,V),ixMap(L,f),ixMap(R,f)).
    ixMap(bbkNd(K,V,L,R),f) => bbkNd(K,f(K,V),ixMap(L,f),ixMap(R,f)).
    ixMap(bkNd(K,V,L,R),f) => bkNd(K,f(K,V),ixMap(L,f),ixMap(R,f)).
    ixMap(rdNd(K,V,L,R),f) => rdNd(K,f(K,V),ixMap(L,f),ixMap(R,f)).
  .} in{
    (M///f) => ixMap(M,f).
  }

  public implementation all k,v ~~ equality[k],comp[k] |:
    ixfilter[rbtree[k,v]->>k,v] => let{
      ixFilter(T,P) => ixLeft((K,V,A)=>(P(K,V) ? add(A,K,V) || A),.leaf,T).
    } in {
      (^//) = ixFilter
    }
}
