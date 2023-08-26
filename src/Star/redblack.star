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
  import star.strings.
  import star.tuples.

  -- red/black trees

  -- Based on Okasaki's book, with deletions by Matt Might

  color ::= .Red | .Black | .BBlack | .NBlack .

  implementation display[color]=>{
    disp(C) => case C in {
      .Red => "red".
      .Black => "blk".
      .BBlack => "bblk".
      .NBlack => "nblk"
    }
  }

  -- Only the type itself is exposed
  public rbtree[k,v] ::=
    .rbLeaf |
    .rbBlf |
    .rbNode(color,k,v,rbtree[k,v],rbtree[k,v]).

  find:all k,v ~~ comp[k], equality[k] |:(rbtree[k,v],k)=> option[v].
  find(.rbLeaf,_) => .none.
  find(.rbNode(_,K,V,_,_),K) => .some(V).
  find(.rbNode(_,K1,_,L,_),K) where K<K1 => find(L,K).
  find(.rbNode(_,_,_,_,R),K) => find(R,K).

  insert:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k,v) => rbtree[k,v].
  insert(T,K,V) => case add(T,K,V) in {
    .rbNode(_,Ky,Vy,A,B) => .rbNode(.Black,Ky,Vy,A,B).
    N => N
  }

  add:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k,v) => rbtree[k,v].
  add(.rbLeaf,K,V) => .rbNode(.Red,K,V,.rbLeaf,.rbLeaf).
  add(.rbNode(C,K,_,L,R),K,V) => .rbNode(C,K,V,L,R).
  add(.rbNode(C,K1,V1,L,R),K,V) where K<K1 =>
    balance(C,K1,V1,add(L,K,V),R).
  add(.rbNode(C,K1,V1,L,R),K,V) =>
    balance(C,K1,V1,L,add(R,K,V)).

  balance:all k,v ~~ comp[k],equality[k] |:
    (color,k,v,rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  balance(Cl,Kz,Vz,.rbNode(.Red,Ky,Vy,.rbNode(.Red,Kx,Vx,A,B),C),D) where isBlack(Cl) =>
    .rbNode(whiten(Cl),Ky,Vy,.rbNode(.Black,Kx,Vx,A,B),.rbNode(.Black,Kz,Vz,C,D)).
  balance(Cl where isBlack(Cl),Kz,Vz,.rbNode(.Red,Kx,Vx,A,.rbNode(.Red,Ky,Vy,B,C)),D) =>
    .rbNode(whiten(Cl),Ky,Vy,.rbNode(.Black,Kx,Vx,A,B),.rbNode(.Black,Kz,Vz,C,D)).
  balance(Cl where isBlack(Cl),Kx,Vx,A,.rbNode(.Red,Kz,Vz,.rbNode(.Red,Ky,Vy,B,C),D)) =>
    .rbNode(whiten(Cl),Ky,Vy,.rbNode(.Black,Kx,Vx,A,B),.rbNode(.Black,Kz,Vz,C,D)).
  balance(Cl where isBlack(Cl),Kx,Vx,A,.rbNode(.Red,Ky,Vy,B,.rbNode(.Red,Kz,Vz,C,D))) =>
    .rbNode(whiten(Cl),Ky,Vy,.rbNode(.Black,Kx,Vx,A,B),.rbNode(.Black,Kz,Vz,C,D)).
  balance(.BBlack,Kz,Vz,.rbNode(.NBlack,Kx,Vx,.rbNode(.Black,Kw,Vw,A,B),.rbNode(.Black,Ky,Vy,C,D)),E) =>
    .rbNode(.Black,Ky,Vy,balance(.Black,Ky,Vy,redden(.rbNode(.Black,Kw,Vw,A,B)),C),.rbNode(.Black,Kz,Vz,D,E)).
  balance(.BBlack,Kx,Vx,A,.rbNode(.NBlack,Kz,Vz,.rbNode(.Black,Ky,Vy,B,C),.rbNode(.Black,Kw,Vw,D,E)))=>
    .rbNode(.Black,Ky,Vy,.rbNode(.Black,Kx,Vx,A,B),balance(.Black,Kz,Vz,C,redden(.rbNode(.Black,Kw,Vw,D,E)))).

  balance(C,K,V,L,R) => .rbNode(C,K,V,L,R).

  delete:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k)=>rbtree[k,v].
  delete(T,K) => blacken(del(T,K)).
  
  del:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k)=>rbtree[k,v].
  del(.rbLeaf,_) => .rbLeaf.
  del(M where .rbNode(C,K,_,L,R).=M,K) => remove(C,L,R).
  del(.rbNode(C,K1,V1,A,B),K) where K<K1 => bubble(C,(K1,V1),del(A,K),B).
  del(.rbNode(C,K1,V1,A,B),K) where K>K1 => bubble(C,(K1,V1),A,del(B,K)).

  remove:all k,v ~~ comp[k],equality[k] |: (color,rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  remove(.Red,.rbLeaf,.rbLeaf)=>.rbLeaf.
  remove(.Black,.rbLeaf,.rbLeaf)=>.rbBlf.
  remove(.Black,.rbNode(.Red,K,V,L,R),.rbLeaf) => .rbNode(.Black,K,V,L,R).
  remove(.Black,.rbLeaf,.rbNode(.Red,K,V,L,R)) => .rbNode(.Black,K,V,L,R).
  remove(C,A,B) => bubble(C,rightMost(A),removeMax(A),B).

  removeMax:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v])=>rbtree[k,v].
  removeMax(.rbNode(Cl,_,_,L,.rbLeaf)) => remove(Cl,L,.rbLeaf).
  removeMax(.rbNode(Cl,K,V,L,R)) => bubble(Cl,(K,V),L,removeMax(R)).

  bubble:all k,v ~~ comp[k],equality[k] |: (color,(k,v),rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  bubble(C,(K,V),L,R) where (isBB(L) || isBB(R)) =>
    balance(darken(C),K,V,redder(L),redder(R)).
  bubble(C,(K,V),L,R) => balance(C,K,V,L,R).

  leftMost(.rbNode(_,K,V,.rbLeaf,_)) => (K,V).
  leftMost(.rbNode(_,_,_,_,T)) => leftMost(T).

  rightMost(.rbNode(_,K,V,_,.rbLeaf)) => (K,V).
  rightMost(.rbNode(_,_,_,_,T)) => rightMost(T).

  isBB:all k,v ~~ (rbtree[k,v])=>boolean.
  isBB(.rbBlf)=>.true.
  isBB(.rbNode(.BBlack,_,_,_,_))=>.true.
  isBB(_) default => .false.
  
  isBlack:(color)=>boolean.
  isBlack(.Black)=>.true.
  isBlack(.BBlack)=>.true.
  isBlack(_) default => .false.

  darken:(color)=>color.
  darken(.Red)=>.Black.
  darken(.Black)=>.BBlack.
  darken(.NBlack)=>.Red.

  whiten:(color)=>color.
  whiten(.Red)=>.NBlack.
  whiten(.Black)=>.Red.
  whiten(.BBlack)=>.Black.

  redden:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  redden(.rbNode(_,K,V,L,R))=>.rbNode(.Red,K,V,L,R).

  blacken:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  blacken(.rbLeaf) => .rbLeaf.
  blacken(.rbBlf) => .rbLeaf.
  blacken(.rbNode(_,K,V,L,R))=>.rbNode(.Black,K,V,L,R).

  redder:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  redder(.rbBlf)=>.rbLeaf.
  redder(.rbNode(Cl,K,V,L,R))=>.rbNode(whiten(Cl),K,V,L,R).
  
  -- Implement some of the standard contracts
  pairs:all k,v ~~ (rbtree[k,v],cons[keyval[k,v]]) => cons[keyval[k,v]].
  pairs(.rbLeaf,Ps) => Ps.
  pairs(.rbBlf,Ps) => Ps.
  pairs(.rbNode(_,K,V,L,R),Ps) => pairs(L,[K->V,..pairs(R,Ps)]).

  dispTree:all k,v ~~ display[k], display[v] |: (rbtree[k,v])=>string.
  dispTree(.rbLeaf)=>"leaf".
  dispTree(.rbBlf)=>"bleaf".
  dispTree(.rbNode(Cl,K,V,L,R)) => "($(Cl) #(dispTree(L))\:$(disp(K))->$(V)\:#(dispTree(R)))".

  public validRb:all k,v ~~ (rbtree[k,v])=>boolean.
  validRb(T) => validReds(T) && allSame(blackHeights(T,0,[])).

  validReds:all k,v ~~ (rbtree[k,v])=>boolean.
  validReds(.rbLeaf)=>.true.
  validReds(.rbNode(.Red,_,_,L,R)) =>
    validReds(L) && validReds(R) && ~isRedNode(L) && ~isRedNode(R).
  validReds(.rbNode(.Black,_,_,L,R)) => validReds(L) && validReds(R).
  validReds(_) default => .false.

  isRedNode:all k,v ~~ (rbtree[k,v])=>boolean.
  isRedNode(.rbNode(.Red,_,_,_,_)) => .true.
  isRedNode(_) default => .false.
	
  blackHeights:all k,v ~~ (rbtree[k,v],integer,cons[integer])=>cons[integer].
  blackHeights(.rbLeaf,H,Hs) => [H,..Hs].
  blackHeights(.rbNode(.Black,_,_,L,R),H,Hs) =>
    blackHeights(L,H+1,blackHeights(R,H+1,Hs)).
  blackHeights(.rbNode(.Red,_,_,L,R),H,Hs) =>
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
      _empty = .rbLeaf.
    }.

  public implementation all k,v ~~ comp[k], equality[k] |:
    sequence[rbtree[k,v] ->> keyval[k,v]] => {
      _nil = .rbLeaf.
      _cons(K->V,Tr) => add(Tr,K,V).
    }.

  public implementation all k,v ~~ equality[k],comp[k] |:
    stream[rbtree[k,v]->>keyval[k,v]] => let{.
      hdtl(T) where H?=hd(T) => .some((H,drop(T,H))).
      hdtl(_) default => .none.

      private
      hd(.rbLeaf)=>.none.
      hd(.rbNode(_,K,V,.rbLeaf,_)) => .some(K->V).
      hd(.rbNode(_,_,_,L,_)) => hd(L).

      drop(T,K->_) => delete(T,K)
    .} in {
      _eof(.rbLeaf) => .true.
      _eof(_) default => .false.

      _hdtl(T) => hdtl(T)
    }.

  public implementation all k,v ~~ ixfold[rbtree[k,v]->>k,v] => let{.
    right(F,U,.rbLeaf) => U.
    right(F,U,.rbBlf) => U.
    right(F,U,.rbNode(_,K,V,L,R)) => right(F,F(K,V,right(F,U,R)),L).

    left(F,U,.rbLeaf) => U.
    left(F,U,.rbNode(_,K,V,L,R)) => left(F,F(K,V,left(F,U,L)),R).
  .} in {
    ixRight = right.
    ixLeft = left.
  }

  public implementation all k,v ~~ iter[rbtree[k,v]->>keyval[k,v]] => let{.
    iter:all x ~~ (rbtree[k,v],x,(keyval[k,v],x)=>x)=>x.
    iter(.rbLeaf,St,_) => St.
    iter(.rbNode(_,K,V,L,R),St,F) =>
      iter(R,F(K->V,iter(L,St,F)),F).
  .} in {
    _iter(Tr,St,Fn) => iter(Tr,St,Fn)
  }

  public implementation all k,v ~~ generate[rbtree[k,v]->>keyval[k,v]] => {
    _generate(T) => generator{
      let{.
	loop:(rbtree[k,v]) => ().
	loop(.rbLeaf) => ().
	loop(.rbNode(_,K,V,L,R)) => valof{
	  loop(L);
	  yield (K->V);
	  loop(R);
	  valis ()
	}
      .}
      in loop(T);
    }
  }
      
  public implementation all k,v ~~ sizeable[rbtree[k,v]] => let{.
    count(.rbLeaf,Cx)=>Cx.
    count(.rbNode(_,_,_,L,R),Cx)=>count(R,count(L,Cx+1)).
  .} in {
    size(T)=>count(T,0).
    isEmpty(.rbLeaf)=>.true.
    isEmpty(.rbBlf)=>.true.
    isEmpty(_) default => .false
  }

  public implementation all k,v ~~ equality[k],comp[k] |:
    coercion[cons[keyval[k,v]],rbtree[k,v]] => {
      _coerce(L) => .some(foldRight((K->V,M)=>add(M,K,V),.rbLeaf,L)).
    }.

  public implementation all k,v ~~ equality[k],comp[k] |:
    coercion[rbtree[k,v],cons[keyval[k,v]]] => {
      _coerce(T) => .some(pairs(T,[]))
    }.

  public implementation ixmap[rbtree] => let{.
    ixMap:all k,v,w ~~ (rbtree[k,v],(k,v)=>w) => rbtree[k,w].
    ixMap(.rbLeaf,_) => .rbLeaf.
    ixMap(.rbNode(Cl,K,V,L,R),f) => .rbNode(Cl,K,f(K,V),ixMap(L,f),ixMap(R,f)).
  .} in{
    (M///f) => ixMap(M,f).
  }

  public implementation all k,v ~~ equality[k],comp[k] |:
    ixfilter[rbtree[k,v]->>k,v] => let{
      ixFilter(T,P) => ixLeft((K,V,A)=>(P(K,V) ?? add(A,K,V) || A),.rbLeaf,T).
    } in {
      (^//) = ixFilter
    }
}
