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
  import star.fiber.

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
    private .leaf |
      private .bleaf |
      private .node(color,k,v,rbtree[k,v],rbtree[k,v]).

  find:all k,v ~~ comp[k], equality[k] |:(rbtree[k,v],k)=> option[v].
  find(.leaf,_) => .none.
  find(.node(_,K,V,_,_),K) => .some(V).
  find(.node(_,K1,_,L,_),K) where K<K1 => find(L,K).
  find(.node(_,_,_,_,R),K) => find(R,K).

  insert:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k,v) => rbtree[k,v].
  insert(T,K,V) => case add(T,K,V) in {
    .node(_,Ky,Vy,A,B) => .node(.Black,Ky,Vy,A,B).
    N => N
  }

  add:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k,v) => rbtree[k,v].
  add(.leaf,K,V) => .node(.Red,K,V,.leaf,.leaf).
  add(.node(C,K,_,L,R),K,V) => .node(C,K,V,L,R).
  add(.node(C,K1,V1,L,R),K,V) where K<K1 =>
    balance(C,K1,V1,add(L,K,V),R).
  add(.node(C,K1,V1,L,R),K,V) =>
    balance(C,K1,V1,L,add(R,K,V)).

  balance:all k,v ~~ comp[k],equality[k] |:
    (color,k,v,rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  balance(Cl,Kz,Vz,.node(.Red,Ky,Vy,.node(.Red,Kx,Vx,A,B),C),D) where isBlack(Cl) =>
    .node(whiten(Cl),Ky,Vy,.node(.Black,Kx,Vx,A,B),.node(.Black,Kz,Vz,C,D)).
  balance(Cl where isBlack(Cl),Kz,Vz,.node(.Red,Kx,Vx,A,.node(.Red,Ky,Vy,B,C)),D) =>
    .node(whiten(Cl),Ky,Vy,.node(.Black,Kx,Vx,A,B),.node(.Black,Kz,Vz,C,D)).
  balance(Cl where isBlack(Cl),Kx,Vx,A,.node(.Red,Kz,Vz,.node(.Red,Ky,Vy,B,C),D)) =>
    .node(whiten(Cl),Ky,Vy,.node(.Black,Kx,Vx,A,B),.node(.Black,Kz,Vz,C,D)).
  balance(Cl where isBlack(Cl),Kx,Vx,A,.node(.Red,Ky,Vy,B,.node(.Red,Kz,Vz,C,D))) =>
    .node(whiten(Cl),Ky,Vy,.node(.Black,Kx,Vx,A,B),.node(.Black,Kz,Vz,C,D)).
  balance(.BBlack,Kz,Vz,.node(.NBlack,Kx,Vx,.node(.Black,Kw,Vw,A,B),.node(.Black,Ky,Vy,C,D)),E) =>
    .node(.Black,Ky,Vy,balance(.Black,Ky,Vy,redden(.node(.Black,Kw,Vw,A,B)),C),.node(.Black,Kz,Vz,D,E)).
  balance(.BBlack,Kx,Vx,A,.node(.NBlack,Kz,Vz,.node(.Black,Ky,Vy,B,C),.node(.Black,Kw,Vw,D,E)))=>
    .node(.Black,Ky,Vy,.node(.Black,Kx,Vx,A,B),balance(.Black,Kz,Vz,C,redden(.node(.Black,Kw,Vw,D,E)))).

  balance(C,K,V,L,R) => .node(C,K,V,L,R).

  delete:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k)=>rbtree[k,v].
  delete(T,K) => blacken(del(T,K)).
  
  del:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k)=>rbtree[k,v].
  del(.leaf,_) => .leaf.
  del(M where .node(C,K,_,L,R).=M,K) => remove(C,L,R).
  del(.node(C,K1,V1,A,B),K) where K<K1 => bubble(C,(K1,V1),del(A,K),B).
  del(.node(C,K1,V1,A,B),K) where K>K1 => bubble(C,(K1,V1),A,del(B,K)).

  remove:all k,v ~~ comp[k],equality[k] |: (color,rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  remove(.Red,.leaf,.leaf)=>.leaf.
  remove(.Black,.leaf,.leaf)=>.bleaf.
  remove(.Black,.node(.Red,K,V,L,R),.leaf) => .node(.Black,K,V,L,R).
  remove(.Black,.leaf,.node(.Red,K,V,L,R)) => .node(.Black,K,V,L,R).
  remove(C,A,B) => bubble(C,rightMost(A),removeMax(A),B).

  removeMax:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v])=>rbtree[k,v].
  removeMax(.node(Cl,_,_,L,.leaf)) => remove(Cl,L,.leaf).
  removeMax(.node(Cl,K,V,L,R)) => bubble(Cl,(K,V),L,removeMax(R)).

  bubble:all k,v ~~ comp[k],equality[k] |: (color,(k,v),rbtree[k,v],rbtree[k,v])=>rbtree[k,v].
  bubble(C,(K,V),L,R) where (isBB(L) || isBB(R)) =>
    balance(darken(C),K,V,redder(L),redder(R)).
  bubble(C,(K,V),L,R) => balance(C,K,V,L,R).

  leftMost(.node(_,K,V,.leaf,_)) => (K,V).
  leftMost(.node(_,_,_,_,T)) => leftMost(T).

  rightMost(.node(_,K,V,_,.leaf)) => (K,V).
  rightMost(.node(_,_,_,_,T)) => rightMost(T).

  isBB:all k,v ~~ (rbtree[k,v])=>boolean.
  isBB(.bleaf)=>.true.
  isBB(.node(.BBlack,_,_,_,_))=>.true.
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
  redden(.node(_,K,V,L,R))=>.node(.Red,K,V,L,R).

  blacken:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  blacken(.leaf) => .leaf.
  blacken(.bleaf) => .leaf.
  blacken(.node(_,K,V,L,R))=>.node(.Black,K,V,L,R).

  redder:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  redder(.bleaf)=>.leaf.
  redder(.node(Cl,K,V,L,R))=>.node(whiten(Cl),K,V,L,R).
  
  -- Implement some of the standard contracts
  pairs:all k,v ~~ (rbtree[k,v],cons[keyval[k,v]]) => cons[keyval[k,v]].
  pairs(.leaf,Ps) => Ps.
  pairs(.bleaf,Ps) => Ps.
  pairs(.node(_,K,V,L,R),Ps) => pairs(L,[K->V,..pairs(R,Ps)]).

  dispTree:all k,v ~~ display[k], display[v] |: (rbtree[k,v])=>string.
  dispTree(.leaf)=>"leaf".
  dispTree(.bleaf)=>"bleaf".
  dispTree(.node(Cl,K,V,L,R)) => "($(Cl) #(dispTree(L))\:$(disp(K))->$(V)\:#(dispTree(R)))".

  public validRb:all k,v ~~ (rbtree[k,v])=>boolean.
  validRb(T) => validReds(T) && allSame(blackHeights(T,0,[])).

  validReds:all k,v ~~ (rbtree[k,v])=>boolean.
  validReds(.leaf)=>.true.
  validReds(.node(.Red,_,_,L,R)) =>
    validReds(L) && validReds(R) && ~isRedNode(L) && ~isRedNode(R).
  validReds(.node(.Black,_,_,L,R)) => validReds(L) && validReds(R).
  validReds(_) default => .false.

  isRedNode:all k,v ~~ (rbtree[k,v])=>boolean.
  isRedNode(.node(.Red,_,_,_,_)) => .true.
  isRedNode(_) default => .false.
	
  blackHeights:all k,v ~~ (rbtree[k,v],integer,cons[integer])=>cons[integer].
  blackHeights(.leaf,H,Hs) => [H,..Hs].
  blackHeights(.node(.Black,_,_,L,R),H,Hs) =>
    blackHeights(L,H+1,blackHeights(R,H+1,Hs)).
  blackHeights(.node(.Red,_,_,L,R),H,Hs) =>
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
      hdtl(T) where H?=hd(T) => .some((H,drop(T,H))).
      hdtl(_) default => .none.

      private
      hd(.leaf)=>.none.
      hd(.node(_,K,V,.leaf,_)) => .some(K->V).
      hd(.node(_,_,_,L,_)) => hd(L).

      drop(T,K->_) => delete(T,K)
    .} in {
      _eof(.leaf) => .true.
      _eof(_) default => .false.

      _hdtl(T) => hdtl(T)
    }.

  public implementation all k,v ~~ ixfold[rbtree[k,v]->>k,v] => let{.
    right(F,U,.leaf) => U.
    right(F,U,.bleaf) => U.
    right(F,U,.node(_,K,V,L,R)) => right(F,F(K,V,right(F,U,R)),L).

    left(F,U,.leaf) => U.
    left(F,U,.node(_,K,V,L,R)) => left(F,F(K,V,left(F,U,L)),R).
  .} in {
    ixRight = right.
    ixLeft = left.
  }

  public implementation all k,v ~~ iter[rbtree[k,v]->>keyval[k,v]] => let{.
    iter:all x ~~ (rbtree[k,v],x,(keyval[k,v],x)=>x)=>x.
    iter(.leaf,St,_) => St.
    iter(.node(_,K,V,L,R),St,F) =>
      iter(R,F(K->V,iter(L,St,F)),F).
  .} in {
    _iter(Tr,St,Fn) => iter(Tr,St,Fn)
  }

  public implementation all k,v ~~ generate[rbtree[k,v]->>keyval[k,v]] => {
    _generate(T) => generator{
      let{.
	loop:(rbtree[k,v]) => () throws res_generator.
	loop(.leaf) => ().
	loop(.node(_,K,V,L,R)) => valof{
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
    count(.leaf,Cx)=>Cx.
    count(.node(_,_,_,L,R),Cx)=>count(R,count(L,Cx+1)).
  .} in {
    size(T)=>count(T,0).
    isEmpty(.leaf)=>.true.
    isEmpty(.bleaf)=>.true.
    isEmpty(_) default => .false
  }

  public implementation all k,v ~~ equality[k],comp[k] |:
    coercion[cons[keyval[k,v]],rbtree[k,v]] => {
      _coerce(L) => .some(foldRight((K->V,M)=>add(M,K,V),.leaf,L)).
    }.

  public implementation all k,v ~~ equality[k],comp[k] |:
    coercion[rbtree[k,v],cons[keyval[k,v]]] => {
      _coerce(T) => .some(pairs(T,[]))
    }.

  public implementation ixmap[rbtree] => let{.
    ixMap:all k,v,w ~~ (rbtree[k,v],(k,v)=>w) => rbtree[k,w].
    ixMap(.leaf,_) => .leaf.
    ixMap(.node(Cl,K,V,L,R),f) => .node(Cl,K,f(K,V),ixMap(L,f),ixMap(R,f)).
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
