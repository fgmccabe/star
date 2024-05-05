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

  -- Based on Okasaki book, with deletions by Matt Might

  color ::= .Red | .Blk | .BBlack.

  implementation display[color]=>{
    disp(C) => case C in {
      | .Red => "red"
      | .Blk => "blk"
      | .BBlack => "bblk"
    }
  }

  -- Only the type itself is exposed
  public rbtree[k,v] ::=
    .lf |
    .blf |
    .nd(color,rbtree[k,v],k,v,rbtree[k,v]).

  find:all k,v ~~ comp[k], equality[k] |:(rbtree[k,v],k)=> option[v].
  find(.lf,_) => .none.
  find(.nd(_,_,K,V,_),K) => .some(V).
  find(.nd(_,L,K1,_,_),K) where K<K1 => find(L,K).
  find(.nd(_,_,_,_,R),K) => find(R,K).

  insert:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k,v) => rbtree[k,v].
  insert(T,K,V) => blacken(ins(T,K,V)).

  ins:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k,v) => rbtree[k,v].
  ins(.blf,K,V) => .nd(.Red,.lf,K,V,.lf).
  ins(.lf,K,V) => .nd(.Red,.lf,K,V,.lf).
  ins(.nd(C,L,K,_,R),K,V) => .nd(C,L,K,V,R).
  ins(.nd(C,L,K1,V1,R),K,V) where K<K1 =>
    balance(C,ins(L,K,V),K1,V1,R).
  ins(.nd(C,L,K1,V1,R),K,V) =>
    balance(C,L,K1,V1,ins(R,K,V)).

  balance:all k,v ~~ (color,rbtree[k,v],k,v,rbtree[k,v])=>rbtree[k,v].
  balance(.Blk,.nd(.Red,.nd(.Red,A,Kx,Vx,B),Ky,Vy,C),Kz,Vz,D) =>
    .nd(.Red,.nd(.Blk,A,Kx,Vx,B),Ky,Vy,.nd(.Blk,C,Kz,Vz,D)).
  balance(.Blk,.nd(.Red,A,Kx,Vx,.nd(.Red,B,Ky,Vy,C)),Kz,Vz,D) =>
    .nd(.Red,.nd(.Blk,A,Kx,Vx,B),Ky,Vy,.nd(.Blk,C,Kz,Vz,D)).
  balance(.Blk,A,Kx,Vx,.nd(.Red,.nd(.Red,B,Ky,Vy,C),Kz,Vz,D)) =>
    .nd(.Red,.nd(.Blk,A,Kx,Vx,B),Ky,Vy,.nd(.Blk,C,Kz,Vz,D)).
  balance(.Blk,A,Kx,Vx,.nd(.Red,B,Ky,Vy,.nd(.Red,C,Kz,Vz,D))) =>
    .nd(.Red,.nd(.Blk,A,Kx,Vx,B),Ky,Vy,.nd(.Blk,C,Kz,Vz,D)).


  balance(.BBlack,.nd(.Red,.nd(.Red,A,Kx,Vx,B),Ky,Vy,C),Kz,Vz,D) =>
    .nd(.Blk,.nd(.Blk,A,Kx,Vx,B),Ky,Vy,.nd(.Blk,C,Kz,Vz,D)).
  balance(.BBlack,.nd(.Red,A,Kx,Vx,.nd(.Red,B,Ky,Vy,C)),Kz,Vz,D) =>
    .nd(.Blk,.nd(.Blk,A,Kx,Vx,B),Ky,Vy,.nd(.Blk,C,Kz,Vz,D)).
  balance(.BBlack,A,Kx,Vx,.nd(.Red,.nd(.Red,B,Ky,Vy,C),Kz,Vz,D)) =>
    .nd(.Blk,.nd(.Blk,A,Kx,Vx,B),Ky,Vy,.nd(.Blk,C,Kz,Vz,D)).
  balance(.BBlack,.nd(.Red,A,Kx,Vx,.nd(.Red,B,Ky,Vy,C)),Kz,Vz,D) =>
    .nd(.Blk,.nd(.Blk,A,Kx,Vx,B),Ky,Vy,.nd(.Blk,C,Kz,Vz,D)).

  balance(C,L,K,V,R) => .nd(C,L,K,V,R).

  delete:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k)=>rbtree[k,v].
  delete(T,K) => del(redden(T),K).
  
  del:all k,v ~~ comp[k],equality[k] |: (rbtree[k,v],k)=>rbtree[k,v].
  del(.lf,_) => .lf.
  del(.nd(.Red,.lf,ky,vy,.lf),k) => (ky==k ?? .lf || .nd(.Red,.lf,ky,vy,.lf)).
  del(.nd(.Blk,.lf,ky,vy,.lf),k) => (ky==k ?? .blf || .nd(.Blk,.lf,ky,vy,.lf)).
  del(.nd(.Blk,.nd(.Red,.lf,ky,vy,.lf),kz,vz,.lf),k) => (
    k < kz ??
    .nd(.Blk,del(.nd(.Red,.lf,ky,vy,.lf),k),kz,vz,.lf) ||
    k==kz ??
    .nd(.Blk,.lf,ky,vy,.lf) ||
    .nd(.Blk,.nd(.Red,.lf,ky,vy,.lf),kz,vz,.lf)).
  del(.nd(Cl,A,ky,vy,B),k) => (
    k < ky ?? rotate(Cl,del(A,k),ky,vy,B) ||
    k == ky ?? valof{
      (ky1,vy1,b1) = min_del(B);
      valis rotate(Cl,A,ky1,vy1,b1)
    } || rotate(Cl,A,ky,vy,del(B,k))).

  min_del:all k,v ~~ (rbtree[k,v])=>(k,v,rbtree[k,v]).
  min_del(.nd(.Red,.lf,kx,vx,.lf)) => (kx,vx,.lf).
  min_del(.nd(.Blk,.lf,kx,vx,.lf)) => (kx,vx,.blf).
  min_del(.nd(.Blk,.lf,kx,vx,.nd(.Red,.lf,ky,vy,.lf))) => (kx,vx,.nd(.Blk,.lf,ky,vy,.lf)).
  min_del(.nd(Cl,A,kx,vx,B)) => valof{
    (k1,v1,a1) = min_del(A);
    valis (k1,v1,rotate(Cl,a1,kx,vx,B))
  }

  rotate:all k,v ~~ (color,rbtree[k,v],k,v,rbtree[k,v])=>rbtree[k,v].
  rotate(.Red,.nd(.BBlack,A,kx,vx,B),ky,vy,.nd(.Blk,C,kz,vz,D)) =>
    balance(.Blk,.nd(.Red,.nd(.Blk,A,kx,vx,B),ky,vy,C),kz,vz,D).
  rotate(.Red,.blf,ky,vy,.nd(.Blk,C,kz,vz,D))=>
    balance(.Blk,.nd(.Red,.lf,ky,vy,C),kz,vz,D).
  rotate(.Red,.nd(.Blk,A,kx,vx,B),ky,vy,.nd(.BBlack,C,kz,vz,D)) =>
    balance(.Blk,A,kx,vx,.nd(.Red,B,ky,vy,.nd(.Blk,C,kz,vz,D))).
  rotate(.Red,.nd(.Blk,A,kx,vx,B),ky,vy,.blf) =>
    balance(.Blk,A,kx,vx,.nd(.Red,B,ky,vy,.lf)).
  rotate(.Blk,.nd(.BBlack,A,kx,vx,B),ky,vy,.nd(.Blk,C,kz,vz,D)) =>
    balance(.BBlack,.nd(.Red,.nd(.Blk,A,kx,vx,B),ky,vy,C),kz,vz,D).
  rotate(.Blk,.blf,ky,vy,.nd(.Blk,C,kz,vz,D))=>
    balance(.BBlack,.nd(.Red,.lf,ky,vy,C),kz,vz,D).
  rotate(.Blk,.nd(.Blk,A,kx,vx,B),ky,vy,.nd(.BBlack,C,kz,vz,D)) =>
    balance(.BBlack,A,kx,vx,.nd(.Red,B,ky,vy,.nd(.Blk,C,kz,vz,D))).
  rotate(.Blk,.nd(.Blk,A,kx,vx,B),ky,vy,.blf) =>
    balance(.BBlack,A,kx,vx,.nd(.Red,B,ky,vy,.lf)).
  rotate(.Blk,.nd(.BBlack,A,kw,vw,B),kx,vx,.nd(.Red,.nd(.Blk,C,ky,vy,D),kz,vz,E)) =>
    .nd(.Blk,balance(.Blk,.nd(.Red,.nd(.Blk,A,kw,vw,B),kx,vx,C),ky,vy,D),kz,vz,E).
  rotate(.Blk,.blf,kx,vx,.nd(.Red,.nd(.Blk,C,ky,vy,D),kz,vz,E)) =>
    .nd(.Blk,balance(.Blk,.nd(.Red,.lf,kx,vx,C),ky,vy,D),kz,vz,E).
  rotate(.Blk,.nd(.Red,A,kw,vw,.nd(.Blk,B,kx,vx,C)),ky,vy,.nd(.BBlack,D,kz,vz,E)) =>
    .nd(.Blk,A,kw,vw,balance(.Blk,B,kx,vx,.nd(.Red,C,ky,vy,.nd(.Blk,D,kz,vz,E)))).
  rotate(.Blk,.nd(.Red,A,kw,vw,.nd(.Blk,B,kx,vx,C)),ky,vy,.blf) =>
    .nd(.Blk,A,kw,vw,balance(.Blk,B,kx,vx,.nd(.Red,C,ky,vy,.lf))).
  rotate(Cl,A,K,V,B) => .nd(Cl,A,K,V,B).

    
  redden:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  redden(.nd(.Blk,.nd(.Blk,A,kx,vx,B),ky,vy,.nd(.Blk,C,kz,vz,D))) =>
    .nd(.Red,.nd(.Blk,A,kx,vx,B),ky,vy,.nd(.Blk,C,kz,vz,D)).
  redden(.blf) => .lf.
  redden(T) default => T.

  blacken:all k,v ~~ (rbtree[k,v])=>rbtree[k,v].
  blacken(.nd(.Red,.nd(.Red,A,kx,vx,B),ky,vy,C)) => .nd(.Blk,.nd(.Red,A,kx,vx,B),ky,vy,C).
  blacken(.nd(.Red,A,kx,vx,.nd(.Red,B,ky,vy,C))) => .nd(.Blk,A,kx,vx,.nd(.Red,B,ky,vy,C)).
  blacken(T) default => T.

  -- Implement some of the standard contracts
  pairs:all k,v ~~ (rbtree[k,v],cons[keyval[k,v]]) => cons[keyval[k,v]].
  pairs(.lf,Ps) => Ps.
  pairs(.blf,Ps) => Ps.
  pairs(.nd(_,L,K,V,R),Ps) => pairs(L,[K->V,..pairs(R,Ps)]).

  dispTree:all k,v ~~ display[k], display[v] |: (rbtree[k,v])=>string.
  dispTree(.lf)=>"leaf".
  dispTree(.blf)=>"bleaf".
  dispTree(.nd(Cl,L,K,V,R)) => "($(Cl) #(dispTree(L))\:$(disp(K))->$(V)\:#(dispTree(R)))".

  public validRb:all k,v ~~ (rbtree[k,v])=>boolean.
  validRb(T) => validReds(T) && allSame(blackHeights(T,0,[])).

  validReds:all k,v ~~ (rbtree[k,v])=>boolean.
  validReds(.lf)=>.true.
  validReds(.blf)=>.true.
  validReds(.nd(.Red,L,_,_,R)) =>
    validReds(L) && validReds(R) && ~isRedNode(L) && ~isRedNode(R).
  validReds(.nd(.Blk,L,_,_,R)) => validReds(L) && validReds(R).
  validReds(_) default => .false.

  isRedNode:all k,v ~~ (rbtree[k,v])=>boolean.
  isRedNode(.nd(.Red,_,_,_,_)) => .true.
  isRedNode(_) default => .false.
	
  blackHeights:all k,v ~~ (rbtree[k,v],integer,cons[integer])=>cons[integer].
  blackHeights(.lf,H,Hs) => [H,..Hs].
  blackHeights(.blf,H,Hs) => [H+1,..Hs].
  blackHeights(.nd(.Blk,L,_,_,R),H,Hs) =>
    blackHeights(L,H+1,blackHeights(R,H+1,Hs)).
  blackHeights(.nd(.BBlack,L,_,_,R),H,Hs) =>
    blackHeights(L,H+2,blackHeights(R,H+2,Hs)).
  blackHeights(.nd(.Red,L,_,_,R),H,Hs) =>
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
      _empty = .lf.
    }.

  public implementation all k,v ~~ comp[k], equality[k] |:
    build[rbtree[k,v] ->> keyval[k,v]] => {
      _null = .lf.
      _push(K->V,Tr) => insert(Tr,K,V).
    }.

  public implementation all k,v ~~ comp[k], equality[k] |:
    sequence[rbtree[k,v] ->> keyval[k,v]] => {
      _nil = .lf.
      _cons(K->V,Tr) => insert(Tr,K,V).
    }.

  public implementation all k,v ~~ equality[k],comp[k] |:
    stream[rbtree[k,v]->>keyval[k,v]] => let{.
    hdtl(T) where H?=hd(T) => .some((H,drop(T,H))).
    hdtl(_) default => .none.

    private hd(.lf)=>.none.
    hd(.nd(_,.lf,K,V,_)) => .some(K->V).
    hd(.nd(_,L,_,_,_)) => hd(L).

    drop(T,K->_) => delete(T,K)
    .} in {
    _eof(.lf) => .true.
    _eof(_) default => .false.
    
    _hdtl(T) => hdtl(T)
    }.

  public implementation all k,v ~~ ixfold[rbtree[k,v]->>k,v] => let{.
    right(F,U,.lf) => U.
    right(F,U,.blf) => U.
    right(F,U,.nd(_,L,K,V,R)) => right(F,F(K,V,right(F,U,R)),L).

    left(F,U,.lf) => U.
    left(F,U,.nd(_,L,K,V,R)) => left(F,F(K,V,left(F,U,L)),R).
  .} in {
    ixRight = right.
    ixLeft = left.
  }

  public implementation all k,v ~~ iter[rbtree[k,v]->>keyval[k,v]] => let{.
    iter:all x ~~ (rbtree[k,v],x,(keyval[k,v],x)=>x)=>x.
    iter(.lf,St,_) => St.
    iter(.nd(_,L,K,V,R),St,F) =>
      iter(R,F(K->V,iter(L,St,F)),F).
  .} in {
    _iter(Tr,St,Fn) => iter(Tr,St,Fn)
  }

  public implementation all k,v ~~ generate[rbtree[k,v]->>keyval[k,v]] => {
    _generate(T) => generator{
      let{.
	loop:(rbtree[k,v]) => ().
	loop(.lf) => ().
	loop(.nd(_,L,K,V,R)) => valof{
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
    count(.lf,Cx)=>Cx.
    count(.nd(_,L,_,_,R),Cx)=>count(R,count(L,Cx+1)).
  .} in {
    size(T)=>count(T,0).
    isEmpty(.lf)=>.true.
    isEmpty(.blf)=>.true.
    isEmpty(_) default => .false
  }

  public implementation all k,v ~~ equality[k],comp[k] |:
    coercion[cons[keyval[k,v]],rbtree[k,v]] => {
      _coerce(L) => .some(foldRight((K->V,M)=>insert(M,K,V),.lf,L)).
    }.

  public implementation all k,v ~~ equality[k],comp[k] |:
    coercion[rbtree[k,v],cons[keyval[k,v]]] => {
      _coerce(T) => .some(pairs(T,[]))
    }.

  public implementation ixmap[rbtree] => let{.
    ixMap:all k,v,w ~~ (rbtree[k,v],(k,v)=>w) => rbtree[k,w].
    ixMap(.lf,_) => .lf.
    ixMap(.nd(Cl,L,K,V,R),f) => .nd(Cl,ixMap(L,f),K,f(K,V),ixMap(R,f)).
  .} in{
    (M///f) => ixMap(M,f).
  }

  public implementation all k,v ~~ equality[k],comp[k] |:
    ixfilter[rbtree[k,v]->>k,v] => let{
      ixFilter(T,P) => ixLeft((K,V,A)=>(P(K,V) ?? insert(A,K,V) || A),.lf,T).
    } in {
      (^//) = ixFilter
    }
}
