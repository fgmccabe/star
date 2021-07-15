star.cons{
  import star.core.
  import star.arith.
  import star.iterable.
  import star.monad.
  import star.coerce.

  public implementation all x ~~ equality[x] |: equality[cons[x]] => let{
    smList:all x ~~ equality[x] |: (cons[x],cons[x]) => boolean.
    smList(.nil,.nil) => .true.
    smList(cons(x,xr),cons(y,yr)) => x==y && smList(xr,yr).
    smList(_,_) default => .false.
  } in {.
    L1 == L2 => smList(L1,L2).
  .}

  public implementation all x ~~ hash[x] |: hash[cons[x]] => {
    hash(L) => cHash(L,0).
  }

  cHash:all x ~~ hash[x] |: (cons[x],integer) => integer.
  cHash(.nil,X) => X.
  cHash(cons(x,xr),H) => cHash(xr,(H+hash(x))*37).

  public implementation all x ~~ comp[x],equality[x] |: comp[cons[x]] => let{
    consLess(.nil,cons(_,_)) => .true.
    consLess(cons(H1,T1),cons(H2,T2)) where H1<H2 => .true.
    consLess(cons(H,T1),cons(H,T2)) => consLess(T1,T2).
    consLess(_,_) default => .false.

    consGe(L1,L2) => ~consLess(L2,L1).
  } in {. (<) = consLess. (>=) = consGe .}

  -- stream & sequence contracts
  public implementation all x ~~ stream[cons[x] ->> x] => {
    _eof(.nil) => .true.
    _eof(cons(_,_)) => .false.
    
    _hdtl(cons(H,T)) => some((H,T)).
    _hdtl(.nil) => .none.
  }

  public implementation all x ~~ sequence[cons[x] ->> x] => {
    _cons(E,S) => cons(E,S).
    _nil = .nil.
  }

  public implementation all e ~~ sizeable[cons[e]] => let{
    consLength:all e ~~ (cons[e],integer) => integer.
    consLength(.nil,Ln) => Ln.
    consLength(cons(_,T),Ln) => consLength(T,Ln+1).
  } in {.
    size(L) => consLength(L,0).
    isEmpty(.nil) => .true.
    isEmpty(_) default => .false.
  .}

  public implementation all e ~~ measured[cons[e]->>integer] => {.
    [|L|] => size(L)
  .}

  last:all e ~~ (cons[e]) => (cons[e],e).
  last(cons(X,.nil)) => (.nil,X).
  last(cons(X,Y)) where (L,E) .= last(Y) => (cons(X,L),E).

  public implementation all x ~~ concat[cons[x]] => {
    X++Y => concat(X,Y).
  }

  concat: all e ~~ (cons[e],cons[e])=>cons[e].
  concat(.nil,Y) => Y.
  concat(cons(E,X),Y) => cons(E,concat(X,Y)).

  public multicat : all e ~~ (cons[cons[e]]) => cons[e].
  multicat(.nil) => .nil.
  multicat(cons(H,T)) => concat(H,multicat(T)).

  public implementation all x ~~ reversible[cons[x]] => {
    reverse(L) => rev(L,.nil).

    private rev:(cons[x],cons[x])=>cons[x].
    rev(.nil,R) => R.
    rev(cons(E,L),R) => rev(L,cons(E,R)).
  }

  public implementation all x ~~ head[cons[x]->>x] => {
    head(cons(E,_)) => some(E).
    head(.nil) => .none.

    tail(cons(_,T)) => some(T).
    tail(.nil) => .none.
  }

  public front:all e ~~ (cons[e],integer)=>option[(cons[e],cons[e])].
  front(Els,Ln) => let{
    ff(Es,So,0) => some((reverse(So),Es)).
    ff([E,..Es],So,Ix) =>
      ff(Es,[E,..So],Ix-1).
    ff(_,_,_) default => .none.
  } in ff(Els,[],Ln).

  public zip: all e,f ~~ (cons[e],cons[f])=>cons[(e,f)].
  zip([],[]) => [].
  zip([E,..Es],[F,..Fs]) => [(E,F),..zip(Es,Fs)].

  public unzip:all e,f ~~ (cons[(e,f)])=>(cons[e],cons[f]).
  unzip([]) => ([],[]).
  unzip([(A,B),..Ls]) where
      (L,R) .= unzip(Ls) => ([A,..L],[B,..R]).

  -- Implement iteration of executions over a cons list
  public implementation all t ~~ iter[cons[t]->>t] => {
    _iter(.nil,St,_) => St.
    _iter(cons(H,T),St,Fn) => _sequence(St,(SS)=>_iter(T,Fn(H,SS),Fn)).
  }

  public implementation all e ~~ display[e] |: display[cons[e]] => let{
    consDisp(.nil) => ss("").
    consDisp(cons(X,.nil)) => disp(X).
    consDisp(cons(X,R)) => ssSeq([disp(X), ss(","), consDisp(R)]).
  } in {
    disp(L) => ssSeq([ss("["), consDisp(L),ss("]")]).
  }

  public implementation functor[cons] => let{
    fm:all a,b ~~ ((a)=>b,cons[a])=>cons[b].
    fm(_,.nil) => .nil.
    fm(f,cons(H,T)) => cons(f(H),fm(f,T))
  } in {.
    fmap = fm.
    C <$ L => fm((_)=>C,L).
  .}

  public implementation monad[cons] => {
    (return X) => cons(X,.nil).
    (XS >>= F) => multicat(fmap(F,XS)).
  }
}
