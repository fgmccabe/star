star.cons{
  import star.core.
  import star.arith.
  import star.iterable.
  import star.monad.
  import star.coerce.
  import star.strings.

  public implementation all x ~~ equality[x] |: equality[cons[x]] => let{.
    smList:all x ~~ equality[x] |: (cons[x],cons[x]) => boolean.
    smList(.nil,.nil) => .true.
    smList(.cons(x,xr),.cons(y,yr)) => x==y && smList(xr,yr).
    smList(_,_) default => .false.
  .} in {
    L1 == L2 => smList(L1,L2).
  }

  public implementation all x ~~ hashable[x] |: hashable[cons[x]] => {
    hash(L) => cHash(L,0).
  }

  cHash:all x ~~ hashable[x] |: (cons[x],integer) => integer.
  cHash(.nil,X) => X.
  cHash(.cons(x,xr),H) => cHash(xr,(H+hash(x))*37).

  public implementation all x ~~ comp[x],equality[x] |: comp[cons[x]] => let{.
    consLess(.nil,.cons(_,_)) => .true.
    consLess(.cons(H1,T1),.cons(H2,T2)) where H1<H2 => .true.
    consLess(.cons(H,T1),.cons(H,T2)) => consLess(T1,T2).
    consLess(_,_) default => .false.

    consGe(L1,L2) => ~consLess(L2,L1).
  .} in {
    (<) = consLess.
    (>=) = consGe
  }

  -- build, stream & sequence contracts
  public implementation all x ~~ build[cons[x] ->> x] => {
    _push(E,S) => .cons(E,S).
    _null = .nil.
  }

  public implementation all x ~~ stream[cons[x] ->> x] => {
    _eof(.nil) => .true.
    _eof(.cons(_,_)) => .false.
    
    _hdtl(.cons(H,T)) => .some((H,T)).
    _hdtl(.nil) => .none.
  }

  public implementation all x ~~ sequence[cons[x] ->> x] => {
    _cons(E,S) => .cons(E,S).
    _nil = .nil.
  }

  public implementation all e ~~ sizeable[cons[e]] => let{.
    consLength:all e ~~ (cons[e],integer) => integer.
    consLength(.nil,Ln) => Ln.
    consLength(.cons(_,T),Ln) => consLength(T,Ln+1).
 .} in {
    size(L) => consLength(L,0).
    isEmpty(.nil) => .true.
    isEmpty(_) default => .false.
  }

  public implementation all e ~~ measured[cons[e]->>integer] => {
    [|L|] => size(L)
  }

  last:all e ~~ (cons[e]) => (cons[e],e).
  last(.cons(X,.nil)) => (.nil,X).
  last(.cons(X,Y)) where (L,E) .= last(Y) => (.cons(X,L),E).

  public implementation all x ~~ concat[cons[x]] => {
    X++Y => concat(X,Y).
    _multicat(X) => multicat(X).
  }

  concat: all e ~~ (cons[e],cons[e])=>cons[e].
  concat(.nil,Y) => Y.
  concat(.cons(E,X),Y) => .cons(E,concat(X,Y)).

  multicat : all e ~~ (cons[cons[e]]) => cons[e].
  multicat(.nil) => .nil.
  multicat(.cons(H,T)) => concat(H,multicat(T)).

  public implementation all x ~~ reversible[cons[x]] => {.
    reverse(L) => rev(L,.nil).

    private rev:(cons[x],cons[x])=>cons[x].
    rev(.nil,R) => R.
    rev(.cons(E,L),R) => rev(L,.cons(E,R)).
 .}

  public implementation all x ~~ head[cons[x]->>x] => {
    head(.cons(E,_)) => .some(E).
    head(.nil) => .none.

    tail(.cons(_,T)) => .some(T).
    tail(.nil) => .none.
  }

  public front:all e ~~ (cons[e],integer)=>option[(cons[e],cons[e])].
  front(Els,Ln) => let{.
    ff(Es,So,0) => .some((reverse(So),Es)).
    ff([E,..Es],So,Ix) =>
      ff(Es,[E,..So],Ix-1).
    ff(_,_,_) default => .none.
 .} in ff(Els,[],Ln).

  public zip: all e,f ~~ (cons[e],cons[f])=>cons[(e,f)].
  zip([],[]) => [].
  zip([E,..Es],[F,..Fs]) => [(E,F),..zip(Es,Fs)].

  public unzip:all e,f ~~ (cons[(e,f)])=>(cons[e],cons[f]).
  unzip([]) => ([],[]).
  unzip([(A,B),..Ls]) where
      (L,R) .= unzip(Ls) => ([A,..L],[B,..R]).

  -- Implement iteration over a cons list
  public implementation all t ~~ iter[cons[t]->>t] => {.
    _iter(.nil,St,_) => St.
    _iter(.cons(H,T),St,Fn) => _iter(T,Fn(H,St),Fn).
  .}

  -- Implement a generator for cons lists
  public implementation all e ~~ generate[cons[e]->>e] => {
    _generate(L) => generator{
      LL = ref L;
      while .cons(H,T) .= LL! do{
	yield H;
	LL := T
      }
    }
  }
    
  public implementation all e ~~ display[e] |: display[cons[e]] => let{.
    consDisp(.nil,L) => L.
    consDisp(.cons(X,.nil),L) => .cons(disp(X), L).
    consDisp(.cons(X,R),L) => .cons(disp(X), .cons(",", consDisp(R,L))).
 .} in {
    disp(L) => _str_multicat(.cons("[",consDisp(L,.cons("]",.nil))))
  }

  -- Format a list
  -- syntax: $(L):pre[fmt,..sep]post;
  -- pre is displayed before the list,
  -- post is displayed after the list
  -- sep is displayed between each element
  -- fmt is used to format each element
  public implementation all e ~~ format[e] |: format[cons[e]] => let{.
    splitFormat(F) => valof{
      if Ix ?= strFind(F,"[",0) && Iz ?= strFind(F,"]",Ix) && Iy ?= strFind(F,",..",Ix) then{
	Pre = subString(F,0,Ix);
	Sep = subString(F,Iy+3,Iz-Iy-3);
	Post = subString(F,Iz+1,[|F|]);
	ElFmt = subString(F,Ix+1,Iy-Ix-1);
	valis .some((Pre,ElFmt,Sep,Post))
      }
      else{
	valis .none
      }
    }
    consFmt(.nil,_,_,L) => L.
    consFmt(.cons(X,.nil),Fmt,_,L) => .cons(_format(X,Fmt), L).
    consFmt(.cons(X,R),Fmt,Sep,L) => .cons(_format(X,Fmt), .cons(Sep, consFmt(R,Fmt,Sep,L))).
  .} in {
    _format(L,Fmt) => valof{
      if (Pre,ElF,Sep,Post)?=splitFormat(Fmt) then
	valis _str_multicat(.cons(Pre,consFmt(L,ElF,Sep,.cons(Post,.nil))))
      else{
	valis _str_multicat(.cons("[",consFmt(L,Fmt,", ",.cons("]",.nil))))
      }
    }
  }

  public implementation functor[cons] => let{.
    fm:all a,b ~~ ((a)=>b,cons[a])=>cons[b].
    fm(_,.nil) => .nil.
    fm(f,.cons(H,T)) => .cons(f(H),fm(f,T))
 .} in {
    fmap = fm.
    C <$ L => fm((_)=>C,L).
  }

  public implementation monad[cons] => {
    (return X) => .cons(X,.nil).
    (XS >>= F) => multicat(fmap(F,XS)).
  }
}
