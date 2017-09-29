star.core {
  @"Core definitions of types and interfaces that are really part of the language".

  equality@"defines functions associated with semantic equality".

  public contract all x ~~ equality[x] ::= {
    (==)@"semantic equality is defined explicitly".

    (==): (x,x)=>logical.

    hash@"hash is an essential part of semantic equality".
    hash:(x)=>integer.
  }

  (\==)@"semantic inequality defined in terms of equality".
  public (\==):all x ~~ equality[x] |: (x,x)=>logical.
  x \== y => \+ x==y.

  public contract all x ~~ equality[x] |: comp[x] ::= {
    (<): (x,x)=>logical.
    (>=): (x,x)=>logical.
  }

  option@"the option type is useful when a value is not always available".
  public all t ~~ option[t] ::= none | some(t).

  public contract all x ~~ additive[x] ::= {
    (+): (x,x)=>x.
    (-): (x,x)=>x.
    zero: x.
  }

  public contract all x ~~ additive[x] |: arith[x] ::= {
    (*): (x,x)=>x.
    (/): (x,x)=>x.
    (%): (x,x)=>x.
    one:x.
  }

  public contract all c ~~ sizeable[c] ::= {
    size:(c) => integer.
    isEmpty:(c)=>logical.
  }

  -- Structured string.
  public ss ::= ss(string) | sc(integer) | ssSeq(list[ss]).

  -- Displayable contract
  public contract all t ~~ display[t] ::= {
    disp:(t)=>ss.
  }

  -- Formatting contract
  public contract all t ~~ format[t] ::= {
    frmt:(t,string) => ss.
  }

  -- _dump function contract is intended to show the structure of its argument
  public contract all t ~~ dump[t] ::= {
    _dump:(t,integer)=>ss.
  }

  -- The dump function only displays down to a given depth
  public dump:all a ~~ dump[a] |: (a,integer) => ss.
  dump(_,Lvl) && Lvl=<0 => ss("...").
  dump(T,Lvl) => _dump(T,Lvl).

  public implementation display[ss] => {
    disp(X) => X
  }

  -- stream contract
  public contract all S,E ~~ stream[S ->> E] ::= {
    _eof:(S) => option[()].
    _hdtl:(S)=> option[(S,E)].
  }

  -- This type alias maps pattern types to regular function return optional
  all a,b ~~ a<=b ~> b=>option[a].

  -- implement standard contracts for integers

  public implementation additive[integer] => {
    X+Y => _int_plus(X,Y).
    X-Y => _int_minus(X,Y).
    zero = 0.
  }

  public implementation arith[integer] => {
    X*Y => _int_times(X,Y).
    X/Y => _int_div(X,Y).
    X%Y => _int_mod(X,Y).
    one = 1.
  }

  public implementation equality[integer] => {
    X == Y => __int_equal(X,Y).
    hash(X) => X.
  }

  public implementation comp[integer] => {
    X<Y => _int_lt(X,Y).
    X>=Y => _int_ge(X,Y).
  }

  public implementation display[integer] => {
    disp(X) => ss(_int2str(X,10,0,0c )).
  }

  public implementation format[integer] => {
    frmt(X,F) => ss(_int_format(X,F)).
  }

  public implementation dump[integer] => {
    _dump(Ix,_) => ss(_int2str(Ix,10,0,0c ))
  }

  -- implement standard contracts for floats
  public implementation additive[float] => {
    X+Y => _flt_plus(X,Y).
    X-Y => _flt_minus(X,Y).

    zero = 0.0.
  }

  public implementation arith[float] => {
    X*Y => _flt_times(X,Y).
    X/Y => _flt_div(X,Y).
    X%Y => _flt_mod(X,Y).

    one = 1.0.
  }

  public implementation equality[float] => {
    X == Y => __flt_eq(X,Y).
    hash(X) => _flt_hash(X).
  }

  public implementation comp[float] => {
    X<Y => _flt_lt(X,Y).
    X>=Y => _flt_ge(X,Y).
  }

  public (>) : all t ~~ comp[t] |: (t,t)=>logical.
  X > Y => Y<X.

  public (=<): all t ~~ comp[t] |: (t,t)=>logical.
  X =< Y => Y>=X.

  public min: all t ~~ comp[t] |: (t,t)=>t.
  min(X,Y) && X<Y => X.
  min(_,Y) => Y.

  public max: all t ~~ comp[t] |: (t,t)=>t.
  max(X,Y) && X>Y => X.
  max(_,Y) => Y.

  public implementation display[float] => {
    disp(X) => ss(_flt2str(X,0,8,0cg,false)).
  }

  public implementation format[float] => {
    frmt(X,F) => ss(_flt_format(X,F)).
  }

  public implementation dump[float] => {
    _dump(X,_) => ss(_flt2str(X,0,8,0cg,false))
  }

  -- and strings ...
  public implementation additive[string] => {
    X+Y => _str_concat(X,Y).
    X-Y => "".

    zero = "".
  }

  public implementation equality[string] => {
    X == Y => _str_eq(X,Y).
    hash(X) => _str_hash(X).
  }

  public implementation comp[string] => {
    X<Y => _str_lt(X,Y).
    X>=Y => _str_ge(X,Y).
  }

  public implementation display[string] => {
    disp(X) => displayString(X).
  }

  public displayString:(string) => ss.
  displayString(S) => ssSeq([sc(0c\"),ssSeq(quoteStr(explode(S))),sc(0c\")]).

  private quoteStr:(list[integer]) => list[ss].
  quoteStr([])=>[].
  quoteStr([c,..l]) => qtChr(c,quoteStr(l)).

  private qtChr:(integer,list[ss]) => list[ss].
  qtChr(0c",l) => [sc(0c\\),sc(0c"),..l].
  qtChr(0c\\,l) => [sc(0c\\),sc(0c\\),..l].
  qtChr(c,l) => [sc(c),..l].

  public implementation dump[string] => {
    _dump(X,_) => displayString(X).
  }

  public implementation sizeable[string] => {
    size(S) => _str_len(S).
    isEmpty("").
  }

  -- Not strictly necessary, but makes for better symmetry.
  public logical ::= true | false.

  -- We need the list in the core
  public all t ~~ list[t] ::= '[]' | ',..'(t,list[t]).

  -- Implement equality for lists
  public implementation all x ~~ equality[x] |: equality[list[x]] => {
    L1 == L2 => listEq(L1,L2).
    hash(L) => listHash(L).
  }

  private
  listEq:all x ~~ equality[x] |: (list[x],list[x])=>logical.
  listEq([],[]) => true.
  listEq([E1,..L1],[E2,..L2]) && E1==E2 => listEq(L1,L2).
  listEq(_,_) => false.

  private
  listHash:all x ~~ equality[x] |: (list[x])=>integer.
  listHash([]) => 0.
  listHash([E,..L]) => hash(E)*37+listHash(L).

  -- stream contract
  public implementation all x ~~ stream[list[x] ->> x] => {
    _eof([]) => some(()).
    _eof(_) => none.
    _hdtl([E,..L]) => some((E,L)).
    _hdtl(_) => none.
  }

  -- display contract for lists
  public implementation all x ~~ display[x] |: display[list[x]] => {
    disp(L) => ssSeq([ss("["),..listDisp(L,"")]).
  }

  private
  listDisp:all x ~~ display[x] |: (list[x],string) => list[ss].
  listDisp([],_) => [ss("]")].
  listDisp([E,..L],Sep) => [ss(Sep),disp(E),..listDisp(L,",")].

  public dispVar:all x ~~ (x)=>ss.
  dispVar(X) => ss(_stringOf(X,0,0)).

  public implementation all a ~~ dump[a] |: dump[list[a]] => {
    _dump(T,Lvl) => ssSeq([ss("["),ssSeq(dumpList(T,"",Lvl-1)),ss("]")]).
  }

  dumpList:all a ~~ dump[a] |: (list[a],string,integer) => list[ss].
  dumpList([],_,_) => [].
  dumpList([A,..L],Sep,Lvl) => [ss(Sep),dump(A,Lvl),..dumpList(L,", ",Lvl)].

  -- display optional values
  public implementation all x ~~ display[x] |: display[option[x]] => {
    disp(O) => dispOptional(O).
  }

  private dispOptional:all x ~~ display[x] |: (option[x]) => ss.
  dispOptional(none) => ss("none").
  dispOptional(some(X)) => ssSeq([ss("some("),disp(X),ss(")")]).

  public implementation all x ~~ equality[x] |: equality[option[x]] => {
    X == Y => optionEqual(X,Y).
    hash(X) => optionHash(X).
  }

  private optionEqual:all x ~~ equality[x] |: (option[x],option[x]) => logical.
  optionEqual(some(A),some(B)) => A==B.
  optionEqual(none,none) => true.
  optionEqual(_,_) => false.

  private optionHash:all x ~~ equality[x] |: (option[x]) => integer.
  optionHash(some(X)) => hash("some")*37+hash(X).
  optionHash(none) => hash("none").

  public maybe:all x ~~ ((x)=>logical) => option[x].
  maybe(P) && P(x) => some(x).
  maybe(_) => none.

  -- Some basic stuff for tuples
  public implementation display[()] => {
    disp(_) => ss("()").
  }

  public implementation dump[()] => {
    _dump(_,_) => ss("()").
  }

  public implementation equality[()] => {
    () == () => true.
    hash(()) => 0.
  }

  -- Display 2-tuples
  public implementation all x,y ~~ display[x], display[y] |: display[(x,y)] => {
    disp(T) => dispPair(T).
  }

  private dispPair:all x,y ~~ display[x], display[y] |: ((x,y)) => ss.
  dispPair((a,b)) => ssSeq([ss("("),disp(a),ss(" , "),disp(b),ss(")")]).

  public implementation all x,y ~~ equality[x], equality[y] |: equality[(x,y)] => {
    X == Y => pairEquals(X,Y).
    hash(X) => pairHash(X).
  }

  pairEquals:all x,y ~~ equality[x], equality[y] |: ((x,y),(x,y))=>logical.
  pairEquals((A1,A2),(B1,B2)) => A1==B1 && A2==B2.

  pairHash:all x,y ~~ equality[x],equality[y] |: ((x,y))=>integer.
  pairHash((A,B)) => hash(A)*37+hash(B).
}
