star.core {
  @"Core definitions of types and interfaces that are really part of the language".

  equality@"defines functions associated with semantic equality".
  public contract all x ~~ equality[x] ::= {
    (==)@"semantic equality is defined explicitly".

    (==): (x,x)=>boolean.

    hash@"hash is an essential part of semantic equality".
    hash:(x)=>integer.
  }

  (\==)@"semantic inequality defined in terms of equality".
  public (\==):all x ~~ equality[x] |: (x,x)=>boolean.
  x \== y => \+ x==y.

  public contract all x ~~ comp[x] ::= {
    (<): (x,x)=>boolean.
    (>=): (x,x)=>boolean.
  }

  option@"the option type is useful when a value is not always available".
  public all t ~~ option[t] ::= none | some(t).

  public contract all c ~~ sizeable[c] ::= {
    size:(c) => integer.
    isEmpty:(c) => boolean.
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

  public implementation display[ss] => {
    disp(X) => X
  }


  -- Not strictly necessary, but makes for better symmetry.
  public boolean ::= true | false.

  -- We need the cons list in the core
  public all t ~~ cons[t] ::= nil | cons(t,cons[t]).

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

  private optionEqual:all x ~~ equality[x] |: (option[x],option[x]) => boolean.
  optionEqual(some(A),some(B)) => A==B.
  optionEqual(none,none) => true.
  optionEqual(_,_) => false.

  private optionHash:all x ~~ equality[x] |: (option[x]) => integer.
  optionHash(some(X)) => hash("some")*37+hash(X).
  optionHash(none) => hash("none").

  public maybe:all x ~~ ((x)=>boolean) => option[x].
  maybe(P) && P(x) => some(x).
  maybe(_) => none.

  -- Some basic stuff for tuples
  public implementation display[()] => {
    disp(_) => ss("()").
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

  pairEquals:all x,y ~~ equality[x], equality[y] |: ((x,y),(x,y))=>boolean.
  pairEquals((A1,A2),(B1,B2)) => A1==B1 && A2==B2.

  pairHash:all x,y ~~ equality[x],equality[y] |: ((x,y))=>integer.
  pairHash((A,B)) => hash(A)*37+hash(B).
}
