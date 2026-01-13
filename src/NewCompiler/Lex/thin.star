star.lex.thin{
  import star.

  -- Implement 'thin' sets: sets that are not completely enumerated

  public thin[e] ::= .thEmpty
  | .thUniverse
  | .thSingle(e)
  | .thDiff(thin[e],thin[e])
  | .thUnion(thin[e],thin[e])
  | .thSect(thin[e],thin[e]).

  addEl:all e ~~ equality[e] |= (thin[e],e)=>thin[e].
  addEl(Thn,e) where isEl(e,Thn) => Thn.
  addEl(Thn,e) => case Thn in {
    | .thUniverse => Thn
    | .thEmpty => .thSingle(e)
    | .thSingle(E) => .thUnion(Thn,.thSingle(e))
    | .thUnion(L,R) => .thUnion(addEl(L,e),R)
    | .thDiff(L,R) => simplify(.thDiff(addEl(L,e),dropEl(R,e)))
    | .thSect(L,R) => .thSect(addEl(L,e),addEl(R,e))
  }

  dropEl:all e ~~ equality[e] |= (thin[e],e)=>thin[e].
  dropEl(Thn,e) where ~isEl(e,Thn) => Thn.
  dropEl(Thn,e) => case Thn in {
    | .thSingle(E) => .thEmpty
    | .thUnion(L,R) => simplify(.thUnion(dropEl(L,e),dropEl(R,e)))
    | .thDiff(L,R) => .thDiff(L,addEl(R,e))
    | .thSect(L,R) => .thSect(dropEl(L,e),R)
    | .thUniverse => .thDiff(.thUniverse,.thSingle(e))
  }

  simplify:all e ~~ equality[e] |= (thin[e])=>thin[e].
  simplify(.thEmpty) => .thEmpty.
  simplify(.thSingle(e)) => .thSingle(e).
  simplify(.thUnion(.thEmpty,e)) => e.
  simplify(.thUnion(e,.thEmpty)) => e.
  simplify(.thUnion(.thUniverse,e)) => .thUniverse.
  simplify(.thUnion(e,.thUniverse)) => .thUniverse.
  simplify(.thUnion(L,R)) => .thUnion(simplify(L),simplify(R)).
  simplify(.thSect(.thUniverse,R)) => R.
  simplify(.thSect(L,.thUniverse)) => L.
  simplify(.thSect(.thEmpty,_)) => .thEmpty.
  simplify(.thSect(_,.thEmpty)) => .thEmpty.
  simplify(.thSect(L,R)) => .thSect(simplify(L),simplify(R)).
  simplify(.thDiff(e,.thEmpty)) => e.
  simplify(.thDiff(_,.thUniverse)) => .thEmpty.
  simplify(.thDiff(L,R)) => simplifyDiff(simplify(L),simplify(R)).

  simplifyDiff(L,.thEmpty) => L.
  simplifyDiff(L,R) => .thDiff(L,R).

  isEl:all e ~~ equality[e] |= (e,thin[e])=>boolean.
  isEl(_,.thEmpty) => .false.
  isEl(e,.thSingle(E)) => e==E.
  isEl(e,.thUnion(L,R)) => isEl(e,L) || isEl(e,R).
  isEl(e,.thSect(L,R)) =>  isEl(e,L) && isEl(e,R).
  isEl(e,.thDiff(L,R)) => isEl(e,L) && ~isEl(e,R).

  public implementation all e ~~ equality[e] |= membership[thin[e] ->> e] => {
    S\+e => addEl(S,e).
    S\-e => dropEl(S,e).
    e.<.S => isEl(e,S).
  }

  enumerate:all e, x, X ~~ equality[e] |= ((e,x)=>x throws X,x,thin[e]) => x throws X.
  enumerate(F,X,.thEmpty) => X.
  enumerate(F,X,.thUniverse) => X.
  enumerate(F,X,.thSingle(e)) => F(e,X).
  enumerate(F,X,.thUnion(L,R)) => enumerate(F,enumerate(F,X,L),R).
  enumerate(F,X,.thUnion(L,R)) =>
    enumerate((e,x) => (isEl(e,L)??F(e,x)||x),enumerate(F,X,L),R).
  enumerate(F,X,.thSect(L,R)) =>
    enumerate((e,x) => (isEl(e,L)??F(e,x)||x),
    enumerate((e,x) => (isEl(e,R)??F(e,x)||x),X,L),R).
  enumerate(F,X,.thDiff(L,R)) =>
    enumerate((e,x) => (~isEl(e,R)??F(e,x)||x),X,L).
    
  public implementation all e ~~ equality[e] |= folding[thin[e]->>e] => {
    foldRight = enumerate.
    foldLeft = enumerate
  }

  dispThin:all e ~~ display[e] |= (thin[e]) => string.
  dispThin(.thEmpty) => "∅".
  dispThin(.thUniverse) => "U".
  dispThin(.thSingle(e)) => "{$(e)}".
  dispThin(.thUnion(l,r)) => "($(l)∪$(r))".
  dispThin(.thSect(l,r)) => "$(l)∩$(r)".
  dispThin(.thDiff(l,r)) => "($(l)∖$(r))".

  public implementation all e ~~ display[e] |= display[thin[e]] => {
    disp = dispThin
  }
}
  
  
