star.sets{
  import star.core.
  import star.collection.
  import star.ideal.
  import star.iterable.
  import star.lists.

  public all e ~~ set[e] ::= set(map[e,()]).

  public implementation all e ~~ equality[e],hash[e] |: sequence[set[e]->>e] => {.
    _nil = set([]).

    _cons(E,set(M)) => set(M[E->()]).
    _apnd(set(M),E) => set(M[E->()]).
  .}

  public implementation all e ~~ equality[e], hash[e] |: membership[set[e] ->> e] => {
    empty = _nil.
    _addMem(e,s) => _cons(e,s).
    _delMem(e,set(M)) => set(M[\+e]).
    _contains(set(M),e) => some(()).=_index(M,e).
  }

  public implementation all e ~~ equality[e], hash[e] |: setops[set[e]] => {
    set(m1)\/set(m2) => set(ixLeft(((mm,k,_) => mm[k->()]),m2,m1)).
    set(m1)/\set(m2) => set(ixLeft((mm,k,_) => (some(_).=m2[k]?mm[k->()]||mm),[],m1)).
    set(m1)\set(m2) => set(ixLeft((mm,k,_) => (some(_).=m2[k]?mm||mm[k->()]),[],m1)).
  }

  public implementation all e ~~ display[e] |: display[set[e]] => let{
    dispEntry([],K,_) => [disp(K)].
    dispEntry(L,K,_) default => [disp(K),ss(","),..L].
  } in {.
    disp(set(M)) => ssSeq([ss("{"),ssSeq(ixLeft(dispEntry,[],M)),ss("}")]).
  .}

  public implementation all e ~~ sizeable[set[e]] => {.
    size(set(M)) => size(M).
    isEmpty(set(M)) => isEmpty(M).
  .}

  -- This is the core of the query semantics
  public implementation all e ~~ equality[e],hash[e] |: iter[set[e]->>e] => {.
    _iter(set(S),St,F) => _iter(S,St,(K->_,X)=>F(K,X))
  .}
}
