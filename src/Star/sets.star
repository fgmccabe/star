star.sets{
  import star.core.
  import star.collection.
  import star.ideal.
  import star.iterable.
  import star.lists.

  public all e ~~ set[e] ::= set(map[e,()]).

  public implementation all e ~~ equality[e],hash[e] |: sequence[set[e]->>e] => {
    _nil = set([]).

    _cons(E,set(M)) => set(M[E->()]).
    _apnd(set(M),E) => set(M[E->()]).
  }

  public implementation all e ~~ equality[e], hash[e] |: membership[set[e] ->> e] => {
    empty = _nil.
    _addMem(e,s) => _cons(e,s).
    _delMem(e,set(M)) => set(M[\+e]).
    _contains(set(M),e) => some(()).=_index(M,e).
  }

  public implementation all e ~~ equality[e], hash[e] |: setops[set[e]] => {
    _union(set(m1),set(m2)) => set(ixLeft(((mm,k,_) => mm[k->()]),m2,m1)).
    _intersect(set(m1),set(m2)) => set(ixLeft((mm,k,_) => (some(_).=m2[k]?mm[k->()]||mm),[],m1)).
    _difference(set(m1),set(m2)) => set(ixLeft((mm,k,_) => (some(_).=m2[k]?mm||mm[k->()]),[],m1)).
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
  public implementation all e ~~ equality[e],hash[e] |: iterable[set[e]->>e] => {.
    _iterate(set(M),F,St) => _ixiterate(M,(K,_,S)=>F(K,S),St).
  .}

  public implementation all t ~~ equality[t],hash[t] |: generator[set[t]->>t] => {.
    _generate(E,continueWith(S)) => continueWith(_cons(E,S)).
    _generate(E,noneFound) => continueWith(_cons(E,empty)).
    _generate(_,St) default => St.
  .}
}
