star.sets{
  import star.core.
  import star.coerce.
  import star.collection.
  import star.ideal.
  import star.iterable.
  import star.cons.

  public all e ~~ set[e] ::= set(map[e,()]).

  public implementation all e ~~ equality[e],hash[e] |: sequence[set[e]->>e] => {.
    _nil = set([]).

    _cons(E,set(M)) => set(M[E->()]).
  .}

  public implementation all e ~~ equality[e], hash[e] |: membership[set[e] ->> e] => {
    set(M)\+e => set(M[e->()]).
    set(M)\-e => set(M[~e]).
    e.<.set(M) => some(()).=_index(M,e).
  }

  public implementation all e ~~ equality[e], hash[e] |: setops[set[e]] => {
    set(m1)\/set(m2) => set(ixLeft(((k,_,mm) => mm[k->()]),m2,m1)).
    set(m1)/\set(m2) => set(ixLeft((k,_,mm) => (some(_).=m2[k]?mm[k->()]||mm),[],m1)).
    set(m1)\set(m2) => set(ixLeft((k,_,mm) => (some(_).=m2[k]?mm||mm[k->()]),[],m1)).
  }

  public implementation all e ~~ display[e] |: display[set[e]] => let{
    dispEntry:(e,(),cons[ss])=>cons[ss].
    dispEntry(K,_,[]) => [disp(K)].
    dispEntry(K,_,L) default => [disp(K),ss(","),..L].
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

  public implementation all e ~~ folding[set[e]->>e] => {
    foldRight(F,A,set(S)) => ixRight((K,_,X) => F(K,X),A,S).
    foldLeft(F,A,set(S)) => ixLeft((K,_,X)=>F(K,X),A,S).
  }

  public implementation all e ~~ coercion[set[e],cons[e]] => {
    _coerce(set(M)) => some(ixRight((K,_,X)=>[K,..X],[],M)).
  }
}
