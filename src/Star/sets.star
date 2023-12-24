star.sets{
  import star.core.
  import star.coerce.
  import star.index.
  import star.collection.
  import star.ideal.
  import star.iterable.
  import star.cons.
  import star.strings.
  import star.tuples.

  public all e ~~ set[e] ::= .set(map[e,()]).

  public implementation all e ~~ equality[e],hashable[e] |: sequence[set[e]->>e] => {
    _nil = .set([]).

    _cons(E,.set(M)) => .set(M[E->()]).
  }

  public implementation all e ~~ equality[e], hashable[e] |: membership[set[e] ->> e] => {
    .set(M)\+e => .set(M[e->()]).
    .set(M)\-e => .set(M[~e]).
    e.<. .set(M) => .some(()).=_index(M,e).
  }

  public implementation all e ~~ equality[e], hashable[e] |: setops[set[e]] => {
    .set(m1)\/.set(m2) => .set(ixLeft(((k,_,mm) => mm[k->()]),m2,m1)).
    .set(m1)/\.set(m2) => .set(ixLeft((k,_,mm) => (.some(_).=m2[k]??mm[k->()]||mm),[],m1)).
    .set(m1)\.set(m2) => .set(ixLeft((k,_,mm) => (.some(_).=m2[k]??mm||mm[k->()]),[],m1)).
  }

  public implementation all e ~~ equality[e], hashable[e] |: concat[set[e]] => {
    S1 ++ S2 => S1\/S2.
    _multicat(Sin) => let{.
      _multi([],Ss) => Ss.
      _multi([.set(S1),..Ss],Si) =>
        _multi(Ss,ixLeft(((k,_,mm) => mm[k->()]),Si,S1)).
    .} in .set(_multi(Sin,[]))
  }

  public implementation all e ~~ display[e] |: display[set[e]] => let{
    dispEntry:(e,(),cons[string])=>cons[string].
    dispEntry(K,_,[]) => [disp(K)].
    dispEntry(K,_,L) default => [disp(K),..L].
  } in {
    disp(.set(M)) => "{#(interleave(ixLeft(dispEntry,[],M),", ")*)}".
  }

  public implementation all e ~~ sizeable[set[e]] => {
    size(.set(M)) => size(M).
    isEmpty(.set(M)) => isEmpty(M).
  }

  -- This is the core of the query semantics
  public implementation all e ~~ iter[set[e]->>e] => {
    _iter(.set(S),St,F) => _iter(S,St,(K->_,X)=>F(K,X))
  }

  public implementation all e ~~ folding[set[e]->>e] => {
    foldRight(F,A,.set(S)) => ixRight((K,_,X) => F(K,X),A,S).
    foldLeft(F,A,.set(S)) => ixLeft((K,_,X)=>F(K,X),A,S).
  }

  public implementation all e ~~ coercion[set[e],cons[e]] => {
    _coerce(.set(M)) => .some(ixRight((K,_,X)=>[K,..X],[],M)).
  }

  public implementation all e ~~ generate[set[e]->>e] => {
    _generate(S) => iterGenerator(S)
  }

  public implementation all e,f ~~ equality[e],equality[f],
  hashable[e],hashable[f] |: mapping[set->>e,f] => {
    (.set(C)//F) => .set(ixRight((E,_,X)=>X[F(E)->()],[],C))
  }

  public implementation all e ~~ equality[e] |: membership[cons[e] ->> e] => let{.
    add_mem([],X) => [X].
    add_mem([X,..Xs],X) => [X,..Xs].
    add_mem([U,..Xs],X) => [U,..add_mem(Xs,X)].

    del_mem([],_) => [].
    del_mem([X,..Xs],X) => Xs.
    del_mem([U,..Xs],X) => [U,..del_mem(Xs,X)].

    is_mem(_,[]) => .false.
    is_mem(X,[X,.._]) => .true.
    is_mem(X,[_,..Xs]) => is_mem(X,Xs).
  .} in {
    S\+E => add_mem(S,E).
    S\-E => del_mem(S,E).
    E.<.S => is_mem(E,S).
  }
  
}
