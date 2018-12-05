star.iterable{
  import star.core.
  import star.monad.
  import star.option.
  import star.coerce.
  import star.collection.
  import star.arith.
  import star.lists.

  -- iterstate is similar to option except that is has more options (sic)
  -- Used in query processing
  public iterState[t] ::= noneFound
                        | noMore(t)
                        | continueWith(t)
                        | abortIter(string).

  public implementation all e ~~ display[e] |: display[iterState[e]] => {.
    disp(noneFound) => ss("none found").
    disp(noMore(X)) => ssSeq([ss("only "),disp(X)]).
    disp(continueWith(X)) => ssSeq([ss("at least "),disp(X)]).
    disp(abortIter(E)) => ssSeq([ss("aborted: "),ss(E)]).
  .}

  public unwrapIter:all t ~~ (iterState[t]) => t.
  unwrapIter(noMore(X)) => X.
  unwrapIter(continueWith(X)) => X.

-- The iterable contract is used in planning queries
-- The iterate function takes a filter function and iterates over the collection using it while it returns a IterState state

  public contract all s,e ~~ iterable[s->>e] ::= {
    _iterate : all r ~~ (s,(e,iterState[r])=>iterState[r],iterState[r]) => iterState[r].
  }

  public contract all s,e ~~ generator[s->>e] ::= {
    _generate : (e,iterState[s]) => iterState[s].
  }

  public contract all s,k,v ~~ indexed_iterable[s ->> k,v] ::= {
    _ixiterate : all r ~~ (s,(k,v,iterState[r])=>iterState[r],iterState[r]) => iterState[r]
  }

  public iterState2option:all e ~~ (iterState[e]) => option[e].
  iterState2option(noMore(X)) => some(X).
  iterState2option(continueWith(X)) => some(X).
  iterState2option(noneFound) => none.
  iterState2option(abortIter(_)) => none.

  public implementation all e ~~ coercion[iterState[e],option[e]] => {
    _coerce(I) => iterState2option(I).
  }

  public _checkIterState: all e ~~ (iterState[e],()=>e) => e.
  _checkIterState(noMore(X),_) => X.
  _checkIterState(continueWith(X),_) => X.
  _checkIterState(noneFound,D) => D().

  public _negate:all x ~~ (iterState[boolean],()=>x,()=>x) => x.
  _negate(noMore(true),A,_) => A().
  _negate(continueWith(true),A,_) => A().
  _negate(_,_,B) default => B().

  public _otherwise:all e ~~ (iterState[e],()=>iterState[e]) => iterState[e].
  _otherwise(noneFound,F) => F().
  _otherwise(St,_) => St.

  public contract all coll/1, m/2, k,v ~~ grouping[coll ->> m,k,v] ::=  {
    (group_by) : (coll[v], (v)=>k) => m[k,coll[v]]
  }

  public implementation monad[iterState] => {
    return X => continueWith(X).

    (noneFound >>= _) => noneFound.
    (continueWith(X) >>= F) => F(X).
    (noMore(X) >>= F) => F(X).
    (abortIter(E) >>= _) => abortIter(E).
  }

  public implementation execution[iterState->>string] => {
    _perform(noMore(X)) => X.
    _perform(continueWith(X)) => X.

    _handle(abortIter(x),E) => E(x).
    _handle(V,_) default => V.

    _raise(x) => abortIter(x).
  }

  public implementation functor[iterState] => {
    fmap(F,noMore(X)) => noMore(F(X)).
    fmap(F,continueWith(X)) => continueWith(F(X)).
    fmap(_,abortIter(E)) => abortIter(E).
    fmap(_,noneFound) => noneFound.
  }

  public implementation all e ~~ iterable[list[e]->>e] => {
    _iterate(Lst,Fn,Init) => iterateOverList(Lst,0,size(Lst),Fn,Init).

    iterateOverList(_,Ix,Mx,_,St) where Ix>=Mx => St.
    iterateOverList(_,_,_,_,noMore(X)) => noMore(X).
    iterateOverList(Lst,Ix,Mx,Fn,St) where El^=Lst[Ix] => iterateOverList(Lst,Ix+1,Mx,Fn,Fn(El,St)).
  }

  public implementation all e ~~ generator[list[e]->>e] => {
    _generate(E,continueWith(L)) => continueWith([E,..L]).
    _generate(E,noneFound) => continueWith([E]).
    _generate(_,St) default => St.
  }
}
