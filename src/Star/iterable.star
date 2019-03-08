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
  public all e,t ~~ iterState[e,t] ::= noneFound
                        | noMore(t)
                        | continueWith(t)
                        | abortIter(e).

  public implementation all e,t ~~ display[e],display[t] |: display[iterState[e,t]] => {.
    disp(noneFound) => ss("none found").
    disp(noMore(X)) => ssSeq([ss("only "),disp(X)]).
    disp(continueWith(X)) => ssSeq([ss("at least "),disp(X)]).
    disp(abortIter(E)) => ssSeq([ss("aborted: "),disp(E)]).
  .}

  public unwrapIter:all e,t ~~ (iterState[e,t]) => t.
  unwrapIter(noMore(X)) => X.
  unwrapIter(continueWith(X)) => X.

-- The iterable contract is used in planning queries
-- The iterate function takes a filter function and iterates over the collection using it while it returns a IterState state

  public contract all s,t ~~ iterable[s->>t] ::= {
    _iterate : all r,e ~~ (s,(t,iterState[e,r])=>iterState[e,r],iterState[e,r]) => iterState[e,r].
  }



  public contract all s,t ~~ generator[s->>t] ::= {
    _generate : all e ~~ (t,iterState[e,s]) => iterState[e,s].
  }

  -- The iter contract is used in query evaluation
  -- The _iter function takes an execution and iterates over the collection composing it

  public contract all s,t ~~ iter[s->>t] ::= {
    _iter:all x,m/1,e ~~ execution[m->>e] |: (s,m[x],(t,x)=>m[x]) => m[x]
  }

  public contract all s,k,v ~~ indexed_iter[s->>k,v] ::= {
    _ix_iter:all x,m/1,e ~~ execution[m->>e] |: (s,m[x],(k,v,x)=>m[x]) => m[x]
  }

  public contract all s,k,v ~~ indexed_iterable[s ->> k,v] ::= {
    _ixiterate : all e,r ~~ (s,(k,v,iterState[e,r])=>iterState[e,r],iterState[e,r]) => iterState[e,r]
  }

  public iterState2option:all e,t ~~ (iterState[e,t]) => option[t].
  iterState2option(noMore(X)) => some(X).
  iterState2option(continueWith(X)) => some(X).
  iterState2option(noneFound) => none.
  iterState2option(abortIter(_)) => none.

  public implementation all e,t ~~ coercion[iterState[e,t],option[t]] => {
    _coerce(I) => iterState2option(I).
  }

  public _checkIterState: all e,t ~~ (iterState[e,t],()=>t) => t.
  _checkIterState(noMore(X),_) => X.
  _checkIterState(continueWith(X),_) => X.
  _checkIterState(noneFound,D) => D().

  public _negate:all x,e ~~ (iterState[e,boolean],()=>x,()=>x) => x.
  _negate(noMore(true),A,_) => A().
  _negate(continueWith(true),A,_) => A().
  _negate(_,_,B) default => B().

  public _otherwise:all e,t ~~ (iterState[e,t],()=>iterState[e,t]) => iterState[e,t].
  _otherwise(noneFound,F) => F().
  _otherwise(St,_) => St.

  public contract all coll/1, m/2, k,v ~~ grouping[coll ->> m,k,v] ::=  {
    (group_by) : (coll[v], (v)=>k) => m[k,coll[v]]
  }

  public implementation all e ~~ monad[iterState[e]] => {
    return X => continueWith(X).

    (noneFound >>= _) => noneFound.
    (continueWith(X) >>= F) => F(X).
    (noMore(X) >>= F) => F(X).
    (abortIter(E) >>= _) => abortIter(E).
  }

  public implementation all e ~~ execution[iterState[e]->>e] => {.
    _perform(noMore(X)) => X.
    _perform(continueWith(X)) => X.

    _handle(abortIter(x),E) => E(x).
    _handle(V,_) default => V.

    _sequence(noneFound,_) => noneFound.
    _sequence(continueWith(X),F) => F(X).
    _sequence(noMore(X),F) => F(X).
    _sequence(abortIter(E),_) => abortIter(E).

    _return(x) => continueWith(x).
    _raise(x) => abortIter(x).
  .}

  public implementation all e ~~ functor[iterState[e]] => {
    fmap(F,noMore(X)) => noMore(F(X)).
    fmap(F,continueWith(X)) => continueWith(F(X)).
    fmap(_,abortIter(E)) => abortIter(E).
    fmap(_,noneFound) => noneFound.
  }

  public implementation all e ~~ iterable[list[e]->>e] => {
    _iterate(Lst,Fn,Init) => iterateOverList(Lst,0,size(Lst),Fn,Init).

    iterateOverList(_,Ix,Mx,_,St) where Ix>=Mx => St.
    iterateOverList(_,_,_,_,noMore(X)) => noMore(X).
    iterateOverList(_,_,_,_,abortIter(E)) => abortIter(E).
    iterateOverList(Lst,Ix,Mx,Fn,St) where El^=Lst[Ix] => iterateOverList(Lst,Ix+1,Mx,Fn,Fn(El,St)).
  }

  public implementation all e ~~ generator[list[e]->>e] => {
    _generate(E,continueWith(L)) => continueWith([E,..L]).
    _generate(E,noneFound) => continueWith([E]).
    _generate(_,St) default => St.
  }
}
