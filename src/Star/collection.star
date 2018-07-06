star.collection{
  import star.core.
  import star.option.
  import star.lists.
  import star.arith.

  public contract all c,e ~~ folding[c->>e] ::= {
    foldRight:all x ~~ (((e,x)=>x),x,c) => x.
    foldLeft:all x ~~ (((x,e)=>x),x,c) => x.
  }

  public contract all t,e ~~ reduce[t->>e] ::= {
    reducer:all a ~~ ((e,a)=>a) => (t,a) => a.
    reducel:all a ~~ ((a,e)=>a) => (a,t) => a.
  }

  public contract all c,e ~~ filter[c->>e] ::= {
    (^/):(c,(e)=>boolean) => c.
  }

  public contract all m/1 ~~ mapping[m] ::= {
    (//):all e,f ~~ (m[e],(e)=>f) => m[f].
  }

  public contract all m/2 ~~ ixmap[m] ::= {
    (///):all k,e,f ~~ (m[k,e],(k,e)=>f)=>m[k,f].
  }

  public contract all k,v,m/2 ~~ ixfilter[m->>k,v] ::= {
    (^//):(m[k,v],(k,v)=>boolean) => m[k,v].
  }

  public contract all m,k,v ~~ indexed[m ->> k,v] ::= {
    _index:(m,k) => option[v].
    _insert:(m,k,v) => m.
    _remove:(m,k) => m.
    _replace:(m,k,v) => m.
  }.

  public contract all k,e ~~ membership[k->>e] ::= {
    empty: k.
    _addMem:(e,k)=>k.
    _delMem:(e,k)=>k.
    _contains:(k,e)=>boolean.
  }

  public contract all k ~~ setops[k] ::= {
    _union : (k,k)=>k.
    _intersect : (k,k)=>k.
    _difference : (k,k)=>k.
  }

  public implementation mapping[list] => {
    L//F => mapOverList(L,F,0,_list_size(L)).

    mapOverList:all e,f ~~ (list[e],(e)=>f,integer,integer)=>list[f].
    mapOverList(_,_,Lx,Lx)=>_list_nil(Lx).
    mapOverList(L,F,Ix,Lx) => _list_prepend(mapOverList(L,F,Ix+1,Lx),F(_list_nth(L,Ix))).
  }

  public implementation reduce[list] => {
    reducer(F) => let{
      rdr([],z) => z.
      rdr([x,..l],z) => F(x,rdr(l,z)).
    } in rdr.
    reducel(F) => let{
      rdl(z,[]) => z.
      rdl(z,[x,..l]) => F(rdl(z,l),x).
    } in rdl.
  }

  public implementation all e ~~ folding[list[e]->>e] => {
    foldRight(F,Z,L) => let{
      Mx = size(L).
      fdr(Ix) where Ix>=Mx => Z.
      fdr(Ix) => F(_list_nth(L,Ix),fdr(Ix+1)).
    } in fdr(0).
    foldLeft(F,Z,L) => let{
      Mx = size(L).
      fdl(Ix,I) where Ix>=Mx => I.
      fdl(Ix,I) => fdl(Ix+1,F(I,_list_nth(L,Ix))).
    } in fdl(0,Z).
  }

  iota:(integer,integer)=>list[integer].
  iota(Mx,Mx) => [].
  iota(Ix,Mx) where Ix<Mx => [Ix,..iota(Ix+1,Mx)].

  listPairs:all e ~~ (list[e],integer,integer)=>list[(integer,e)].
  listPairs(_,Mx,Mx) => [].
  listPairs(M,Ix,Mx) where Ix<Mx => [(Ix,_list_nth(M,Ix)),..listPairs(M,Ix+1,Mx)].

  public implementation all e ~~ indexed[list[e] ->> integer,e] => {
    _index(L,ix) where ix>=0 && ix<_list_size(L) => some(_list_nth(L,ix)).
    _index(_,_) default => none.

    _insert(L,ix,v) => _list_insert(L,ix,v).

    _remove(L,ix) => _list_remove(L,ix).

    _replace(L,ix,v) => _list_replace(L,ix,v).
  }
}
