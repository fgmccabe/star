star.collection{
  import star.core.
  import star.option.
  import star.lists.
  import star.arith.

  public contract all c/1 ~~ folding[c] ::= {
    foldRight:all x,e ~~ (((e,x)=>x),x,c[e]) => x.
    foldLeft:all x,e ~~ (((x,e)=>x),x,c[e]) => x.
  }

  public contract all t/1 ~~ reduce[t] ::= {
    reducer:all a,b ~~ ((a,b)=>b) => (t[a],b) => b.
    reducel:all a,b ~~ ((b,a)=>b) => (b,t[a]) => b.
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
    present:(m,k) => option[v].
    _remove:(m,k) => m.
    _put:(m,k,v) => m.
    keys:(m) => list[k].
    pairs:(m) => list[(k,v)].
    values:(m) => list[v].
    _empty:m.
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
}
