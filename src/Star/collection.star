star.collection{
  import star.core.
  import star.option.

  public contract all c,e ~~ folding[c->>e] ::= {
    foldRight:all x ~~ (((e,x)=>x),x,c) => x.
    foldLeft:all x ~~ (((e,x)=>x),x,c) => x.
  }

  public contract all c,e ~~ filter[c->>e] ::= {
    (^/):(c,(e)=>boolean) => c.
  }

  public contract all m/1,e,f ~~ mapping[m->>e,f] ::= {
    (//):(m[e],(e)=>f) => m[f].
  }

  public contract all m/2 ~~ ixmap[m] ::= {
    (///):all k,e,f ~~ (m[k,e],(k,e)=>f)=>m[k,f].
  }

  public contract all k,v,m/2 ~~ ixfilter[m->>k,v] ::= {
    (^//):(m[k,v],(k,v)=>boolean) => m[k,v].
  }

  public contract all m,k,v ~~ map[m ->> k,v] ::= {
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

  public contract all k,e ~~ setops[k->>e] ::= {
    union : (k,k)=>k.
    intersect : (k,k)=>k.
    difference : (k,k)=>k.
  }
}
