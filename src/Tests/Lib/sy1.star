test.sy1{
--  Basic tests that do not use core
  import test.sy0.

  tr[e] ::= .e | nd{
    left:tr[e].
    lbl:e.
    right:tr[e]
  }.

  implementation lS[integer] => {.
    large=10.
    small=-10.
  .}

  nameOf:all x,y ~~ x<~{name:y} |: (x)=>y.
  nameOf(X)=>X.name.

  personName:(person)=>string.
  personName(P)=>nameOf(P).

  name:(person)=>option[string].
  name(.noone) => .none.
  name(someone{name=N}) => some(N).

  nme:(person)=>string.
  nme(someone{name=N}) => N.

  sp:(person)=>ref option[person].
  sp(P)=>P.spouse.

  largest:all x ~~ lS[x] |: ()=>x.
  largest() => large.

  public contract all x ~~ cmp[x] ::= {
    less:(x,x)=>boolean.
  }

  public find:all x~~cmp[x] |: (tree[x],x)=>boolean.
  find(.empty,_) => .false.
  find(node(L,X,R),K) =>
    less(X,K) ? find(L,K) ||
    less(K,X) ? find(R,K) ||
    .true.

  mkSingle:all a ~~ (a)=>tr[a].
  mkSingle(A)=>nd{. left=.e. lbl=A. right=.e .}.
}
