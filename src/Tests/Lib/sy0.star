test.sy0{
  --  Basic tests without core

  -- Type definitions

  public all x ~~ foo[x] ~> tree[x].

  public option[x] ::= .none | some(x).

  public boolean ::= .true | .false.

  -- Tree type
  public tree[x] ::= .empty | node(foo[x],x,tree[x]).

  -- Person type
  public person ::= .noone
    | someone{
      name:string.
      spouse : ref option[person].
    }.

  nn:(person)=>option[string].
  nn(.noone) => .none.
  nn(someone{name=N}) => some(N).

  -- A small contract

  public contract all x ~~ lS[x] ::= {
    large : x.
    small : x.
  }

  public implementation lS[integer] => {.
    large=10.
    small=-10.
  .}

  largest:all x ~~ lS[x] |: ()=>x.
  largest() => large.

  contract all x ~~ ar[x] ::= {
    plus:(x,x)=>x.
  }

  double:all x ~~ ar[x] |: (x)=>x.
  double(X) => plus(X,X).

  quad:all  x ~~ ar[x] |: (x)=>x.
  quad(X) => double(double(X)).

  dbl:all x ~~ ((x)=>x)=>(x)=>x.
  dbl(F) => (X)=>F(F(X)).

  qd:all  x ~~ ar[x] |: (x)=>x.
  qd(X) => dbl(double)(X).

  implementation ar[integer] => {
    plus(x,y) => _int_plus(x,y).
  }

  dblInt:(integer)=>integer.
  dblInt(X) => plus(X,X).
}
