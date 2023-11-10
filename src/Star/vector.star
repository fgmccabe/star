star.vector{
  import star.core.
  import star.arith.
  import star.bits.
  import star.coerce.
  import star.collection.
  import star.index.
  import star.iterable.

  public all x ~~ vect[x] ::= .vector(integer,vct[x],vct[x]).
  
  all x ~~ vct[x] ::= .e | .s(x) | .s2(x,x) | .s3(x,x,x) |
  .vct(integer,vct[x],vct[x],vct[x],vct[x]).

  tail_offset:all x ~~ (vect[x]) => integer.
  tail_offset(.vector(Ln,_,Tail)) => case Tail in {
    .e => Lc
    | .s(_) => Ln-1
    | .s2(_,_) => Ln-2
    | .s3(_,_,_) => Ln-3
  }.

  lookup:all x ~~ (vct[x],integer,integer) => option[x].
  lookup(V,Ky,Sx) where Sx>=0 => 
    case (Ky.>>.Sx).&.3 in {
      0 => pick0(V,Ky,Sx-2)
      | 1 => pick1(V,Ky,Sx-2)
      | 2 => pick2(V,Ky,Sx-2)
      | 3 => pick3(V,Ky,Sx-2)
    }.


  pick0(.vct(_,
  lookup(.vct(_,V0,V1,V2,V3),Ky,Sx) where Sx>0 =>
    case  in {
      0 => lookup(V0,Ky,Sx-2)
    }.
  lookup(.vct(_,V0,V1,V2,V3),Ky,0) => case Ky.&.0x3 in {
    0 => inspect(V0)
    | 1 => inspect(V1)
    | 2 => inspect(V2)
    | 3 => inspect(V3)
  }.
  lookup(.s3(V0,V1,V2),Ky,Sx) where Sx>0 =>
    case (Ky.>>.Sx).&.3 in {
      0 => lookup(V0,Ky,Sx-2)
      | 1 => lookup(V1,Ky,Sx-2)
      | 2 => lookup(V2,Ky,Sx-2)
      | 3 => .none
    }.
  lookup(.s3(V0,V1,V2),Ky,0) => case Ky.&.0x3 in {
    0 => inspect(V0)
    | 1 => inspect(V1)
    | 2 => inspect(V2)
    | 3 => .none
  }.

  inspect(.e) => .none.
  inspect(.s(x)) => .some(x).

}
