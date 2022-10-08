star.bintree{
  import star.core.
  import star.arith.
  import star.coerce.
  import star.collection.
  import star.index.
  import star.iterable.

  public bin[k,v] ::= private .e | private n(bin[k,v],k,v,integer,bin[k,v]).

  findInTree:all k,v ~~ equality[k], comp[k] |: (bin[k,v],k)=>option[v].
  findInTree(T,Ky) => case T in {
    .n(_,Ky,V,_,_) => .some(V).
    .n(L,K,_,_,_) where Ky<K => findInTree(L,Ky).
    .n(_,K,_,_,R) where Ky>K => findInTree(R,Ky).
    .e => .none.
  }

  depth(.e) => 0.
  depth(n(_,_,_,D,_)) => D.

  rotateRight:all k,v ~~ equality[k],comp[k] |: (bin[k,v])=>bin[k,v].
  rotateRight(n(n(L1,Kx,Vx,_,R1),Ky,Vy,_,R2)) where
      Dr .= max(depth(R1),depth(R2))+1 =>
    n(L1,Kx,Vx,max(depth(L1),Dr)+1,n(L2,Ky,Vy,Dr,R2)).

  rotateLeft:all k,v ~~ equality[k],comp[k] |: (bin[k,v])=>bin[k,v].
  rotateLeft(n(L1,Kx,Ky,_,n(L2,Ky,Vy,_,R2))) where
    Dl .= max(depth(L1),depth(L2))+1 =>
    n(n(L1,Kx,Vx,Dl,L2),Ky,Vy,max(Dl,depth(R2))+1,R2).

  insert:all k,v ~~ equality[k],comp[k] |: (bin[k,v],k,v)=>bin[k,v].
  insert(.e,K,V) => n(.e,K,V,1,.e).
  insert(n(L1,K,D,R1),K,V) => n(L1,K,V,D,R1).
  insert(n(L1,Kl,Vl,_,R1),K,V) where Kl>K => valof{
    LL = insert(L1,K,V);
    valis balance(n(LL,Kl,Vl,max(depth(LL),depth(R1))+1,R1))
  }
  insert(n(L1,Kl,Vl,_,R1),K,V) where Kl<K => valof{
    RR = insert(R1,K,V);
    valis balance(n(L1,Kl,Vl,max(depth(L1),depth(RR))+1,RR))
  }

  balanced ::= .even | .leftHeavy | .rightHeavy.

  balanced:all k,v ~~ (bin[k,v]) => balanced.
  balanced(.e) => .even.
  balanced(n(L,_,_,R)) where Dl.=depth(L) && Dr.=depth(R) =>
    (Dl>Dr+1 ?
      .leftHeavy ||
      (Dr>Dl+1 ?
	.rightHeavy ||
	.even)).

  balance(.e) => .e.
  balance(T) => case balanced(T) in {
    .even => T.
    .leftHeavy => shiftRight(T).
    .rightHeavy => shiftLeft(T).
  }
}  
  
  

  
