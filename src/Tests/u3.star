test.u3{
  -- simple test of uniqueness algorithm

  split:(integer,cons[integer])=>integer.
  split(V,L) => case V in {
    | 0 => case L in {
      | .nil => 0
      | .cons(X,Y) => valof{
	(Xs,Tx) = left(Y);
	valis join(Xs,Tx)
      }
    }
    | 1 => case L in {
      | .nil => 1
      | .cons(X,Y) => valof{
	(Xs,Tx) = right(Y);
	valis join(Xs,Tx)
      }
    }
  }

  left(.cons(X,Y)) => (X,Y).

  right(.cons(X,Y)) => (X,Y).

  join(X,Y) => X.
}
      
