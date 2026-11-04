test.fbdead{
  import star.
  import star.assert.

  mkFb:all e ~~ (cons[e]) => fiber[(),e,string].
  mkFb(Lst) => valof{
    F:((fiber[(),e,string],()) => e throws string);
    F = (this,_) => valof{
      LL := Lst;
      while .cons(X,Els).=LL! do{
	LL := Els;
	this suspend X
      };
      throw "done"
    };
    valis _fiber(F)
  }

  main:(){}.
  main(){
    Fb = mkFb([1]);
    try{
      X = Fb resume ();
      show X;
      Y = Fb resume ();  -- consume the final throw, fiber dies here
      show Y
    } catch {
      M do showMsg("caught $(M)")
    };
    -- resume again, after the fiber has already thrown and died
    try{
      Z = Fb resume ();
      show Z
    } catch {
      M do showMsg("caught again $(M)")
    }
  }
}
