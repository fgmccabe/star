test.fbres{
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
    Fb = mkFb([1,2,3]);
    try{
      while .true do{
	X = Fb resume ();
	show X
      }
    } catch {
      M do showMsg("caught $(M)")
    }
  }
}
