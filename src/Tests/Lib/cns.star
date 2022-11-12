test.cns{
  import star.

  public zip: all e,f ~~ (cons[e],cons[f])=>cons[(e,f)].
  zip([],[]) => [].
  zip([E,..Es],[F,..Fs]) => [(E,F),..zip(Es,Fs)].

  -- public zp: all e,f ~~ (cons[e],cons[f])=>cons[(e,f)].
  -- zp(.nil,.nil) => .nil.
  -- zp(.cons(E,Es),.cons(F,Fs)) => .cons((E,F),zp(Es,Fs)).

}

