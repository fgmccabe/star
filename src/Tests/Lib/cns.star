test.cns{
  import star.
  import star.assert.

  -- Public zip: all e,f ~~ (cons[e],cons[f])=>cons[(e,f)].
  -- zip([],[]) => [].
  -- zip([E,..Es],[F,..Fs]) => [(E,F),..zip(Es,Fs)].

  -- public zp: all e,f ~~ (cons[e],cons[f])=>cons[(e,f)].
  -- zp(.nil,.nil) => .nil.
  -- zp(.cons(E,Es),.cons(F,Fs)) => .cons((E,F),zp(Es,Fs)).

--  public strFind:(string,string,integer) => option[integer].
--  strFind(Txt,Ky,Ix) where Lc.= _str_find(Txt,Ky,Ix) && Lc>=0 => ?Lc.
--  strFind(_,_,_) default => .none.

  ne:all x ~~ equality[x] |: (x,x)=>boolean.
  ne(x,y) => ~x==y.
  
  main:()=>().
  main()=>valof{
    logMsg("hello");
    assert ne(3,2);
    valis ()
  }
}

