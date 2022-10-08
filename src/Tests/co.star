test.co{
  import star.
  import star.script.
  
  cns[x] ::= .nl | .cns(x,cns[x]).

  public implementation all x ~~ sequence[cns[x] ->> x] => {
    _cons(E,S) => .cns(E,S).
    _nil = .nl.
  }

  implementation all x ~~ stream[cns[x] ->> x] => {
    _eof(.nl) => .true.
    _eof(.cns(_,_)) => .false.
    
    _hdtl(.cns(H,T)) => .some((H,T)).
    _hdtl(.nl) => .none.
  }

  person ::= someOne{
    name:string.
    age:(integer)=>integer
  }

  Pete  = someOne{
    name = "peter".
    age(X) => X+30
  }

  lookIn:all e ~~ ((cns[e],string,string)) => e.
  lookIn(L) => trpl(L).
  
  trpl(([A,.._],_,_)) => A.

  main:()=>().
  main()=>valof{
    assert Pete.name=="peter";
    assert Pete.age(3)==33;
    valis ()
  }
}
