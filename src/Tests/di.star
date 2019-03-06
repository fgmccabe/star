test.di{
  import star.

  public contract all s,t,m/1,e ~~ iter[s,m->>t,e] ::= {
    _iter:all x ~~ (s,m[x],(t,x)=>m[x]) => m[x]
  }

  public implementation all t,e ~~ iter[cons[t],action[e]->>t,e] => {
    _iter(nil,St,_) => St.
    _iter(cons(H,T),St,Fn) => _sequence(St,(SS)=>_iter(T,Fn(H,SS),Fn)).
  }

  Is = cons(1,cons(2,cons(3,cons(4,nil)))).

  Px : action[(),integer].
  Px = _iter(Is,action{return 1},(Ix,Cx)=>action{return Ix*Cx}).

  show "Px=\(valof Px)".
}
