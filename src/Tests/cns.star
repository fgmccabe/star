test.cns{
  import star.

  remove:all e ~~ equality[e] |:(cons[e],e)=>cons[e].
  remove(.nil,_) => .nil.
  remove(cons(H,T),K) where H==K => T.
  remove(cons(H,T),K) => cons(H,remove(T,K)).
}
