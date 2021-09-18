test.cns{
  import star.
  import star.script.

  remove:all e ~~ equality[e] |:(cons[e],e)=>cons[e].
  remove(.nil,_) => .nil.
  remove(cons(H,T),K) where H==K => T.
  remove(cons(H,T),K) => cons(H,remove(T,K)).

  main:()=>action[(),()].
  main()=>action{
    assert remove([1,2,3],2) == [1,3]
  }
}
