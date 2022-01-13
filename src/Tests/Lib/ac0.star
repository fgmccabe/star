test.ac0{
  import star.
  
  public logM:all e,m/2 ~~ execution[m] |: (string)=>m[e,()].
  logM(M) => do{
    _ .= _logmsg(M);
    return ()
  }
}  
