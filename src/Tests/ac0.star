test.ac0{
  public logM:(string)=>action[(),()].
  logM(M) => action{
    _ .= _logmsg(M);
    return ()
  }
}  
