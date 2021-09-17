test.ac0{
  import star.
  
  public logM:(string)=>result[(),()].
  logM(M) => do{
    _ .= _logmsg(_str_fltn(M));
    return ()
  }
}  
