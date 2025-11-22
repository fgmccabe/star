test.a0{
  -- Nothing imported.

  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>_int_times(N,fact(_int_minus(N,1))).

  main:(){}.
  main(){
    _logmsg(_stringOf(fact(3),0))
  }
}
  
