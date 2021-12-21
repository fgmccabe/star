test.co1{
  import star.core.

  public implementation all x ~~ equality[x] |: equality[cons[x]] => let{
    smList:all x ~~ equality[x] |: (cons[x],cons[x]) => boolean.
    smList(.nil,.nil) => .true.
    smList(cons(x,xr),cons(y,yr)) => x==y && smList(xr,yr).
    smList(_,_) default => .false.
  } in {
    L1 == L2 => smList(L1,L2).
  }
}  
