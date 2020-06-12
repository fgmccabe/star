test.co2{
  import star.core.

  public implementation all x ~~ comp[x],equality[x] |: comp[cons[x]] => let{
    consLess(.nil,cons(_,_)) => .true.
    consLess(cons(H1,T1),cons(H2,T2)) where H1<H2 => .true.
    consLess(cons(H,T1),cons(H,T2)) => consLess(T1,T2).
    consLess(_,_) default => .false.

    consGe(L1,L2) => ~consLess(L2,L1).
  } in {. (<) = consLess. (>=) = consGe .}

}  
