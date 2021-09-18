test.cnb{
  import star.
  import star.script.

  public factU:(integer)=>integer.
  factU(N) => valof do{
    I .= ref 1;
    R .= ref 1;

    do{
      R := R!*I!;
      I := I!+1
    } until I!>N;

    valis R!
  }

  public factF:(integer)=>integer.
  factF(N) => valof factFF(1,N,1).

  factFF(F,T,S) => do{
    if F>T then
      valis S
    else
    factFF(F+1,T,S*F)
  }
  
  main:()=>action[(),()].
  main() => action{
    show factU(6);
    show factF(6)
  }
}
  
