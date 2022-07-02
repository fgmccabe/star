test.cnb{
  import star.
  import star.script.

  public factU:(integer)=>integer.
  factU(N) => valof{
    I .= ref 1;
    R .= ref 1;

    do{
      R := R!*I!;
      I := I!+1
    } until I!>N;

    valis R!
  }

  public factF:(integer)=>integer.
  factF(N) => (try valof factFF(1,N,1) catch { _ => -1}).

  factFF:(integer,integer,integer)=>result[(),integer].
  factFF(F,T,S) => do{
    if F>T then
      valis S
    else
    valis valof factFF(F+1,T,S*F)
  }
  
  main:()=>().
  main() => valof{
    show factU(6);
    show factF(6);
    valis ()
  }
}
  
