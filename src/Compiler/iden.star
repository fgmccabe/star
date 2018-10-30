star.compiler.iden{
  import star.
  import star.pkg.

  public iden ::= iden(pkg,string).

  public implementation equality[iden] => {.
    iden(P1,I1) == iden(P2,I2) => P1==P2 && I1==I2.
  .}

  public implementation display[iden] => {.
    disp(iden(P,I)) => ssSeq([disp(P),ss("#"),ss(I)]).
  .}
}
