star.compiler.ast{
  import star.
  import star.compiler.location.
  import star.compiler.operators.

  public ast ::=
      nme(locn,string)
    | lit(locn,literal)
    | tpl(locn,string,list[])
    | app(locn,ast,ast).

  public literal ::=
      intgr(integer)
    | flot(float)
    | strg(string).

  public implementation equality[ast] => {.
    nme(_,I1) == nme(_,I2) => I1==I2.
    lit(_,L1) == lit(_,L2) => L1==L2.
    tpl(_,K1,E1) == tpl(_,K2,E2) => K1==K2 && E1==E2.
    app(_,O1,A1) == app(_,O2,A2) => O1==O2 && A1==A2.
    _ == _ default => false.
  .}

  public implementation equality[literal] => {.
    intgr(I1) == intgr(I2) => I1==I2.
    flot(F1) == flot(F2) => F1==F2.
    strg(S1) == strg(S2) => S1==S2.
    _ == _ default => false.
  .}

  public implementation display[ast] => {.
    disp(A) => dispAst(A,2000).
  .}

  public implementation display[literal] => {.
    disp(intgr(Ix)) => disp(Ix).
    disp(flot(Dx)) => disp(Dx).
    disp(strg(Sx)) => disp(Sx).
  .}

  public dispAst:(ast,integer) => ss.
  dispAst(lit(_,Lt),_) => disp(Lt).
  dispAst(nme(_,Id),_) => dispId(Id).
  dispAst(tpl(_,Bk,Els),_) where bkt(Lft,_,Rgt,Inn)^=isBracket(Bk) =>
    ssSeq([ss(Lft),ssSeq(Els//(E)=>dispAst(E,Inn)),ss(Rgt)]).


}
