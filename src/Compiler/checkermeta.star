star.compiler.meta{
  import star.

  import star.compiler.ast.
  import star.compiler.location.

  public importSpec ::= pkgImp(locn,visibility,ast) | openStmt(locn,ast).

  public defnSpec ::= defnSpec(defnSp,locn,list[ast]).

  public defnSp ::= varSp(string)
  | cnsSp(string)
  | tpSp(string)
  | conSp(string)
  | implSp(string).

  public implementation display[defnSp] => let{
    dispSp(varSp(Nm)) => ssSeq([ss("var: "),ss(Nm)]).
    dispSp(cnsSp(Nm)) => ssSeq([ss("constructor: "),ss(Nm)]).
    dispSp(tpSp(Nm)) => ssSeq([ss("type: "),ss(Nm)]).
    dispSp(conSp(Nm)) => ssSeq([ss("contract: "),ss(Nm)]).
    dispSp(implSp(Nm)) => ssSeq([ss("implementation: "),ss(Nm)]).
  } in {.
    disp = dispSp
  .}

  public implementation equality[defnSp] => let{
    eql(cnsSp(S1),cnsSp(S2)) => S1==S2.
    eql(tpSp(S1),tpSp(S2)) => S1==S2.
    eql(varSp(S1),varSp(S2)) => S1==S2.
    eql(implSp(S1),implSp(S2)) => S1==S2.
    eql(conSp(S1),conSp(S2)) => S1==S2.
    eql(_,_) default => false.
  } in {
    S1 == S2 => eql(S1,S2)
  }

  public implementation display[defnSpec] => let{
    dispSpec(defnSpec(Sp,Lc,Els)) =>
      ssSeq([disp(Sp),ss("@"),disp(Lc),disp(Els)])
  } in {
    disp = dispSpec
  }

  public implementation display[importSpec] => let{
    dispSpc(pkgImp(Lc,Vi,Pk)) =>
      ssSeq([disp(Vi),ss(" import "),disp(Pk)]).
    dispSpc(openStmt(Lc,Pk)) =>
      ssSeq([ss("open "),disp(Pk)]).
  } in {.
    disp(S) => dispSpc(S)
  .}
}
