star.compiler.meta{
  import star.

  import star.compiler.ast.
  import star.compiler.location.

  public importSpec ::= pkgImp(locn,visibility,ast) | openStmt(locn,ast).

  public defnSpec ::= defnSpec(defnSp,list[ast]).

  public defnSp ::= varSp(locn,string)
  | cnsSp(locn,string)
  | tpSp(locn,string)
  | conSp(locn,string)
  | implSp(locn,string).

  public implementation display[defnSp] => let{
    dispSp(varSp(Lc,Nm)) => ssSeq([ss("var: "),ss(Nm),ss("@"),disp(Lc)]).
    dispSp(cnsSp(Lc,Nm)) => ssSeq([ss("constructor: "),ss(Nm),ss("@"),disp(Lc)]).
    dispSp(tpSp(Lc,Nm)) => ssSeq([ss("type: "),ss(Nm),ss("@"),disp(Lc)]).
    dispSp(conSp(Lc,Nm)) => ssSeq([ss("contract: "),ss(Nm),ss("@"),disp(Lc)]).
    dispSp(implSp(Lc,Nm)) => ssSeq([ss("implementation: "),ss(Nm),ss("@"),disp(Lc)]).
  } in {.
    disp = dispSp
  .}

  
}
