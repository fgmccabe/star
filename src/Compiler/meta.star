star.compiler.meta{
  import star.

  import star.uri.
  import star.pkg.

  import star.compiler.ast.
  import star.compiler.ast.disp.
  import star.compiler.location.

  public visibility ::= .priVate | .deFault | .pUblic | .transItive.

  public implementation display[visibility] => {
    disp(.priVate) => ss("private").
    disp(.pUblic) => ss("public").
    disp(.deFault) => ss("default").
    disp(.transItive) => ss("transitive").
  }

  public implementation comp[visibility] => {
    .priVate < .pUblic => .true.
    .priVate < .transItive => .true.
    .priVate < .deFault => .true.
    .deFault < .pUblic => .true.
    .deFault < .transItive => .true.
    .pUblic < .transItive => .true.
    _ < _ default => .false.

    .priVate >= .priVate => .true.
    .deFault >= .priVate => .true.
    .deFault >= .deFault => .true.
    .pUblic >= .priVate => .true.
    .pUblic >= .deFault => .true.
    .pUblic >= .pUblic => .true.
    .transItive >= _ => .true.
    _ >= _ default => .false.
  }

  public implementation equality[visibility] => {
    .priVate == .priVate => .true.
    .deFault == .deFault => .true.
    .pUblic == .pUblic => .true.
    .transItive == .transItive => .true.
    _ == _ default => .false.
  }

  public importSpec ::= pkgImp(locn,visibility,pkg).

  public defnSpec ::= defnSpec(defnSp,locn,cons[ast]).

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
    eql(_,_) default => .false.
  } in {
    S1 == S2 => eql(S1,S2)
  }

  public implementation hash[defnSp] => {.
    hash(varSp(Nm)) => hash(Nm)*37+hash("var").
    hash(cnsSp(Nm)) => hash(Nm)*37+hash("cns").
    hash(tpSp(Nm)) => hash(Nm)*37+hash("tp").
    hash(conSp(Nm)) => hash(Nm)*37+hash("con").
    hash(implSp(Nm)) => hash(Nm)*37+hash("impl").
  .}

  public implementation display[defnSpec] => let{
    dispSpec(defnSpec(Sp,Lc,Els)) =>
      ssSeq([disp(Sp),ss("@"),disp(Lc),disp(Els)])
  } in {
    disp = dispSpec
  }

  public implementation display[importSpec] => let{
    dispSpc(pkgImp(Lc,Vi,Pk)) =>
      ssSeq([disp(Vi),ss(" import "),disp(Pk)]).
  } in {.
    disp(S) => dispSpc(S)
  .}

  public compilerOptions ::=
    compilerOptions{
      repo:uri.
      cwd:uri.
      graph:option[uri].
      showAst:boolean.
      showMacro:boolean.
      showCanon:boolean.
      showCore:boolean.
      showCode:boolean.
    }.
}
