star.compiler.meta{
  import star.

  import star.cmdOpts.
  import star.file.
  import star.uri.
  import star.pkg.

  import star.compiler.ast.
  import star.compiler.location.

  public visibility ::= .priVate | .deFault | .pUblic | .transItive.

  public implementation display[visibility] => {
    disp(.priVate) => "private".
    disp(.pUblic) => "public".
    disp(.deFault) => "default".
    disp(.transItive) => "transitive".
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
    | implSp(string)
    | accSp(string)
    | updSp(string).

  public implementation display[defnSp] => let{
    dispSp(varSp(Nm)) => "var: $(Nm)".
    dispSp(cnsSp(Nm)) => "constructor: $(Nm)".
    dispSp(tpSp(Nm)) => "type: $(Nm)".
    dispSp(conSp(Nm)) => "contract: $(Nm)".
    dispSp(implSp(Nm)) => "implementation: $(Nm)".
    dispSp(accSp(Nm)) => "accessor: $(Nm)".
    dispSp(updSp(Nm)) => "updater: $(Nm)".
  } in {
    disp = dispSp
  }

  public implementation equality[defnSp] => let{
    eql(cnsSp(S1),cnsSp(S2)) => S1==S2.
    eql(tpSp(S1),tpSp(S2)) => S1==S2.
    eql(varSp(S1),varSp(S2)) => S1==S2.
    eql(implSp(S1),implSp(S2)) => S1==S2.
    eql(accSp(S1),accSp(S2)) => S1==S2.
    eql(updSp(S1),updSp(S2)) => S1==S2.
    eql(conSp(S1),conSp(S2)) => S1==S2.
    eql(_,_) default => .false.
  } in {
    S1 == S2 => eql(S1,S2)
  }

  public implementation hash[defnSp] => {
    hash(varSp(Nm)) => hash(Nm)*37+hash("var").
    hash(cnsSp(Nm)) => hash(Nm)*37+hash("cns").
    hash(tpSp(Nm)) => hash(Nm)*37+hash("tp").
    hash(conSp(Nm)) => hash(Nm)*37+hash("con").
    hash(implSp(Nm)) => hash(Nm)*37+hash("impl").
    hash(accSp(Nm)) => hash(Nm)*37+hash("access").
    hash(updSp(Nm)) => hash(Nm)*37+hash("update").
  }

  public implementation display[defnSpec] => let{
    dispSpec(defnSpec(Sp,Lc,Els)) => "$(Sp)@$(Lc)$(Els)"
  } in {
    disp = dispSpec
  }

  public implementation display[importSpec] => let{
    dispSpc(pkgImp(Lc,Vi,Pk)) => "$(Vi) import $(Pk)"
  } in {
    disp(S) => dispSpc(S)
  }

  public optimizationLvl ::= .base | .inlining.

  public implementation equality[optimizationLvl] => {
    .base == .base => .true.
    .inlining == .inlining => .true.
    _ == _ default => .false
  }

  public implementation coercion[string,optimizationLvl] => {
    _coerce("base") => some(.base).
    _coerce("inline") => some(.inlining).
    _coerce("0") => some(.base).
    _coerce("1") => some(.base).
    _coerce(_) default => .none.
  }

  public implementation display[optimizationLvl] => {
    disp(.base) => "base".
    disp(.inlining) => "inlining"
  }

  public traceAst = ref .false.
  public traceDependencies = ref .false.
  public traceMacro = ref .false.
  public macroOnly = ref .false.
  public traceCanon = ref .false.
  public typeCheckOnly = ref .false.
  public showCore = ref .false.
  public showCode = ref .false.

  public compilerOptions ::=
    compilerOptions{
      repo:uri.
      cwd:uri.
      graph:option[uri].
      optimization:optimizationLvl.
      doStdin:boolean.
    }.

  public defltOptions(WI,RI) =>compilerOptions{repo=RI.
    cwd=WI.
    optimization = .base.
    graph = .none.
    doStdin=.false.
  }

  public traceAstOption:cmdOption[compilerOptions].
  traceAstOption = cmdOption{
    shortForm = "-dA".
    alternatives = [].
    usage = "-dA -- show ast".
    validator = .none.
    setOption(_,Opts) => valof{
      traceAst := .true;
      
      valis Opts
    }
  }

  public macroOnlyOption:cmdOption[compilerOptions].
  macroOnlyOption = cmdOption{
    shortForm = "-m".
    alternatives = ["--macro-only"].
    usage = "-m -- only do macros".
    validator = .none.
    setOption(_,Opts) => valof{
      macroOnly := .true;
      valis Opts
    }
  }

  public traceMacroOption:cmdOption[compilerOptions].
  traceMacroOption = cmdOption{
    shortForm = "-dM".
    alternatives = [].
    usage = "-dM -- show macro".
    validator = .none.
    setOption(_,Opts) => valof{
      traceMacro := .true;
      valis Opts
    }
  }

  public traceDependencyOption:cmdOption[compilerOptions].
  traceDependencyOption = cmdOption{
    shortForm = "-dD".
    alternatives = [].
    usage = "-dD -- trace dependencies".
    validator = .none.
    setOption(_,Opts) => valof{
      traceDependencies := .true;
      valis Opts
    }
  }

  public traceCheckOption:cmdOption[compilerOptions].
  traceCheckOption = cmdOption{
    shortForm = "-dt".
    alternatives = [].
    usage = "-dt -- show type checkedcode".
    validator = .none.
    setOption(_,Opts) => valof{
      traceCanon := .true;
      
      valis Opts
    }
  }

  public repoOption:cmdOption[compilerOptions].
  repoOption = cmdOption{
    shortForm = "-R".
    alternatives = [].
    usage = "-R dir -- directory of repository".
    validator = some(isDir).
    setOption(R,Opts) where RU ^= parseUri(R) && NR^=resolveUri(Opts.cwd,RU) =>
      compilerOptions{repo=NR.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	doStdin=Opts.doStdin
      }.
  }

  public wdOption:cmdOption[compilerOptions].
  wdOption = cmdOption{
    shortForm = "-W".
    alternatives = [].
    usage = "-W dir -- working directory".
    validator = some(isDir).
    setOption(W,Opts) where RW ^= parseUri(W) && NW^=resolveUri(Opts.cwd,RW)=>
      compilerOptions{repo=Opts.repo.
	cwd=NW.
	graph=Opts.graph.
	optimization=Opts.optimization.
	doStdin=Opts.doStdin
      }.
  }

  public stdinOption:cmdOption[compilerOptions].
  stdinOption = cmdOption{
    shortForm = "".
    alternatives = ["--stdin"].
    usage = "--stdin -- compile standard input".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	doStdin=.true.
      }.
  }
}
