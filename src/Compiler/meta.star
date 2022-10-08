star.compiler.meta{
  import star.

  import star.cmdOpts.
  import star.file.
  import star.uri.
  import star.pkg.

  import star.compiler.ast.
  import star.compiler.location.
  import star.compiler.types.

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
    .transItive < .pUblic => .true.
    _ < _ default => .false.

    .priVate >= .priVate => .true.
    .deFault >= .priVate => .true.
    .deFault >= .deFault => .true.
    .pUblic >= .priVate => .true.
    .pUblic >= .deFault => .true.
    .pUblic >= .transItive => .true.
    .pUblic >= .pUblic => .true.
    _ >= _ default => .false.
  }

  public implementation equality[visibility] => {
    .priVate == .priVate => .true.
    .deFault == .deFault => .true.
    .pUblic == .pUblic => .true.
    .transItive == .transItive => .true.
    _ == _ default => .false.
  }

  public pkgSpec::= .pkgSpec(pkg,cons[importSpec],cons[decl]).

  public decl ::= .implDec(option[locn],string,string,tipe) |
    .accDec(option[locn],tipe,string,string,tipe) |
    .updDec(option[locn],tipe,string,string,tipe) |
    .conDec(option[locn],string,string,typeRule) |
    .tpeDec(option[locn],string,tipe,typeRule) |
    .varDec(option[locn],string,string,tipe) |
    .funDec(option[locn],string,string,tipe) |
    .cnsDec(option[locn],string,string,tipe).

  public importSpec ::= .pkgImp(option[locn],visibility,pkg).

  public defnSpec ::= .defnSpec(defnSp,option[locn],cons[ast]).

  public defnSp ::= .varSp(string)
    | .cnsSp(string)
    | .tpSp(string)
    | .conSp(string)
    | .implSp(string)
    | .accSp(string)
    | .updSp(string).

  public implementation display[defnSp] => let{
    dispSp(S) => case S in {
      .varSp(Nm) => "var: $(Nm)".
      .cnsSp(Nm) => "constructor: $(Nm)".
      .tpSp(Nm) => "type: $(Nm)".
      .conSp(Nm) => "contract: $(Nm)".
      .implSp(Nm) => "implementation: $(Nm)".
      .accSp(Nm) => "accessor: $(Nm)".
      .updSp(Nm) => "updater: $(Nm)".
    }
  } in {
    disp = dispSp
  }

  public implementation equality[defnSp] => let{
    eql(Sp1,Sp2) => case Sp1 in {
      .cnsSp(S1) => .cnsSp(S2).=Sp2 && S1==S2.
      .tpSp(S1) => .tpSp(S2).=Sp2 && S1==S2.
      .varSp(S1) => .varSp(S2).=Sp2 && S1==S2.
      .implSp(S1) => .implSp(S2).=Sp2 && S1==S2.
      .accSp(S1) => .accSp(S2).=Sp2 && S1==S2.
      .updSp(S1) => .updSp(S2).=Sp2 && S1==S2.
      .conSp(S1) => .conSp(S2).=Sp2 && S1==S2.
      _ default => .false.
    }
  } in {
    S1 == S2 => eql(S1,S2)
  }

  public implementation hashable[defnSp] => {
    hash(Sp) => case Sp in {
      .varSp(Nm) => hash(Nm)*37+hash("var").
      .cnsSp(Nm) => hash(Nm)*37+hash("cns").
      .tpSp(Nm) => hash(Nm)*37+hash("tp").
      .conSp(Nm) => hash(Nm)*37+hash("con").
      .implSp(Nm) => hash(Nm)*37+hash("impl").
      .accSp(Nm) => hash(Nm)*37+hash("access").
      .updSp(Nm) => hash(Nm)*37+hash("update").
    }
  }

  public implementation display[defnSpec] => let{
    dispSpec(.defnSpec(Sp,Lc,Els)) => "$(Sp)@$(Lc)$(Els)"
  } in {
    disp = dispSpec
  }

  public implementation display[importSpec] => let{
    dispSpc(.pkgImp(Lc,Vi,Pk)) => "$(Vi) import $(Pk)"
  } in {
    disp(S) => dispSpc(S)
  }

  public implementation display[pkgSpec] => {
    disp(.pkgSpec(Pkg,Imports,Decls)) =>
      "Package: $(Pkg), imports=$(Imports), exports=$(Decls)".
  }

  public implementation display[decl] => {
    disp(Dc) => case Dc in {
      .implDec(_,Nm,ImplNm,ImplTp) => "Impl #(Nm)~#(ImplNm)\:$(ImplTp)".
      .accDec(_,Tp,Fld,Fun,FunTp) => "Acc $(Tp).#(Fld) using #(Fun)\:$(FunTp)".
      .updDec(_,Tp,Fld,Fun,FunTp) => "Update $(Tp).#(Fld) using #(Fun)\:$(FunTp)".
      .conDec(_,Nm,_,RlTp) => "Contract #(Nm)\:$(RlTp)".
      .tpeDec(_,Nm,Tp,_) => "Type #(Nm)\::$(Tp)".
      .varDec(_,Nm,FullNm,Tp) => "Var #(FullNm)\:$(Tp)".
      .funDec(_,Nm,FullNm,Tp) => "Fun #(FullNm)\:$d(Tp)".
      .cnsDec(_,Nm,FullNm,Tp) => "Con #(FullNm)\:$(Tp)".
    }
  }

  public optimizationLvl ::= .base | .inlining.

  public implementation equality[optimizationLvl] => {
    .base == .base => .true.
    .inlining == .inlining => .true.
    _ == _ default => .false
  }

  public implementation coercion[string,optimizationLvl] => {
    _coerce(Lvl) => case Lvl in {
      "base" => some(.base).
      "inline" => some(.inlining).
      "0" => some(.base).
      "1" => some(.base).
      _ default => .none.
    }
  }

  public implementation display[optimizationLvl] => {
    disp(.base) => "base".
    disp(.inlining) => "inlining"
  }

  public traceAst = ref .false.
  public traceDependencies = ref .false.
  public macroTracing = ref .false.
  public macroOnly = ref .false.
  public showCanon = ref .false.
  public traceCanon = ref .false.
  public typeCheckOnly = ref .false.
  public showNormalize = ref .false.
  public traceNormalize = ref .false.
  public optimization = ref .base.
  public traceCodegen = ref .false.
  public showCode = ref .false.
  public genCode = ref .true.

  public compilerOptions ::=
    compilerOptions{
      repo:uri.
      cwd:uri.
      graph:option[uri].
      doStdin:boolean.
    }.

  public defltOptions(WI,RI) =>compilerOptions{repo=RI.
    cwd=WI.
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

  public macroTracingOption:cmdOption[compilerOptions].
  macroTracingOption = cmdOption{
    shortForm = "-dM".
    alternatives = [].
    usage = "-dM -- show macro".
    validator = .none.
    setOption(_,Opts) => valof{
      macroTracing := .true;
      valis Opts
    }
  }

  public traceDependencyOption:cmdOption[compilerOptions].
  traceDependencyOption = cmdOption{
    shortForm = "-tD".
    alternatives = [].
    usage = "-tD -- trace dependencies".
    validator = .none.
    setOption(_,Opts) => valof{
      traceDependencies := .true;
      valis Opts
    }
  }

  public checkOnlyOption:cmdOption[compilerOptions].
  checkOnlyOption = cmdOption{
    shortForm = "-c".
    alternatives = ["--type-check-only"].
    usage = "-c -- type check only".
    validator = .none.
    setOption(_,Opts) => valof{
      typeCheckOnly := .true;
      valis Opts
    }
  }

  public traceCheckOption:cmdOption[compilerOptions].
  traceCheckOption = cmdOption{
    shortForm = "-tc".
    alternatives = [].
    usage = "-tc -- trace typechecking".
    validator = .none.
    setOption(_,Opts) => valof{
      traceCanon := .true;
      
      valis Opts
    }
  }

  public showCheckOption:cmdOption[compilerOptions].
  showCheckOption = cmdOption{
    shortForm = "-dc".
    alternatives = [].
    usage = "-dc -- show type checkedcode".
    validator = .none.
    setOption(_,Opts) => valof{
      showCanon := .true;
      
      valis Opts
    }
  }

  public showNormalizeOption:cmdOption[compilerOptions].
  showNormalizeOption = cmdOption{
    shortForm = "-dT".
    alternatives = [].
    usage = "-dT -- show type normalizes".
    validator = .none.
    setOption(_,Opts) => valof{
      showNormalize := .true;
      
      valis Opts
    }
  }

  public traceNormalizeOption:cmdOption[compilerOptions].
  traceNormalizeOption = cmdOption{
    shortForm = "-tT".
    alternatives = [].
    usage = "-tT -- trace normalize".
    validator = .none.
    setOption(_,Opts) => valof{
      traceNormalize := .true;
      
      valis Opts
    }
  }

  public traceCodegenOption:cmdOption[compilerOptions].
  traceCodegenOption = cmdOption{
    shortForm = "-ti".
    alternatives = [].
    usage = "-ti -- trace code generation".
    validator = .none.
    setOption(_,Opts) => valof{
      traceCodegen := .true;
      
      valis Opts
    }
  }

  public noCodeOption:cmdOption[compilerOptions].
  noCodeOption = cmdOption{
    shortForm = "-x".
    alternatives = ["--no-codegen"].
    usage = "-x -- dont generate code".
    validator = .none.
    setOption(_,Opts) => valof{
      genCode := .false;
      valis Opts
    }
  }

  public optimizeLvlOption:cmdOption[compilerOptions].
  optimizeLvlOption = cmdOption{
    shortForm = "-O".
    alternatives = ["--optimize"].
    usage = "-O <Lvl> -- optimization level".
    validator = some((O)=> _ ^= O:?optimizationLvl).
    setOption(L,Opts) where Lvl^=L:?optimizationLvl => valof{
      optimization := Lvl;
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
	doStdin=.true.
      }.
  }

  public graphOption:cmdOption[compilerOptions].
  graphOption = cmdOption{
    shortForm = "-G".
    alternatives = ["--genGraph"].
    usage = "-G uri -- generate dependency graph".
    validator = some((_)=>.true).
    setOption(R,Opts) where RU ^= parseUri(R) && NR^=resolveUri(Opts.cwd,RU) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=some(NR).
	doStdin=Opts.doStdin
      }.
  }
}
