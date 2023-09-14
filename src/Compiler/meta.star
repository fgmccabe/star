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
    .pUblic < .transItive => .true.
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

  public pkgSpec ::= pkgSpec{
    pkg : pkg.
    imports : cons[importSpec].
    exports: cons[decl]}.

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
    | .implSp(string).

  public implementation display[defnSp] => let{
    dispSp(S) => case S in {
      .varSp(Nm) => "var: $(Nm)".
      .cnsSp(Nm) => "constructor: $(Nm)".
      .tpSp(Nm) => "type: $(Nm)".
      .conSp(Nm) => "contract: $(Nm)".
      .implSp(Nm) => "implementation: $(Nm)".
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
    disp(P) =>
      "Package: $(P.pkg)\n  imports=#(interleave(P.imports//disp,"\n")*)\n  exports=#(interleave(P.exports//disp,"\n")*)".
  }

  public implementation display[decl] => {
    disp(Dc) => case Dc in {
      .conDec(_,Nm,_,RlTp) => "Ctrct #(Nm)\:$(RlTp)".
      .implDec(_,Nm,ImplNm,ImplTp) => "Impl #(Nm)[#(ImplNm)]\:$(ImplTp)".
      .accDec(_,Tp,Fld,Fun,FunTp) => "Acc $(Tp).#(Fld) using #(Fun)\:$(FunTp)".
      .updDec(_,Tp,Fld,Fun,FunTp) => "Upd $(Tp).#(Fld) using #(Fun)\:$(FunTp)".
      .tpeDec(_,Nm,_,TpRl) => "Type #(Nm)\::$(TpRl)".
      .varDec(_,Nm,FullNm,Tp) => "Var #(Nm)[#(FullNm)]\:$(Tp)".
      .funDec(_,Nm,FullNm,Tp) => "Fun #(Nm)[#(FullNm)]\:$(Tp)".
      .cnsDec(_,Nm,FullNm,Tp) => "Con #(Nm)[#(FullNm)]\:$(Tp)".
    }
  }

  public contract all e ~~ hasName[e] ::= {
    lclName:(e) => option[string].
    fullName:(e) => option[string].
  }

  public implementation hasName[decl] => {
    lclName(D) => case D in {
      .conDec(_,Nm,_,_) => .some(Nm).
      .implDec(_,Nm,_,_) => .some(Nm).
      .tpeDec(_,Nm,_,TpRl) => .some(Nm).
      .varDec(_,Nm,FullNm,Tp) => .some(Nm).
      .funDec(_,Nm,FullNm,Tp) => .some(Nm).
      .cnsDec(_,Nm,FullNm,Tp) => .some(Nm).
      _ default => .none
    }.
    fullName(D) => case D in {
      .conDec(_,_,FullNm,_) => .some(FullNm).
      .implDec(_,_,FullNm,_) => .some(FullNm).
      .tpeDec(_,Nm,Tp,_) => .some(tpName(Tp)).
      .varDec(_,Nm,FullNm,Tp) => .some(FullNm).
      .funDec(_,Nm,FullNm,Tp) => .some(FullNm).
      .cnsDec(_,Nm,FullNm,Tp) => .some(FullNm).
      _ default => .none
    }.
  }
  
  public optimizationLvl ::= .base | .inlining.

  public implementation equality[optimizationLvl] => {
    .base == .base => .true.
    .inlining == .inlining => .true.
    _ == _ default => .false
  }

  public implementation coercion[string,optimizationLvl] => {
    _coerce(Lvl) => case Lvl in {
      "base" => .some(.base).
      "inline" => .some(.inlining).
      "0" => .some(.base).
      "1" => .some(.base).
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
  public showMacrod = ref .false.
  public macroOnly = ref .false.
  public showCanon = ref .false.
  public traceCanon = ref .false.
  public typeCheckOnly = ref .false.
  public showNormalize = ref .false.
  public traceNormalize = ref .false.
  public optimization = ref .base.
  public traceCodegen = ref .false.
  public traceInline = ref .false.
  public showCode = ref .false.
  public genCode = ref .true.
  public genDebug = ref .false.

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
    shortForm = "-da".
    alternatives = [].
    usage = "-da -- show ast".
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
    shortForm = "-tm".
    alternatives = [].
    usage = "-tm -- trace macro".
    validator = .none.
    setOption(_,Opts) => valof{
      macroTracing := .true;
      valis Opts
    }
  }

  public showMacroOption:cmdOption[compilerOptions].
  showMacroOption = cmdOption{
    shortForm = "-dm".
    alternatives = [].
    usage = "-dm -- show macro".
    validator = .none.
    setOption(_,Opts) => valof{
      showMacrod := .true;
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
    shortForm = "-dn".
    alternatives = [].
    usage = "-dn -- show type normalizes".
    validator = .none.
    setOption(_,Opts) => valof{
      showNormalize := .true;
      
      valis Opts
    }
  }

  public traceNormalizeOption:cmdOption[compilerOptions].
  traceNormalizeOption = cmdOption{
    shortForm = "-tn".
    alternatives = [].
    usage = "-tn -- trace normalize".
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

  public showCodegenOption:cmdOption[compilerOptions].
  showCodegenOption = cmdOption{
    shortForm = "-di".
    alternatives = [].
    usage = "-di -- show generated code".
    validator = .none.
    setOption(_,Opts) => valof{
      showCode := .true;
      
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
    validator = .some((O)=> _ ?= O:?optimizationLvl).
    setOption(L,Opts) where Lvl?=L:?optimizationLvl => valof{
      optimization := Lvl;
      valis Opts
    }
  }

  public traceInlineOption:cmdOption[compilerOptions].
  traceInlineOption = cmdOption{
    shortForm = "-tO".
    alternatives = [].
    usage = "-tO -- trace inlining".
    validator = .none.
    setOption(_,Opts) => valof{
      traceInline := .true;
      
      valis Opts
    }
  }

  public repoOption:cmdOption[compilerOptions].
  repoOption = cmdOption{
    shortForm = "-R".
    alternatives = [].
    usage = "-R dir -- directory of repository".
    validator = .some(isDir).
    setOption(R,Opts) where RU ?= parseUri(R) && NR?=resolveUri(Opts.cwd,RU) =>
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
    validator = .some(isDir).
    setOption(W,Opts) where RW ?= parseUri(W) && NW?=resolveUri(Opts.cwd,RW)=>
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
    validator = .some((_)=>.true).
    setOption(R,Opts) where RU ?= parseUri(R) && NR?=resolveUri(Opts.cwd,RU) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=.some(NR).
	doStdin=Opts.doStdin
      }.
  }

  public genDebugOption:cmdOption[compilerOptions].
  genDebugOption = cmdOption{
    shortForm = "-g".
    alternatives = ["--debug"].
    usage = "-g -- generate debug info".
    validator = .none.
    setOption(_,Opts) => valof{
      genDebug := .true;
      valis Opts
    }
  }

  public metaTrace:all e ~~ display[e] |: (e,string,ref boolean) => e.
  metaTrace(X,Msg,Flg) where Flg! => valof{
    logMsg("#(Msg): $(X)");
    valis X
  }
  metaTrace(X,_,_) default => X.
}
