star.compiler.opts{
  import star.

  import star.cmdOpts.
  import star.file.
  import star.uri.

  public optimizationLvl ::= .base | .inlining.

  public implementation equality[optimizationLvl] => {
    .base == .base => .true.
    .inlining == .inlining => .true.
    _ == _ default => .false
  }

  public implementation coercion[string,optimizationLvl] => {
    _coerce(Lvl) => case Lvl in {
      | "base" => .some(.base)
      | "inline" => .some(.inlining)
      | "0" => .some(.base)
      | "1" => .some(.base)
      | _ default => .none
    }
  }

  public implementation display[optimizationLvl] => {
    disp(.base) => "base".
    disp(.inlining) => "inlining"
  }

  public forceCompile = ref .false.

  public traceAst = ref .false.
  public traceDependencies = ref .false.
  public macroTracing = ref .false.
  public showMacrod = ref .false.
  public macroOnly = ref .false.
  public showCanon = ref .false.
  public traceCanon = ref .false.
  public typeCheckOnly = ref .false.
  public traceResolve = ref .false.
  public showNormalize = ref .false.
  public traceNormalize = ref .false.
  public optimization = ref .base.
  public traceCodegen = ref .false.
  public traceInline = ref .false.
  public showCode = ref .false.
  public genCode = ref .true.
  public genWasm = ref .false.
  public traceWasm = ref .false.
  public genDebug = ref .false.

  public compilerOptions ::=
    compilerOptions{
      repo:uri.
      cwd:uri.
      graph:option[uri].
      doStdin:boolean.
      wasm:option[uri].
    }.

  public defltOptions(WI,RI) =>compilerOptions{repo=RI.
    cwd=WI.
    graph = .none.
    doStdin=.false.
    wasm = .none.
  }

  public forceCompileOption:cmdOption[compilerOptions].
  forceCompileOption = cmdOption{
    shortForm = "-f".
    alternatives = [].
    usage = "-f -- force compilation".
    validator = .none.
    setOption(_,Opts) => valof{
      forceCompile := .true;
      
      valis Opts
    }
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

  public macroTraceOption:cmdOption[compilerOptions].
  macroTraceOption = cmdOption{
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

  public traceResolveOption:cmdOption[compilerOptions].
  traceResolveOption = cmdOption{
    shortForm = "-tr".
    alternatives = [].
    usage = "-tr -- trace resolving".
    validator = .none.
    setOption(_,Opts) => valof{
      traceResolve := .true;
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
      (Opts.repo=NR).
  }

  public wdOption:cmdOption[compilerOptions].
  wdOption = cmdOption{
    shortForm = "-W".
    alternatives = [].
    usage = "-W dir -- working directory".
    validator = .some(isDir).
    setOption(W,Opts) where RW ?= parseUri(W) && NW?=resolveUri(Opts.cwd,RW)=>
      (Opts.cwd=NW).
  }

  public genWasmOption:cmdOption[compilerOptions].
  genWasmOption = cmdOption{
    shortForm = "-w".
    alternatives = [].
    usage = "-w file -- generate wasm in file".
    validator = .some((_)=>.true).
    setOption(R,Opts) where RU ?= parseUri(R) && NR?=resolveUri(Opts.cwd,RU) => valof{
      genWasm := .true;
      valis (Opts.wasm=.some(NR))
    }.
  }

  public traceWasmOption:cmdOption[compilerOptions].
  traceWasmOption = cmdOption{
    shortForm = "-tw".
    alternatives = [].
    usage = "-tw -- trace wasm generation".
    validator = .none.
    setOption(_,Opts) => valof{
      traceWasm := .true;
      
      valis Opts
    }
  }

  public stdinOption:cmdOption[compilerOptions].
  stdinOption = cmdOption{
    shortForm = "".
    alternatives = ["--stdin"].
    usage = "--stdin -- compile standard input".
    validator = .none.
    setOption(_,Opts) => (Opts.doStdin=.true).
  }

  public graphOption:cmdOption[compilerOptions].
  graphOption = cmdOption{
    shortForm = "-G".
    alternatives = ["--genGraph"].
    usage = "-G uri -- generate dependency graph".
    validator = .some((_)=>.true).
    setOption(R,Opts) where RU ?= parseUri(R) && NR?=resolveUri(Opts.cwd,RU) =>
      (Opts.graph=.some(NR)).
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
}  
  
