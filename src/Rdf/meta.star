rdf.meta{
  import star.

  import star.cmdOpts.
  import star.file.
  import star.uri.

  public traceLex = ref .false.
  public showLex = ref .false.
  public traceParse = ref .false.
  public showParse = ref .false.

  public rdfOptions ::=
    rdfOptions{
      repo:uri.
      cwd:uri.
      doStdin:boolean.
    }.

  public defltOptions(WI,RI) =>rdfOptions{repo=RI.
    cwd=WI.
    doStdin=.false.
  }

  public wdOption:cmdOption[rdfOptions].
  wdOption = cmdOption{
    shortForm = "-W".
    alternatives = [].
    usage = "-W dir -- working directory".
    validator = .some(isDir).
    setOption(W,Opts) where RW ?= parseUri(W) && NW?=resolveUri(Opts.cwd,RW)=>
      rdfOptions{repo=Opts.repo.
	cwd=NW.
	doStdin=Opts.doStdin
      }.
  }

  public stdinOption:cmdOption[rdfOptions].
  stdinOption = cmdOption{
    shortForm = "".
    alternatives = ["--stdin"].
    usage = "--stdin -- compile standard input".
    validator = .none.
    setOption(_,Opts) =>
      rdfOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	doStdin=.true.
      }.
  }

  public repoOption:cmdOption[rdfOptions].
  repoOption = cmdOption{
    shortForm = "-R".
    alternatives = [].
    usage = "-R dir -- directory of repository".
    validator = .some(isDir).
    setOption(R,Opts) where RU ?= parseUri(R) && NR?=resolveUri(Opts.cwd,RU) =>
      rdfOptions{repo=NR.
	cwd=Opts.cwd.
	doStdin=Opts.doStdin
      }.
  }

  public traceLexOption:cmdOption[rdfOptions].
  traceLexOption = cmdOption{
    shortForm = "-tl".
    alternatives = [].
    usage = "-tl -- trace rdf lex".
    validator = .none.
    setOption(_,Opts) => valof{
      traceLex := .true;
      
      valis Opts
    }
  }

  public showLexOption:cmdOption[rdfOptions].
  showLexOption = cmdOption{
    shortForm = "-dl".
    alternatives = [].
    usage = "-dl -- display rdf lex".
    validator = .none.
    setOption(_,Opts) => valof{
      showLex := .true;
      
      valis Opts
    }
  }

  public traceParseOption:cmdOption[rdfOptions].
  traceParseOption = cmdOption{
    shortForm = "-tp".
    alternatives = [].
    usage = "-tp -- trace parsing rdf".
    validator = .none.
    setOption(_,Opts) => valof{
      traceParse := .true;
      
      valis Opts
    }
  }

  public showParseOption:cmdOption[rdfOptions].
  showParseOption = cmdOption{
    shortForm = "-dp".
    alternatives = [].
    usage = "-dp -- display rdf parse".
    validator = .none.
    setOption(_,Opts) => valof{
      showParse := .true;
      
      valis Opts
    }
  }
}
  
