rdf.driver{
  import star.

  import star.assert.
  import star.file.
  import star.location.
  import star.pkg.
  import star.resources.
  import star.uri.
  import star.cmdOpts.

  import rdf.errors.
  import rdf.meta.
  import rdf.lexer.
  import rdf.token.
  import rdf.parser.
  import rdf.triple.
  import rdf.graph.

  public _main:(cons[string])=> integer.
  _main(Args) => valof{
    try{
      WI= ? parseUri("file:"++_cwd());
      RI= ? parseUri("file:"++_repo());
      try{
	handleCmds(processOptions(Args,[wdOption,
	      stdinOption,
	      traceLexOption,
	      showLexOption,
	      traceParseOption,
	      showParseOption],
	    defltOptions(WI,RI)
	  ));
	valis 0
      } catch {
	Msg do {
	  logMsg(.severe,Msg);
	  valis 10
	}
      }
    } catch {
      .exception(Msg) do {
	logMsg(.severe,Msg);
	valis 12
      }
    }
  }

  handleCmds:((rdfOptions,cons[string])){}.
  handleCmds((Opts,Args)){
    for O in Args do{
      resetErrors();

      if OUri ?= parseUri(O) && SrcUri ?= resolveUri(Opts.cwd,OUri) then{
	if Grph ?= parseN3(SrcUri) then{
	  if showParse! then{
	    logMsg(.info,"Graph is $(Grph)");
	  }
	} else{
	  logMsg(.warning,"something went wrong with parsing triples")
	}
      } else{
	logMsg(.severe,"cant resolve ontology source uri for $(O)");
      }
    }
  }
}

