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

  public _main:(cons[string])=>().
  _main(Args) => valof{
    WI= ? parseUri("file:"++_cwd());
    RI= ? parseUri("file:"++_repo());
    try{
      valis handleCmds(processOptions(Args,[wdOption,
	    stdinOption,
	    traceLexOption,
	    showLexOption,
	    traceParseOption,
	    showParseOption],
	  defltOptions(WI,RI)
	))
    } catch {
      Msg => { logMsg(Msg);
	valis ()
      }
    };
  }

  handleCmds:((rdfOptions,cons[string]))=>().
  handleCmds((Opts,Args)) => valof{
    for O in Args do{
      resetErrors();

      if OUri ?= parseUri(O) && SrcUri ?= resolveUri(Opts.cwd,OUri) then{
	if Txt ?= getResource(SrcUri) then{
	  (Toks) = allTokens(startLoc(O),Txt::cons[char]);

	  if showLex! then
	    logMsg("tokens from $(O)\: $(Toks)");

	  if Trpls ?= (parseGraph() --> Toks) then{
	    if showParse! then
	      logMsg("Triples: $(Trpls)");

	    Graph = foldRight((Tr,Gx)=>addTriple(Gx,Tr),nullGraph,Trpls);

	    logMsg("Graph is $(Graph)");

	    assert validGraph(Graph);
	  } else{
	    logMsg("something went wrong with parsing triples")
	  }
	} else
	logMsg("cant find ontology source text at $(SrcUri)");
      } else
	logMsg("cant resolve ontology source uri for $(O)");
    };
    valis ()
  }
}

