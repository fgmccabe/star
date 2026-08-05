rdf.sparql.parser{
  import star.

  import rdf.token.
  import rdf.triple.
  import rdf.sparql.query.

  /* Parse SPARQL queries -- see sparql.bnf for the reference grammar --
     into the query/pattern/term/expression AST defined in rdf.sparql.query.

     A number of grammar rules below are suffixed (Clause/Expr/Rule) to avoid
     colliding with same-named AST constructors imported from
     rdf.sparql.query (e.g. the bindClause rule vs. the pattern.bind
     constructor) -- importing that module brings its constructor labels into
     scope as bare names, which clash with a rule of the same name.

     Section 1: query forms and prologue. The following are used here but
     defined in later sections: groupGraphPattern (section 2); expression,
     constraint, builtInCall, functionCall, brackettedExpression (sections 4/5).

     Several rules here are suffixed `Rule` because the bare name is already a
     query.star AST constructor or type: datasetClause, describeTargets,
     groupCondition, orderCondition, selectVar, dataBlock.

     prologue/prologueDecl remain plain recognizers: prefix/base declarations
     aren't resolved against prefixedName here (that expansion, like the
     annotationItem reifier-identity question in section 3, belongs to a later
     pass), so there is nothing for them to produce yet. */

  public queryUnit:() >> query --> cons[token].
  queryUnit >> Q --> sparqlQuery >> Q, [.endTok(_)].

  sparqlQuery:() >> query --> cons[token].
  sparqlQuery >> combineTopValues(Q,VC) --> prologue,
    (selectQuery | constructQuery | describeQuery | askQuery) >> Q,
    valuesClause >> VC.

  /* A top-level trailing VALUES clause is equivalent to an inline VALUES
     block conjoined onto the query's own pattern (for DESCRIBE, onto its
     optional WHERE pattern, creating one if there wasn't one already). */
  combineTopValues:(query,option[dataBlock]) => query.
  combineTopValues(Q,.none) => Q.
  combineTopValues(.select(M,Pr,DCs,P,Mods),.some(DB)) => .select(M,Pr,DCs,conjPattern2(P,.values(DB)),Mods).
  combineTopValues(.construct(T,DCs,P,Mods),.some(DB)) => .construct(T,DCs,conjPattern2(P,.values(DB)),Mods).
  combineTopValues(.describe(DT,DCs,WP,Mods),.some(DB)) => .describe(DT,DCs,.some(conjPattern2(optPatOr(.nilPattern,WP),.values(DB))),Mods).
  combineTopValues(.ask(DCs,P,Mods),.some(DB)) => .ask(DCs,conjPattern2(P,.values(DB)),Mods).

  -- Prologue

  prologue --> prologueDecl*.

  prologueDecl --> baseDecl.
  prologueDecl --> prefixDecl.
  prologueDecl --> versionDecl.

  baseDecl --> sparqlKw("base"), iri.
  prefixDecl --> sparqlKw("prefix"), pnameNs, iri.
  versionDecl --> sparqlKw("version"), versionSpecifier.

  versionSpecifier --> [.tok(_,.strTok(_))].

  -- Query forms

  selectQuery:() >> query --> cons[token].
  selectQuery >> .select(Mod,Proj,DCs,P,Mods) --> selectClause >> (Mod,Proj), datasetClauseRule* >> DCs,
    whereClause >> P, solutionModifier >> Mods.

  subSelectClause:() >> query --> cons[token].
  subSelectClause >> .select(Mod,Proj,[],FinalPattern,Mods) -->
    selectClause >> (Mod,Proj), whereClause >> WP, solutionModifier >> Mods, valuesClause >> VC,
    {FinalPattern .= combineValues(WP,VC)}.

  combineValues:(pattern,option[dataBlock]) => pattern.
  combineValues(P,.none) => P.
  combineValues(P,.some(DB)) => conjPattern2(P,.values(DB)).

  selectClause:() >> (selectModifier,projection) --> cons[token].
  selectClause >> (Mod,Proj) --> sparqlKw("select"), ? selectModifierRule >> ModOpt, selectVars >> Proj,
    {Mod .= optSelectModifier(ModOpt)}.

  selectModifierRule:() >> selectModifier --> cons[token].
  selectModifierRule >> .distinct --> sparqlKw("distinct").
  selectModifierRule >> .reduced --> sparqlKw("reduced").

  optSelectModifier:(option[selectModifier]) => selectModifier.
  optSelectModifier(.none) => .noModifier.
  optSelectModifier(.some(M)) => M.

  selectVars:() >> projection --> cons[token].
  selectVars >> .selectAll --> punc("*").
  selectVars >> .vars([V0,..Vs]) --> selectVarRule >> V0, selectVarRule* >> Vs.

  selectVarRule:() >> selectVar --> cons[token].
  selectVarRule >> .plain(V) --> varRef >> V.
  selectVarRule >> .computed(E,V) --> punc("("), expression >> E, sparqlKw("as"), varRef >> V, punc(")").

  constructQuery:() >> query --> cons[token].
  constructQuery >> Q --> sparqlKw("construct"), constructBody >> Q.

  /* The two ConstructQuery forms: an explicit template with its own WHERE
     pattern, or the CONSTRUCT WHERE shorthand where the (single) template
     doubles as the WHERE pattern too -- T is bound once by constructTemplate
     and simply referenced twice in the output, the same way collectionToPattern
     (section 3) reuses a single binding in two positions of its result. */
  constructBody:() >> query --> cons[token].
  constructBody >> .construct(T,DCs,WP,Mods) --> constructTemplate >> T, datasetClauseRule* >> DCs,
    whereClause >> WP, solutionModifier >> Mods.
  constructBody >> .construct(T,DCs,T,Mods) --> datasetClauseRule* >> DCs, sparqlKw("where"),
    constructTemplate >> T, solutionModifier >> Mods.

  describeQuery:() >> query --> cons[token].
  describeQuery >> .describe(DT,DCs,WP,Mods) --> sparqlKw("describe"), describeTargetsRule >> DT,
    datasetClauseRule* >> DCs, ? whereClause >> WP, solutionModifier >> Mods.

  describeTargetsRule:() >> describeTargets --> cons[token].
  describeTargetsRule >> .allDescribed --> punc("*").
  describeTargetsRule >> .described([V0,..Vs]) --> varOrIri >> V0, varOrIri* >> Vs.

  askQuery:() >> query --> cons[token].
  askQuery >> .ask(DCs,P,Mods) --> sparqlKw("ask"), datasetClauseRule* >> DCs, whereClause >> P, solutionModifier >> Mods.

  -- Dataset clause

  datasetClauseRule:() >> datasetClause --> cons[token].
  datasetClauseRule >> D --> sparqlKw("from"), (defaultGraphClause >> D | namedGraphClause >> D).

  defaultGraphClause:() >> datasetClause --> cons[token].
  defaultGraphClause >> .defaultGraph(.literal(C)) --> sourceSelector >> C.

  namedGraphClause:() >> datasetClause --> cons[token].
  namedGraphClause >> .namedGraph(.literal(C)) --> sparqlKw("named"), sourceSelector >> C.

  sourceSelector:() >> concept --> cons[token].
  sourceSelector >> C --> iri >> C.

  -- Where clause

  whereClause:() >> pattern --> cons[token].
  whereClause >> P --> ? sparqlKw("where"), groupGraphPattern >> P.

  -- Solution modifiers

  solutionModifier:() >> solutionMods --> cons[token].
  solutionModifier >> buildSolutionMods(G,H,O,LO) --> ? groupClause >> G, ? havingClause >> H,
    ? orderClause >> O, ? limitOffsetClauses >> LO.

  buildSolutionMods:(option[cons[groupCondition]],option[cons[expression]],
    option[cons[orderCondition]],option[(option[integer],option[integer])]) => solutionMods.
  buildSolutionMods(G,H,O,LO) => solutionMods{
    grouping = optListOr([],G).
    having = optListOr([],H).
    ordering = optListOr([],O).
    limit = optPairFst(LO).
    offset = optPairSnd(LO).
  }.

  optListOr:all t ~~ (cons[t],option[cons[t]]) => cons[t].
  optListOr(D,.none) => D.
  optListOr(_,.some(L)) => L.

  optPairFst:(option[(option[integer],option[integer])]) => option[integer].
  optPairFst(.none) => .none.
  optPairFst(.some((L,_))) => L.

  optPairSnd:(option[(option[integer],option[integer])]) => option[integer].
  optPairSnd(.none) => .none.
  optPairSnd(.some((_,O))) => O.

  groupClause:() >> cons[groupCondition] --> cons[token].
  groupClause >> [G0,..Gs] --> sparqlKw("group"), sparqlKw("by"), groupConditionRule >> G0, groupConditionRule* >> Gs.

  groupConditionRule:() >> groupCondition --> cons[token].
  groupConditionRule >> .groupExpr(E,.none) --> builtInCall >> E.
  groupConditionRule >> .groupExpr(E,.none) --> functionCall >> E.
  groupConditionRule >> .groupExpr(E,Alias) --> punc("("), expression >> E, ? (sparqlKw("as"), varRef) >> Alias, punc(")").
  groupConditionRule >> .groupExpr(.term(.var(V)),.none) --> varRef >> V.

  havingClause:() >> cons[expression] --> cons[token].
  havingClause >> [H0,..Hs] --> sparqlKw("having"), havingCondition >> H0, havingCondition* >> Hs.

  havingCondition:() >> expression --> cons[token].
  havingCondition >> E --> constraint >> E.

  orderClause:() >> cons[orderCondition] --> cons[token].
  orderClause >> [O0,..Os] --> sparqlKw("order"), sparqlKw("by"), orderConditionRule >> O0, orderConditionRule* >> Os.

  orderConditionRule:() >> orderCondition --> cons[token].
  orderConditionRule >> .asc(E) --> sparqlKw("asc"), brackettedExpression >> E.
  orderConditionRule >> .desc(E) --> sparqlKw("desc"), brackettedExpression >> E.
  orderConditionRule >> .asc(E) --> constraint >> E.
  orderConditionRule >> .asc(.term(.var(V))) --> varRef >> V.

  limitOffsetClauses:() >> (option[integer],option[integer]) --> cons[token].
  limitOffsetClauses >> (.some(L),O) --> limitClause >> L, ? offsetClause >> O.
  limitOffsetClauses >> (L,.some(O)) --> offsetClause >> O, ? limitClause >> L.

  limitClause:() >> integer --> cons[token].
  limitClause >> N --> sparqlKw("limit"), integerToken >> N.

  offsetClause:() >> integer --> cons[token].
  offsetClause >> N --> sparqlKw("offset"), integerToken >> N.

  -- Values clause

  valuesClause:() >> option[dataBlock] --> cons[token].
  valuesClause >> DB --> ? (sparqlKw("values"), dataBlockRule) >> DB.

  /* Section 2: graph patterns. The following are used here but defined in
     later sections: triplesSameSubjectPath, tripleTermData (section 3);
     expression, brackettedExpression, builtInCall, functionCall (sections 4/5);
     rdfLiteral, numericLiteral, booleanLiteral (section 6).

     OPTIONAL and MINUS are algebra combinators that take the pattern
     accumulated *so far* in the enclosing groupGraphPatternSub as their left
     operand (query.pattern's .optional/.minus are both (pattern,pattern), not
     something optionalGraphPattern/minusGraphPattern could produce alone) --
     unlike UNION, FILTER, BIND etc., which just conjoin with whatever precedes
     them. graphPatternNotTriples therefore returns a `combinator` marking
     which kind of combination this item needs, and groupGraphPatternSub's fold
     applies the right one at each step. dataBlock is renamed dataBlockRule
     here for the same reason as section 1's *Rule renames. */

  combinator ::= .plainPat(pattern) | .optWrap(pattern) | .minusWrap(pattern).

  combineItem:(pattern,combinator) => pattern.
  combineItem(Acc,.plainPat(P)) => conjPattern2(Acc,P).
  combineItem(Acc,.optWrap(P)) => .optional(Acc,P).
  combineItem(Acc,.minusWrap(P)) => .minus(Acc,P).

  groupGraphPattern:() >> pattern --> cons[token].
  groupGraphPattern >> .subSelect(Q) --> punc("{"), subSelectClause >> Q, punc("}").
  groupGraphPattern >> P --> punc("{"), groupGraphPatternSub >> P, punc("}").

  groupGraphPatternSub:() >> pattern --> cons[token].
  groupGraphPatternSub >> foldGgps(P0,Items) --> ? triplesBlock >> TB0, ggpsItem* >> Items,
    {P0 .= optPatOr(.nilPattern,TB0)}.

  foldGgps:(pattern,cons[(combinator,option[pattern])]) => pattern.
  foldGgps(P,[]) => P.
  foldGgps(P,[(C,TBOpt),..Rest]) => foldGgps(conjPattern(combineItem(P,C),TBOpt),Rest).

  ggpsItem:() >> (combinator,option[pattern]) --> cons[token].
  ggpsItem >> (C,TBOpt) --> graphPatternNotTriples >> C, ? punc("."), ? triplesBlock >> TBOpt.

  optPatOr:(pattern,option[pattern]) => pattern.
  optPatOr(D,.none) => D.
  optPatOr(_,.some(P)) => P.

  triplesBlock:() >> pattern --> cons[token].
  triplesBlock >> conjPattern(T,flattenOptOpt(Rest)) --> triplesSameSubjectPath >> T, ? (punc("."), ? triplesBlock) >> Rest.

  flattenOptOpt:(option[option[pattern]]) => option[pattern].
  flattenOptOpt(.none) => .none.
  flattenOptOpt(.some(.none)) => .none.
  flattenOptOpt(.some(.some(P))) => .some(P).

  graphPatternNotTriples:() >> combinator --> cons[token].
  graphPatternNotTriples >> .plainPat(P) --> groupOrUnionGraphPattern >> P.
  graphPatternNotTriples >> .optWrap(P) --> optionalGraphPattern >> P.
  graphPatternNotTriples >> .minusWrap(P) --> minusGraphPattern >> P.
  graphPatternNotTriples >> .plainPat(P) --> graphGraphPattern >> P.
  graphPatternNotTriples >> .plainPat(P) --> serviceGraphPattern >> P.
  graphPatternNotTriples >> .plainPat(P) --> filterClause >> P.
  graphPatternNotTriples >> .plainPat(P) --> bindClause >> P.
  graphPatternNotTriples >> .plainPat(P) --> inlineData >> P.

  groupOrUnionGraphPattern:() >> pattern --> cons[token].
  groupOrUnionGraphPattern >> foldUnion(P0,Ps) --> groupGraphPattern >> P0, (sparqlKw("union"), groupGraphPattern)* >> Ps.

  foldUnion:(pattern,cons[pattern]) => pattern.
  foldUnion(P,[]) => P.
  foldUnion(P,[P1,..Rest]) => foldUnion(.union(P,P1),Rest).

  optionalGraphPattern:() >> pattern --> cons[token].
  optionalGraphPattern >> P --> sparqlKw("optional"), groupGraphPattern >> P.

  minusGraphPattern:() >> pattern --> cons[token].
  minusGraphPattern >> P --> sparqlKw("minus"), groupGraphPattern >> P.

  graphGraphPattern:() >> pattern --> cons[token].
  graphGraphPattern >> .graph(T,P) --> sparqlKw("graph"), varOrIri >> T, groupGraphPattern >> P.

  serviceGraphPattern:() >> pattern --> cons[token].
  serviceGraphPattern >> .service(T,P,Silent) --> sparqlKw("service"), kwFlag("silent") >> Silent,
    varOrIri >> T, groupGraphPattern >> P.

  bindClause:() >> pattern --> cons[token].
  bindClause >> .bind(E,V) --> sparqlKw("bind"), punc("("), expression >> E, sparqlKw("as"), varRef >> V, punc(")").

  inlineData:() >> pattern --> cons[token].
  inlineData >> .values(D) --> sparqlKw("values"), dataBlockRule >> D.

  dataBlockRule:() >> dataBlock --> cons[token].
  dataBlockRule >> D --> inlineDataOneVar >> D.
  dataBlockRule >> D --> inlineDataFull >> D.

  inlineDataOneVar:() >> dataBlock --> cons[token].
  inlineDataOneVar >> .oneVar(V,Vals) --> varRef >> V, punc("{"), dataBlockValue* >> Vals, punc("}").

  inlineDataFull:() >> dataBlock --> cons[token].
  inlineDataFull >> .full(Vars,Rows) -->
    (rdfNilVars >> Vars | punc("("), varRef* >> Vars, punc(")")),
    punc("{"), (dataRowParen | rdfNilRow)* >> Rows, punc("}").

  rdfNilVars:() >> cons[string] --> cons[token].
  rdfNilVars >> [] --> rdfNil.

  dataRowParen:() >> cons[option[term]] --> cons[token].
  dataRowParen >> R --> punc("("), dataBlockValue* >> R, punc(")").

  rdfNilRow:() >> cons[option[term]] --> cons[token].
  rdfNilRow >> [] --> rdfNil.

  dataBlockValue:() >> option[term] --> cons[token].
  dataBlockValue >> .some(.literal(C)) --> iri >> C.
  dataBlockValue >> .some(.literal(C)) --> rdfLiteral >> C.
  dataBlockValue >> .some(.literal(C)) --> numericLiteral >> C.
  dataBlockValue >> .some(.literal(C)) --> booleanLiteral >> C.
  dataBlockValue >> .none --> sparqlKw("undef").
  dataBlockValue >> .some(.literal(C)) --> tripleTermData >> C.

  rdfNil --> punc("("), punc(")").

  filterClause:() >> pattern --> cons[token].
  filterClause >> .filter(E) --> sparqlKw("filter"), constraint >> E.

  constraint:() >> expression --> cons[token].
  constraint >> E --> brackettedExpression >> E.
  constraint >> E --> builtInCall >> E.
  constraint >> E --> functionCall >> E.

  /* Shared by DISTINCT/REDUCED/SILENT-style optional keywords across sections
     1, 2 and 5: `? sparqlKw(Kw)` alone can't produce a boolean since sparqlKw
     itself has no output to wrap -- this gives the presence/absence check its
     own boolean-producing rule instead. */
  kwFlag:(string) >> boolean --> cons[token].
  kwFlag(Kw) >> .true --> sparqlKw(Kw).
  kwFlag(Kw) >> .false --> [].

  /* Section 3: triples and property paths, including RDF-star reified
     triples/triple terms and annotations. The following are used here but
     defined in later sections: rdfLiteral, numericLiteral, booleanLiteral
     (section 6).

     propertyList(Path)(NotEmpty) build a plain list of (predicate,objects)
     pairs rather than a pattern directly, since they don't know the subject
     -- it's supplied by the caller (triplesSameSubject(Path)), which combines
     subject+pairs into the actual conjoined .basic triples via
     propertyPairsToPattern. Each object carries the extra `pattern` from a
     nested collection/blank node property list used as that object (e.g.
     `?s ex:p (1 2 3)`), which must still be conjoined into the result. */
  public propertyPairs ~> cons[(predicate,cons[(term,pattern,cons[annotationItem])])].

  constructTemplate:() >> pattern --> cons[token].
  constructTemplate >> P --> punc("{"), constructTriplesOpt >> P, punc("}").

  constructTriplesOpt:() >> pattern --> cons[token].
  constructTriplesOpt >> P --> constructTriples >> P.
  constructTriplesOpt >> .nilPattern --> [].

  constructTriples:() >> pattern --> cons[token].
  constructTriples >> conjPattern(T,More) --> triplesSameSubject >> T,
    ? (punc("."), constructTriplesOpt) >> More.

  triplesSameSubject:() >> pattern --> cons[token].
  triplesSameSubject >> propertyPairsToPattern(S,PVs) --> varOrTerm >> S, propertyListNotEmpty >> PVs.
  triplesSameSubject >> conjPattern2(Internal,propertyPairsToPattern(Anchor,PVs)) -->
    triplesNode >> (Anchor,Internal), propertyList >> PVs.
  triplesSameSubject >> T --> reifiedTripleBlock >> T.

  reifiedTripleBlock:() >> pattern --> cons[token].
  reifiedTripleBlock >> propertyPairsToPattern(T,PVs) --> reifiedTripleExpr >> T, propertyList >> PVs.

  propertyList:() >> propertyPairs --> cons[token].
  propertyList >> PVs --> propertyListNotEmpty >> PVs.
  propertyList >> [] --> [].

  propertyListNotEmpty:() >> propertyPairs --> cons[token].
  propertyListNotEmpty >> [(V0,OL0),..filterSome(VOs)] --> verb >> V0, objectList >> OL0,
    (punc(";"), ? verbObjectPair)* >> VOs.

  /* A bare sequence's `>>` value is only its *last* term (per grammar.adoc),
     so `(verb, objectList)` alone would only capture objectList, silently
     dropping the verb -- this explicit pair rule avoids that. */
  verbObjectPair:() >> (predicate,cons[(term,pattern,cons[annotationItem])]) --> cons[token].
  verbObjectPair >> (V,OL) --> verb >> V, objectList >> OL.

  verb:() >> predicate --> cons[token].
  verb >> P --> varOrIri >> T, {P .= .simple(T)}.
  verb >> .simple(.literal(rdfTypeConcept())) --> sparqlKw("a").

  objectList:() >> cons[(term,pattern,cons[annotationItem])] --> cons[token].
  objectList >> [O0,..Os] --> object >> O0, (punc(","), object)* >> Os.

  /* The middle `pattern` carries structure from a nested collection/blank
     node property list used directly as an object (e.g. `?s ex:p (1 2 3)`)
     -- discarding it would silently drop that structure's own triples. */
  object:() >> (term,pattern,cons[annotationItem]) --> cons[token].
  object >> (T,Extra,As) --> graphNode >> (T,Extra), annotation >> As.

  triplesSameSubjectPath:() >> pattern --> cons[token].
  triplesSameSubjectPath >> propertyPairsToPattern(S,PVs) --> varOrTerm >> S, propertyListPathNotEmpty >> PVs.
  triplesSameSubjectPath >> conjPattern2(Internal,propertyPairsToPattern(Anchor,PVs)) -->
    triplesNodePath >> (Anchor,Internal), propertyListPath >> PVs.
  triplesSameSubjectPath >> T --> reifiedTripleBlockPath >> T.

  reifiedTripleBlockPath:() >> pattern --> cons[token].
  reifiedTripleBlockPath >> propertyPairsToPattern(T,PVs) --> reifiedTripleExpr >> T, propertyListPath >> PVs.

  propertyListPath:() >> propertyPairs --> cons[token].
  propertyListPath >> PVs --> propertyListPathNotEmpty >> PVs.
  propertyListPath >> [] --> [].

  propertyListPathNotEmpty:() >> propertyPairs --> cons[token].
  propertyListPathNotEmpty >> [(V0,OL0),..filterSome(VOs)] --> (verbPath | verbSimple) >> V0, objectListPath >> OL0,
    (punc(";"), ? verbObjectPairPath)* >> VOs.

  verbObjectPairPath:() >> (predicate,cons[(term,pattern,cons[annotationItem])]) --> cons[token].
  verbObjectPairPath >> (V,OL) --> (verbPath | verbSimple) >> V, objectListPath >> OL.

  verbPath:() >> predicate --> cons[token].
  verbPath >> .path(P) --> pathExpr >> P.

  verbSimple:() >> predicate --> cons[token].
  verbSimple >> .simple(.var(V)) --> varRef >> V.

  objectListPath:() >> cons[(term,pattern,cons[annotationItem])] --> cons[token].
  objectListPath >> [O0,..Os] --> objectPath >> O0, (punc(","), objectPath)* >> Os.

  objectPath:() >> (term,pattern,cons[annotationItem]) --> cons[token].
  objectPath >> (T,Extra,As) --> graphNodePath >> (T,Extra), annotationPath >> As.

  pathExpr:() >> path --> cons[token].
  pathExpr >> P --> pathAlternative >> P.

  pathAlternative:() >> path --> cons[token].
  pathAlternative >> foldPathAlt(P0,Ps) --> pathSequence >> P0, (punc("|"), pathSequence)* >> Ps.

  pathSequence:() >> path --> cons[token].
  pathSequence >> foldPathSeq(P0,Ps) --> pathEltOrInverse >> P0, (punc("/"), pathEltOrInverse)* >> Ps.

  pathElt:() >> path --> cons[token].
  pathElt >> applyPathMod(P,M) --> pathPrimary >> P, ? pathMod >> M.

  pathEltOrInverse:() >> path --> cons[token].
  pathEltOrInverse >> P --> pathElt >> P.
  pathEltOrInverse >> .inverse(P) --> punc("^"), pathElt >> P.

  /* The `?` (zero-or-one) modifier is followed here by a negative lookahead:
     since a variable is lexed as two tokens (`punc("?")` then an identifier,
     see `varRef` below), a bare `punc("?")` immediately followed by an
     identifier token is the start of the *next* variable, not a path modifier. */
  pathMod:() >> pathMod --> cons[token].
  pathMod >> .pOptional --> punc("?"), ~ [.tok(_,.idTok(_))].
  pathMod >> .pStar --> punc("*").
  pathMod >> .pPlus --> punc("+").

  pathPrimary:() >> path --> cons[token].
  pathPrimary >> .predicate(.literal(C)) --> iri >> C.
  pathPrimary >> .predicate(.literal(rdfTypeConcept())) --> sparqlKw("a").
  pathPrimary >> .negated(Ps) --> punc("!"), pathNegatedPropertySet >> Ps.
  pathPrimary >> .group(P) --> punc("("), pathExpr >> P, punc(")").

  pathNegatedPropertySet:() >> cons[negatedPathItem] --> cons[token].
  pathNegatedPropertySet >> [I] --> pathOneInPropertySet >> I.
  pathNegatedPropertySet >> [] --> punc("("), punc(")").
  pathNegatedPropertySet >> [I0,..Is] --> punc("("), pathOneInPropertySet >> I0,
    (punc("|"), pathOneInPropertySet)* >> Is, punc(")").

  pathOneInPropertySet:() >> negatedPathItem --> cons[token].
  pathOneInPropertySet >> .fwd(.literal(C)) --> iri >> C.
  pathOneInPropertySet >> .fwd(.literal(rdfTypeConcept())) --> sparqlKw("a").
  pathOneInPropertySet >> .inv(.literal(C)) --> punc("^"), iri >> C.
  pathOneInPropertySet >> .inv(.literal(rdfTypeConcept())) --> punc("^"), sparqlKw("a").

  triplesNode:() >> (term,pattern) --> cons[token].
  triplesNode >> TP --> collection >> TP.
  triplesNode >> TP --> blankNodePropertyList >> TP.

  blankNodePropertyList:() >> (term,pattern) --> cons[token].
  blankNodePropertyList >> (Anchor,propertyPairsToPattern(Anchor,PVs)) -->
    punc("["), {Anchor .= .literal(genAnon())}, propertyListNotEmpty >> PVs, punc("]").

  triplesNodePath:() >> (term,pattern) --> cons[token].
  triplesNodePath >> TP --> collectionPath >> TP.
  triplesNodePath >> TP --> blankNodePropertyListPath >> TP.

  blankNodePropertyListPath:() >> (term,pattern) --> cons[token].
  blankNodePropertyListPath >> (Anchor,propertyPairsToPattern(Anchor,PVs)) -->
    punc("["), {Anchor .= .literal(genAnon())}, propertyListPathNotEmpty >> PVs, punc("]").

  collection:() >> (term,pattern) --> cons[token].
  collection >> collectionToPattern([E0,..Es]) --> punc("("), graphNode >> E0, graphNode* >> Es, punc(")").

  collectionPath:() >> (term,pattern) --> cons[token].
  collectionPath >> collectionToPattern([E0,..Es]) --> punc("("), graphNodePath >> E0, graphNodePath* >> Es, punc(")").

  graphNode:() >> (term,pattern) --> cons[token].
  graphNode >> (T,.nilPattern) --> varOrTerm >> T.
  graphNode >> TP --> triplesNode >> TP.
  graphNode >> (T,.nilPattern) --> reifiedTripleExpr >> T.

  graphNodePath:() >> (term,pattern) --> cons[token].
  graphNodePath >> (T,.nilPattern) --> varOrTerm >> T.
  graphNodePath >> TP --> triplesNodePath >> TP.
  graphNodePath >> (T,.nilPattern) --> reifiedTripleExpr >> T.

  varOrTerm:() >> term --> cons[token].
  varOrTerm >> .var(V) --> varRef >> V.
  varOrTerm >> .literal(C) --> iri >> C.
  varOrTerm >> .literal(C) --> rdfLiteral >> C.
  varOrTerm >> .literal(C) --> numericLiteral >> C.
  varOrTerm >> .literal(C) --> booleanLiteral >> C.
  varOrTerm >> .literal(C) --> blankNode >> C.
  varOrTerm >> .literal(.uri(rdfNilUri())) --> rdfNil.
  varOrTerm >> T --> tripleTermExpr >> T.

  -- RDF-star: reified triples, triple terms, reifiers and annotations

  reifiedTripleExpr:() >> term --> cons[token].
  reifiedTripleExpr >> .reifiedTriple(S,P,O,R) -->
    punc("<<"), reifiedTripleSubject >> S, verb >> P, reifiedTripleObject >> O, ? reifierClause >> AI, punc(">>"),
    {R .= annotationItemToReifierId(AI)}.

  reifiedTripleSubject:() >> term --> cons[token].
  reifiedTripleSubject >> .var(V) --> varRef >> V.
  reifiedTripleSubject >> .literal(C) --> iri >> C.
  reifiedTripleSubject >> .literal(C) --> rdfLiteral >> C.
  reifiedTripleSubject >> .literal(C) --> numericLiteral >> C.
  reifiedTripleSubject >> .literal(C) --> booleanLiteral >> C.
  reifiedTripleSubject >> .literal(C) --> blankNode >> C.
  reifiedTripleSubject >> T --> reifiedTripleExpr >> T.
  reifiedTripleSubject >> T --> tripleTermExpr >> T.

  reifiedTripleObject:() >> term --> cons[token].
  reifiedTripleObject >> .var(V) --> varRef >> V.
  reifiedTripleObject >> .literal(C) --> iri >> C.
  reifiedTripleObject >> .literal(C) --> rdfLiteral >> C.
  reifiedTripleObject >> .literal(C) --> numericLiteral >> C.
  reifiedTripleObject >> .literal(C) --> booleanLiteral >> C.
  reifiedTripleObject >> .literal(C) --> blankNode >> C.
  reifiedTripleObject >> T --> reifiedTripleExpr >> T.
  reifiedTripleObject >> T --> tripleTermExpr >> T.

  tripleTermExpr:() >> term --> cons[token].
  tripleTermExpr >> .tripleTermPattern(S,P,O) -->
    punc("<<"), punc("("), tripleTermSubject >> S, verb >> P, tripleTermObject >> O, punc(")"), punc(">>").

  tripleTermSubject:() >> term --> cons[token].
  tripleTermSubject >> .var(V) --> varRef >> V.
  tripleTermSubject >> .literal(C) --> iri >> C.
  tripleTermSubject >> .literal(C) --> rdfLiteral >> C.
  tripleTermSubject >> .literal(C) --> numericLiteral >> C.
  tripleTermSubject >> .literal(C) --> booleanLiteral >> C.
  tripleTermSubject >> .literal(C) --> blankNode >> C.
  tripleTermSubject >> T --> tripleTermExpr >> T.

  tripleTermObject:() >> term --> cons[token].
  tripleTermObject >> .var(V) --> varRef >> V.
  tripleTermObject >> .literal(C) --> iri >> C.
  tripleTermObject >> .literal(C) --> rdfLiteral >> C.
  tripleTermObject >> .literal(C) --> numericLiteral >> C.
  tripleTermObject >> .literal(C) --> booleanLiteral >> C.
  tripleTermObject >> .literal(C) --> blankNode >> C.
  tripleTermObject >> T --> tripleTermExpr >> T.

  /* Fully-ground triple term, as used in VALUES data blocks: unlike
     tripleTerm above, this can't hold a variable, so it produces a plain
     rdf.triple.concept (reusing .tripleTerm(triple) there) rather than a
     query.term. */
  tripleTermData:() >> concept --> cons[token].
  tripleTermData >> .tripleTerm(.tr(S,P,O)) --> punc("<<"), punc("("), tripleTermDataSubject >> S,
    (iri >> P | sparqlKw("a"), {P .= rdfTypeConcept()}), tripleTermDataObject >> O, punc(")"), punc(">>").

  tripleTermDataSubject:() >> concept --> cons[token].
  tripleTermDataSubject >> C --> iri >> C.

  tripleTermDataObject:() >> concept --> cons[token].
  tripleTermDataObject >> C --> iri >> C.
  tripleTermDataObject >> C --> rdfLiteral >> C.
  tripleTermDataObject >> C --> numericLiteral >> C.
  tripleTermDataObject >> C --> booleanLiteral >> C.
  tripleTermDataObject >> .tripleTerm(.tr(S,P,O)) --> tripleTermData >> .tripleTerm(.tr(S,P,O)).

  reifierClause:() >> option[term] --> cons[token].
  reifierClause >> Id --> punc("~"), ? varOrReifierId >> Id.

  varOrReifierId:() >> term --> cons[token].
  varOrReifierId >> .var(V) --> varRef >> V.
  varOrReifierId >> .literal(C) --> iri >> C.
  varOrReifierId >> .literal(C) --> blankNode >> C.

  annotationPath:() >> cons[annotationItem] --> cons[token].
  annotationPath >> Is --> annotationPathEntry* >> Is.

  annotationPathEntry:() >> annotationItem --> cons[token].
  annotationPathEntry >> .reifier(R) --> reifierClause >> R.
  annotationPathEntry >> I --> annotationBlockPath >> I.

  annotationBlockPath:() >> annotationItem --> cons[token].
  annotationBlockPath >> .annotationBlock(propertyPairsToPattern(anonSubject(),PVs)) -->
    punc("{"), punc("|"), propertyListPathNotEmpty >> PVs, punc("|"), punc("}").

  annotation:() >> cons[annotationItem] --> cons[token].
  annotation >> Is --> annotationEntry* >> Is.

  annotationEntry:() >> annotationItem --> cons[token].
  annotationEntry >> .reifier(R) --> reifierClause >> R.
  annotationEntry >> I --> annotationBlockRule >> I.

  annotationBlockRule:() >> annotationItem --> cons[token].
  annotationBlockRule >> .annotationBlock(propertyPairsToPattern(anonSubject(),PVs)) -->
    punc("{"), punc("|"), propertyListNotEmpty >> PVs, punc("|"), punc("}").

  /* Section 4: expressions. The following are used here but defined in later
     sections: iriOrFunction, expressionList, builtInCall (section 5);
     rdfLiteral, numericLiteral, numericLiteralPositive, numericLiteralNegative,
     booleanLiteral (section 6, already typed).

     `&&`, `||` and `!=` compose from adjacent single-char tokens (`&`,`&`;
     `|`,`|`; `!`,`=`) already produced by rdf.lexer, the same way `{|`/`|}`
     did in section 3 -- no further lexer changes needed for this section. */

  expression:() >> expression --> cons[token].
  expression >> E --> conditionalOrExpression >> E.

  conditionalOrExpression:() >> expression --> cons[token].
  conditionalOrExpression >> foldOr(E0,Es) --> conditionalAndExpression >> E0,
    (punc("|"), punc("|"), conditionalAndExpression)* >> Es.

  foldOr:(expression,cons[expression]) => expression.
  foldOr(E,[]) => E.
  foldOr(E,[E1,..Rest]) => foldOr(.or(E,E1),Rest).

  conditionalAndExpression:() >> expression --> cons[token].
  conditionalAndExpression >> foldAnd(E0,Es) --> valueLogical >> E0, (punc("&"), punc("&"), valueLogical)* >> Es.

  foldAnd:(expression,cons[expression]) => expression.
  foldAnd(E,[]) => E.
  foldAnd(E,[E1,..Rest]) => foldAnd(.and(E,E1),Rest).

  valueLogical:() >> expression --> cons[token].
  valueLogical >> E --> relationalExpression >> E.

  /* The eight relational-suffix alternatives share a single optional slot:
     relOp records which one (if any) matched, and applyRelOp turns that plus
     the LHS into the actual comparison/membership expression -- the same
     "parse a marker, apply it afterward" idiom applyPathMod (section 3) uses. */
  relOp ::= .cmpEq(expression) | .cmpNe(expression) | .cmpLt(expression) | .cmpGt(expression)
    | .cmpLe(expression) | .cmpGe(expression) | .cmpIn(cons[expression]) | .cmpNotIn(cons[expression]).

  relationalExpression:() >> expression --> cons[token].
  relationalExpression >> applyRelOp(L,Op) --> numericExpression >> L, ? relOpSuffix >> Op.

  relOpSuffix:() >> relOp --> cons[token].
  relOpSuffix >> .cmpEq(R) --> punc("="), numericExpression >> R.
  relOpSuffix >> .cmpNe(R) --> punc("!"), punc("="), numericExpression >> R.
  relOpSuffix >> .cmpLe(R) --> punc("<="), numericExpression >> R.
  relOpSuffix >> .cmpGe(R) --> punc(">="), numericExpression >> R.
  relOpSuffix >> .cmpLt(R) --> punc("<"), numericExpression >> R.
  relOpSuffix >> .cmpGt(R) --> punc(">"), numericExpression >> R.
  relOpSuffix >> .cmpIn(Es) --> sparqlKw("in"), expressionList >> Es.
  relOpSuffix >> .cmpNotIn(Es) --> sparqlKw("not"), sparqlKw("in"), expressionList >> Es.

  applyRelOp:(expression,option[relOp]) => expression.
  applyRelOp(L,.none) => L.
  applyRelOp(L,.some(.cmpEq(R))) => .eq(L,R).
  applyRelOp(L,.some(.cmpNe(R))) => .ne(L,R).
  applyRelOp(L,.some(.cmpLt(R))) => .lt(L,R).
  applyRelOp(L,.some(.cmpGt(R))) => .gt(L,R).
  applyRelOp(L,.some(.cmpLe(R))) => .le(L,R).
  applyRelOp(L,.some(.cmpGe(R))) => .ge(L,R).
  applyRelOp(L,.some(.cmpIn(Es))) => .isIn(L,Es).
  applyRelOp(L,.some(.cmpNotIn(Es))) => .notIn(L,Es).

  numericExpression:() >> expression --> cons[token].
  numericExpression >> E --> additiveExpression >> E.

  /* AdditiveExpression's three tail shapes (+ term, - term, or a directly
     juxtaposed signed literal that can itself carry trailing '*' or '/'
     factors, e.g. `2 -3*4`) all fold left-associatively into the running
     total, so they share one marker type the same way relOp does above. */
  addOp ::= .addTail(expression) | .subTail(expression).

  additiveExpression:() >> expression --> cons[token].
  additiveExpression >> foldAdditive(E0,Ops) --> multiplicativeExpression >> E0, additiveOp* >> Ops.

  additiveOp:() >> addOp --> cons[token].
  additiveOp >> .addTail(R) --> punc("+"), multiplicativeExpression >> R.
  additiveOp >> .subTail(R) --> punc("-"), multiplicativeExpression >> R.
  additiveOp >> .addTail(R) --> (numericLiteralPositive >> N | numericLiteralNegative >> N),
    signedFactorTail(.term(.literal(N))) >> R.

  foldAdditive:(expression,cons[addOp]) => expression.
  foldAdditive(E,[]) => E.
  foldAdditive(E,[.addTail(R),..Rest]) => foldAdditive(.add(E,R),Rest).
  foldAdditive(E,[.subTail(R),..Rest]) => foldAdditive(.sub(E,R),Rest).

  /* Shared by additiveOp's signed-literal case and multiplicativeExpression
     itself (both are "seed expression, then repeated '*'/'/' factors"). */
  mulOp ::= .mulTail(expression) | .divTail(expression).

  signedFactorTail:(expression) >> expression --> cons[token].
  signedFactorTail(E0) >> foldMultiplicative(E0,Ms) --> mulDivOp* >> Ms.

  mulDivOp:() >> mulOp --> cons[token].
  mulDivOp >> .mulTail(R) --> punc("*"), unaryExpression >> R.
  mulDivOp >> .divTail(R) --> punc("/"), unaryExpression >> R.

  foldMultiplicative:(expression,cons[mulOp]) => expression.
  foldMultiplicative(E,[]) => E.
  foldMultiplicative(E,[.mulTail(R),..Rest]) => foldMultiplicative(.mul(E,R),Rest).
  foldMultiplicative(E,[.divTail(R),..Rest]) => foldMultiplicative(.div(E,R),Rest).

  multiplicativeExpression:() >> expression --> cons[token].
  multiplicativeExpression >> R --> unaryExpression >> E0, signedFactorTail(E0) >> R.

  unaryExpression:() >> expression --> cons[token].
  unaryExpression >> .not(E) --> punc("!"), unaryExpression >> E.
  unaryExpression >> .pos(E) --> punc("+"), primaryExpression >> E.
  unaryExpression >> .neg(E) --> punc("-"), primaryExpression >> E.
  unaryExpression >> E --> primaryExpression >> E.

  primaryExpression:() >> expression --> cons[token].
  primaryExpression >> E --> brackettedExpression >> E.
  primaryExpression >> E --> builtInCall >> E.
  primaryExpression >> E --> iriOrFunction >> E.
  primaryExpression >> .term(.literal(C)) --> rdfLiteral >> C.
  primaryExpression >> .term(.literal(C)) --> numericLiteral >> C.
  primaryExpression >> .term(.literal(C)) --> booleanLiteral >> C.
  primaryExpression >> .term(.var(V)) --> varRef >> V.
  primaryExpression >> E --> exprTripleTermRule >> E.

  /* verb (section 3) always yields .simple(T) here (SPARQL's Verb production
     is VarOrIri | 'a', never a property path), so predicateToExpr's .path arm
     is unreachable -- kept only for exhaustiveness. */
  exprTripleTermRule:() >> expression --> cons[token].
  exprTripleTermRule >> .exprTripleTerm(S,predicateToExpr(P),O) -->
    punc("<<"), punc("("), exprTripleTermSubject >> S, verb >> P, exprTripleTermObject >> O, punc(")"), punc(">>").

  predicateToExpr(.simple(T)) => .term(T).
  predicateToExpr(.path(_)) => unreachable.

  exprTripleTermSubject:() >> expression --> cons[token].
  exprTripleTermSubject >> .term(.literal(C)) --> iri >> C.
  exprTripleTermSubject >> .term(.var(V)) --> varRef >> V.

  exprTripleTermObject:() >> expression --> cons[token].
  exprTripleTermObject >> .term(.literal(C)) --> iri >> C.
  exprTripleTermObject >> .term(.literal(C)) --> rdfLiteral >> C.
  exprTripleTermObject >> .term(.literal(C)) --> numericLiteral >> C.
  exprTripleTermObject >> .term(.literal(C)) --> booleanLiteral >> C.
  exprTripleTermObject >> .term(.var(V)) --> varRef >> V.
  exprTripleTermObject >> E --> exprTripleTermRule >> E.

  brackettedExpression:() >> expression --> cons[token].
  brackettedExpression >> E --> punc("("), expression >> E, punc(")").

  /* Section 5: built-ins, aggregates, and function calls. The following are
     used here but defined in section 6: rdfLiteral, numericLiteral,
     booleanLiteral (already typed).

     Most BuiltInCall alternatives share one of a handful of shapes (keyword
     applied to 1/2/3 expressions, or to NIL); oneArgBuiltin/twoArgBuiltin/
     threeArgBuiltin/nilArgBuiltin/aggFn1 below are parameterized over the
     keyword to avoid repeating each shape ~15-20 times. Every one collapses
     to the generic expression.call(name,args) rather than one constructor
     per built-in (see query.star's expression comment). */

  builtInCall:() >> expression --> cons[token].
  builtInCall >> E --> aggregateExpr >> E.
  builtInCall >> .call("str",[A]) --> oneArgBuiltin("str") >> A.
  builtInCall >> .call("lang",[A]) --> oneArgBuiltin("lang") >> A.
  builtInCall >> .call("langmatches",[A,B]) --> twoArgBuiltin("langmatches") >> (A,B).
  builtInCall >> .call("langdir",[A]) --> oneArgBuiltin("langdir") >> A.
  builtInCall >> .call("datatype",[A]) --> oneArgBuiltin("datatype") >> A.
  builtInCall >> .bound(V) --> sparqlKw("bound"), punc("("), varRef >> V, punc(")").
  builtInCall >> .call("iri",[A]) --> oneArgBuiltin("iri") >> A.
  builtInCall >> .call("uri",[A]) --> oneArgBuiltin("uri") >> A.
  builtInCall >> .call("bnode",Args) --> sparqlKw("bnode"), bnodeArgs >> Args.

  bnodeArgs:() >> cons[expression] --> cons[token].
  bnodeArgs >> [A] --> punc("("), expression >> A, punc(")").
  bnodeArgs >> [] --> rdfNil.
  builtInCall >> .call("rand",[]) --> nilArgBuiltin("rand").
  builtInCall >> .call("abs",[A]) --> oneArgBuiltin("abs") >> A.
  builtInCall >> .call("ceil",[A]) --> oneArgBuiltin("ceil") >> A.
  builtInCall >> .call("floor",[A]) --> oneArgBuiltin("floor") >> A.
  builtInCall >> .call("round",[A]) --> oneArgBuiltin("round") >> A.
  builtInCall >> .call("concat",Es) --> sparqlKw("concat"), expressionList >> Es.
  builtInCall >> E --> substringExpression >> E.
  builtInCall >> .call("strlen",[A]) --> oneArgBuiltin("strlen") >> A.
  builtInCall >> E --> strReplaceExpression >> E.
  builtInCall >> .call("ucase",[A]) --> oneArgBuiltin("ucase") >> A.
  builtInCall >> .call("lcase",[A]) --> oneArgBuiltin("lcase") >> A.
  builtInCall >> .call("encode_for_uri",[A]) --> oneArgBuiltin("encode_for_uri") >> A.
  builtInCall >> .call("contains",[A,B]) --> twoArgBuiltin("contains") >> (A,B).
  builtInCall >> .call("strstarts",[A,B]) --> twoArgBuiltin("strstarts") >> (A,B).
  builtInCall >> .call("strends",[A,B]) --> twoArgBuiltin("strends") >> (A,B).
  builtInCall >> .call("strbefore",[A,B]) --> twoArgBuiltin("strbefore") >> (A,B).
  builtInCall >> .call("strafter",[A,B]) --> twoArgBuiltin("strafter") >> (A,B).
  builtInCall >> .call("year",[A]) --> oneArgBuiltin("year") >> A.
  builtInCall >> .call("month",[A]) --> oneArgBuiltin("month") >> A.
  builtInCall >> .call("day",[A]) --> oneArgBuiltin("day") >> A.
  builtInCall >> .call("hours",[A]) --> oneArgBuiltin("hours") >> A.
  builtInCall >> .call("minutes",[A]) --> oneArgBuiltin("minutes") >> A.
  builtInCall >> .call("seconds",[A]) --> oneArgBuiltin("seconds") >> A.
  builtInCall >> .call("timezone",[A]) --> oneArgBuiltin("timezone") >> A.
  builtInCall >> .call("tz",[A]) --> oneArgBuiltin("tz") >> A.
  builtInCall >> .call("now",[]) --> nilArgBuiltin("now").
  builtInCall >> .call("uuid",[]) --> nilArgBuiltin("uuid").
  builtInCall >> .call("struuid",[]) --> nilArgBuiltin("struuid").
  builtInCall >> .call("md5",[A]) --> oneArgBuiltin("md5") >> A.
  builtInCall >> .call("sha1",[A]) --> oneArgBuiltin("sha1") >> A.
  builtInCall >> .call("sha256",[A]) --> oneArgBuiltin("sha256") >> A.
  builtInCall >> .call("sha384",[A]) --> oneArgBuiltin("sha384") >> A.
  builtInCall >> .call("sha512",[A]) --> oneArgBuiltin("sha512") >> A.
  builtInCall >> .call("coalesce",Es) --> sparqlKw("coalesce"), expressionList >> Es.
  builtInCall >> .call("if",[A,B,C]) --> sparqlKw("if"), punc("("), expression >> A, punc(","), expression >> B,
    punc(","), expression >> C, punc(")").
  builtInCall >> .call("strlang",[A,B]) --> twoArgBuiltin("strlang") >> (A,B).
  builtInCall >> .call("strlangdir",[A,B,C]) --> threeArgBuiltin("strlangdir") >> (A,B,C).
  builtInCall >> .call("strdt",[A,B]) --> twoArgBuiltin("strdt") >> (A,B).
  builtInCall >> .call("sameterm",[A,B]) --> twoArgBuiltin("sameterm") >> (A,B).
  builtInCall >> .call("isiri",[A]) --> oneArgBuiltin("isiri") >> A.
  builtInCall >> .call("isuri",[A]) --> oneArgBuiltin("isuri") >> A.
  builtInCall >> .call("isblank",[A]) --> oneArgBuiltin("isblank") >> A.
  builtInCall >> .call("isliteral",[A]) --> oneArgBuiltin("isliteral") >> A.
  builtInCall >> .call("isnumeric",[A]) --> oneArgBuiltin("isnumeric") >> A.
  builtInCall >> .call("haslang",[A]) --> oneArgBuiltin("haslang") >> A.
  builtInCall >> .call("haslangdir",[A]) --> oneArgBuiltin("haslangdir") >> A.
  builtInCall >> E --> regexExpression >> E.
  builtInCall >> E --> existsFunc >> E.
  builtInCall >> E --> notExistsFunc >> E.
  builtInCall >> .call("istriple",[A]) --> oneArgBuiltin("istriple") >> A.
  builtInCall >> .call("triple",[A,B,C]) --> threeArgBuiltin("triple") >> (A,B,C).
  builtInCall >> .call("subject",[A]) --> oneArgBuiltin("subject") >> A.
  builtInCall >> .call("predicate",[A]) --> oneArgBuiltin("predicate") >> A.
  builtInCall >> .call("object",[A]) --> oneArgBuiltin("object") >> A.

  oneArgBuiltin:(string) >> expression --> cons[token].
  oneArgBuiltin(Kw) >> A --> sparqlKw(Kw), punc("("), expression >> A, punc(")").

  twoArgBuiltin:(string) >> (expression,expression) --> cons[token].
  twoArgBuiltin(Kw) >> (A,B) --> sparqlKw(Kw), punc("("), expression >> A, punc(","), expression >> B, punc(")").

  threeArgBuiltin:(string) >> (expression,expression,expression) --> cons[token].
  threeArgBuiltin(Kw) >> (A,B,C) --> sparqlKw(Kw), punc("("), expression >> A, punc(","), expression >> B,
    punc(","), expression >> C, punc(")").

  nilArgBuiltin(Kw) --> sparqlKw(Kw), rdfNil.

  regexExpression:() >> expression --> cons[token].
  regexExpression >> .call("regex",twoPlusOptArgs(A,B,C)) --> sparqlKw("regex"), punc("("), expression >> A,
    punc(","), expression >> B, ? (punc(","), expression) >> C, punc(")").

  substringExpression:() >> expression --> cons[token].
  substringExpression >> .call("substr",twoPlusOptArgs(A,B,C)) --> sparqlKw("substr"), punc("("), expression >> A,
    punc(","), expression >> B, ? (punc(","), expression) >> C, punc(")").

  twoPlusOptArgs:(expression,expression,option[expression]) => cons[expression].
  twoPlusOptArgs(A,B,.none) => [A,B].
  twoPlusOptArgs(A,B,.some(C)) => [A,B,C].

  strReplaceExpression:() >> expression --> cons[token].
  strReplaceExpression >> .call("replace",replaceArgs(A,B,C,D)) --> sparqlKw("replace"), punc("("), expression >> A,
    punc(","), expression >> B, punc(","), expression >> C, ? (punc(","), expression) >> D, punc(")").

  replaceArgs:(expression,expression,expression,option[expression]) => cons[expression].
  replaceArgs(A,B,C,.none) => [A,B,C].
  replaceArgs(A,B,C,.some(D)) => [A,B,C,D].

  existsFunc:() >> expression --> cons[token].
  existsFunc >> .existsPattern(P) --> sparqlKw("exists"), groupGraphPattern >> P.

  notExistsFunc:() >> expression --> cons[token].
  notExistsFunc >> .notExists(P) --> sparqlKw("not"), sparqlKw("exists"), groupGraphPattern >> P.

  /* COUNT is its own shape (DISTINCT flag plus either `*` or an expression);
     SUM/MIN/MAX/AVG/SAMPLE share aggFn1's single-expression shape;
     GROUP_CONCAT is its own shape again (optional SEPARATOR). */
  aggregateExpr:() >> expression --> cons[token].
  aggregateExpr >> .aggregate("count",CountArg,Dist) --> sparqlKw("count"), punc("("), kwFlag("distinct") >> Dist,
    countTarget >> CountArg, punc(")").
  aggregateExpr >> E --> aggFn1("sum") >> E.
  aggregateExpr >> E --> aggFn1("min") >> E.
  aggregateExpr >> E --> aggFn1("max") >> E.
  aggregateExpr >> E --> aggFn1("avg") >> E.
  aggregateExpr >> E --> aggFn1("sample") >> E.
  aggregateExpr >> .groupConcat(.some(E),Dist,Sep) --> sparqlKw("group_concat"), punc("("), kwFlag("distinct") >> Dist,
    expression >> E, ? (punc(";"), sparqlKw("separator"), punc("="), stringLiteral) >> SepOpt, punc(")"),
    {Sep .= optMarkupToString(SepOpt)}.

  countTarget:() >> option[expression] --> cons[token].
  countTarget >> .none --> punc("*").
  countTarget >> .some(E) --> expression >> E.

  optMarkupToString:(option[cons[markup]]) => option[string].
  optMarkupToString(.none) => .none.
  optMarkupToString(.some(Segs)) => .some(markupToString(Segs)).

  markupToString:(cons[markup]) => string.
  markupToString([]) => "".
  markupToString([M,..Rest]) => markupPart(M) ++ markupToString(Rest).

  markupPart(.str(S)) => S.
  markupPart(.link(_,_)) => unreachable.

  aggFn1:(string) >> expression --> cons[token].
  aggFn1(Kw) >> .aggregate(Kw,.some(E),Dist) --> sparqlKw(Kw), punc("("), kwFlag("distinct") >> Dist,
    expression >> E, punc(")").

  iriOrFunction:() >> expression --> cons[token].
  iriOrFunction >> combineIriOrFunction(C,ArgOpt) --> iri >> C, ? argList >> ArgOpt.

  combineIriOrFunction:(concept,option[(cons[expression],boolean)]) => expression.
  combineIriOrFunction(C,.none) => .term(.literal(C)).
  combineIriOrFunction(C,.some((Args,Dist))) => .funcCall(.literal(C),Args,Dist).

  functionCall:() >> expression --> cons[token].
  functionCall >> .funcCall(.literal(C),Args,Dist) --> iri >> C, argList >> (Args,Dist).

  argList:() >> (cons[expression],boolean) --> cons[token].
  argList >> ([],.false) --> rdfNil.
  argList >> ([E0,..Es],Dist) --> punc("("), kwFlag("distinct") >> Dist, expression >> E0,
    (punc(","), expression)* >> Es, punc(")").

  expressionList:() >> cons[expression] --> cons[token].
  expressionList >> [] --> rdfNil.
  expressionList >> [E0,..Es] --> punc("("), expression >> E0, (punc(","), expression)* >> Es, punc(")").

  /* SPARQL strings don't support N3-style `$(...)` interpolation, but
     rdf.lexer's string reader is shared and will still fire its
     interpolation path for the literal text `$[` -- an edge case treated
     degenerately here (empty segment) rather than by giving the lexer a
     SPARQL-specific string mode. */
  stringLiteral:() >> cons[markup] --> cons[token].
  stringLiteral >> segsToMarkup(Segs) --> [.tok(_,.strTok(Segs))].

  /* Section 6: RDF terms and literals. This closes out every forward
     reference from sections 1-5.

     NumericLiteralUnsigned collapses the BNF's INTEGER/DECIMAL/DOUBLE
     three-way split down to the two numeric token kinds rdf.lexer actually
     produces (.intTok/.fltTok) -- it doesn't distinguish DECIMAL from DOUBLE
     itself.

     NumericLiteralPositive/Negative are simplified: real SPARQL tokenizes
     `+3`/`-3` as a single signed-literal token but `+ 3`/`- 3` (with a space)
     as separate operator and literal tokens -- an adjacency distinction
     rdf.lexer doesn't make (it always emits `+`/`-` as standalone punc
     tokens). Here they're just `punc("+"|"-"), numericLiteralUnsigned`,
     which still recognizes valid signed literals correctly since these rules
     are only reachable from grammar positions where a signed literal is
     expected; it just doesn't enforce the no-whitespace rule the real
     grammar does.

     LANG_DIR (`@en`, `@en-US`, `@en--Latn`, ...) simplifies to `@` followed
     by one identifier token: rdf.lexer's identifier scanner already treats
     internal `-`/`--` as ordinary identifier characters (see lexer.star's
     isIdentChr), so it greedily consumes the whole tag as a single .idTok
     without the grammar needing to spell out the subtag structure itself. */

  /* The three alternatives are tried longest-first (lang tag, then datatype,
     then bare) so the bare `.text` case doesn't preempt a suffix that's
     actually present -- matching how the recognizer's `?(A|B)` tries A/B
     before falling back to matching nothing. */
  rdfLiteral:() >> concept --> cons[token].
  rdfLiteral >> .langText(S,L) --> stringLiteral >> S, langTag >> L.
  rdfLiteral >> .typedText(S,D) --> stringLiteral >> S, punc("^"), punc("^"), iri >> D.
  rdfLiteral >> .text(S) --> stringLiteral >> S.

  langTag:() >> string --> cons[token].
  langTag >> L --> punc("@"), [.tok(_,.idTok(L))].

  numericLiteral:() >> concept --> cons[token].
  numericLiteral >> N --> numericLiteralUnsigned >> N.
  numericLiteral >> N --> numericLiteralPositive >> N.
  numericLiteral >> N --> numericLiteralNegative >> N.

  numericLiteralUnsigned:() >> concept --> cons[token].
  numericLiteralUnsigned >> .int(N) --> [.tok(_,.intTok(N))].
  numericLiteralUnsigned >> .flt(N) --> [.tok(_,.fltTok(N))].

  numericLiteralPositive:() >> concept --> cons[token].
  numericLiteralPositive >> N --> punc("+"), numericLiteralUnsigned >> N.

  numericLiteralNegative:() >> concept --> cons[token].
  numericLiteralNegative >> negateConcept(N) --> punc("-"), numericLiteralUnsigned >> N.

  booleanLiteral:() >> concept --> cons[token].
  booleanLiteral >> .bool(.true) --> sparqlKw("true").
  booleanLiteral >> .bool(.false) --> sparqlKw("false").

  /* A BLANK_NODE_LABEL's `_:` only tokenizes as a bare `_` identifier
     followed by `:` when nothing else is glued onto the underscore (`:` is
     not an identifier-continuation character, so the lexer's identifier scan
     stops right there) -- matching how `_:label` is always written with no
     space, per the SPARQL grammar. labeledBlank/genAnon both reuse
     rdf.triple's .anon representation -- the same one Turtle/N3 blank nodes
     already use -- with labeledBlank keeping same-label-same-node consistent
     within one query via a small assoc list reset at the start of queryUnit. */
  blankNode:() >> concept --> cons[token].
  blankNode >> labeledBlank(L) --> [.tok(_,.idTok("_"))], punc(":"), [.tok(_,.idTok(L))].
  blankNode >> genAnon() --> punc("["), punc("]").

  -- Foundational terminals (built directly on rdf.lexer's generic token stream)

  public varRef:() >> string --> cons[token].
  varRef >> V --> punc("?"), [.tok(_,.idTok(V))].
  varRef >> V --> punc("$"), [.tok(_,.idTok(V))].

  public varOrIri:() >> term --> cons[token].
  varOrIri >> .var(V) --> varRef >> V.
  varOrIri >> .literal(C) --> iri >> C.

  /* iri produces a concept (never a variable), reused as-is anywhere a
     ground IRI is needed; varOrIri/term-building rules wrap it in .literal. */
  public iri:() >> concept --> cons[token].
  iri >> .uri(U) --> [.tok(_,.uriTok(U))].
  iri >> C --> prefixedName >> C.

  prefixedName:() >> concept --> cons[token].
  prefixedName >> C --> pnameLn >> C.
  prefixedName >> .named(P,"") --> pnameNs >> P.

  pnameLn:() >> concept --> cons[token].
  pnameLn >> .named(P,L) --> pnameNs >> P, [.tok(_,.idTok(L))].

  pnameNs:() >> string --> cons[token].
  pnameNs >> P --> [.tok(_,.idTok(P))], punc(":").
  pnameNs >> "" --> punc(":").

  integerToken:() >> integer --> cons[token].
  integerToken >> N --> [.tok(_,.intTok(N))].

  /* SPARQL keywords are case-insensitive (unlike rdf.parser's `keyword`,
     which is an exact case-sensitive match used by the N3 parser). */
  sparqlKw(Key) --> [.tok(_,.idTok(Id))], {sameKeyword(Key,Id)}.

  sameKeyword(Key,Id) => foldCase(Key)==foldCase(Id).

  foldCase:(string) => string.
  foldCase(S) => (S::cons[char]//foldChar)::string.

  foldChar(Ch) where _isLuChar(Ch) => _int2chr(_codePoint(Ch)+32).
  foldChar(Ch) default => Ch.

  punc(P) --> [.tok(_,.pncTok(P))].

  -- Section 3 support functions

  filterSome:all t ~~ (cons[option[t]]) => cons[t].
  filterSome([]) => [].
  filterSome([.some(X),..Xs]) => [X,..filterSome(Xs)].
  filterSome([.none,..Xs]) => filterSome(Xs).

  /* Combine a subject with the (predicate,objects) pairs collected by
     propertyList(Path)(NotEmpty) into the actual conjoined .basic triples,
     folding in each object's extra structure (from a nested collection/
     blank node property list) and wrapping with .annotated where present. */
  propertyPairsToPattern:(term,propertyPairs) => pattern.
  propertyPairsToPattern(_,[]) => .nilPattern.
  propertyPairsToPattern(S,[(P,Os)]) => objectsToPattern(S,P,Os).
  propertyPairsToPattern(S,[(P,Os),..Rest]) =>
    conjPattern2(objectsToPattern(S,P,Os),propertyPairsToPattern(S,Rest)).

  objectsToPattern:(term,predicate,cons[(term,pattern,cons[annotationItem])]) => pattern.
  objectsToPattern(_,_,[]) => .nilPattern.
  objectsToPattern(S,P,[(O,Extra,As)]) => conjPattern2(Extra,oneTriple(S,P,O,As)).
  objectsToPattern(S,P,[(O,Extra,As),..Rest]) =>
    conjPattern2(conjPattern2(Extra,oneTriple(S,P,O,As)),objectsToPattern(S,P,Rest)).

  oneTriple:(term,predicate,term,cons[annotationItem]) => pattern.
  oneTriple(S,P,O,[]) => .basic(S,P,O).
  oneTriple(S,P,O,As) => .annotated(.basic(S,P,O),As).

  conjPattern:(pattern,option[pattern]) => pattern.
  conjPattern(T,.none) => T.
  conjPattern(T,.some(P2)) => conjPattern2(T,P2).

  conjPattern2:(pattern,pattern) => pattern.
  conjPattern2(.nilPattern,P) => P.
  conjPattern2(P,.nilPattern) => P.
  conjPattern2(P1,P2) => .conj(P1,P2).

  rdfTypeConcept() => .uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type").
  rdfFirstUri() => "http://www.w3.org/1999/02/22-rdf-syntax-ns#first".
  rdfRestUri() => "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest".
  rdfNilUri() => "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil".

  foldPathAlt:(path,cons[path]) => path.
  foldPathAlt(P0,[]) => P0.
  foldPathAlt(P0,[P1,..Rest]) => foldPathAlt(.alt(P0,P1),Rest).

  foldPathSeq:(path,cons[path]) => path.
  foldPathSeq(P0,[]) => P0.
  foldPathSeq(P0,[P1,..Rest]) => foldPathSeq(.seq(P0,P1),Rest).

  applyPathMod:(path,option[pathMod]) => path.
  applyPathMod(P,.none) => P.
  applyPathMod(P,.some(M)) => .mod(P,M).

  /* Builds the standard rdf:first/rdf:rest cons-list encoding for a
     Collection/CollectionPath, returning the list's anchor term (the first
     cell, or rdf:nil if empty) and the pattern of triples describing it. */
  collectionToPattern:(cons[(term,pattern)]) => (term,pattern).
  collectionToPattern([]) => (.literal(.uri(rdfNilUri())), .nilPattern).
  collectionToPattern([(E,EPat),..Rest]) where
      Cell .= .literal(genAnon()) && (RestAnchor,RestPat) .= collectionToPattern(Rest) =>
    (Cell, conjPattern2(EPat,
      conjPattern2(.basic(Cell,.simple(.literal(.uri(rdfFirstUri()))),E),
        conjPattern2(.basic(Cell,.simple(.literal(.uri(rdfRestUri()))),RestAnchor), RestPat)))).

  /* Collapses the `? reifierClause` around a `<< ... >>`'s optional reifier
     (reifierClause's own result is already option[term], for its own
     optional id) down to a single option[term]. */
  annotationItemToReifierId:(option[option[term]]) => option[term].
  annotationItemToReifierId(.none) => .none.
  annotationItemToReifierId(.some(X)) => X.

  /* Placeholder subject for an annotation block's own triples: real RDF-star
     semantics connect these to the enclosing triple's reifier, which this
     pass doesn't wire up (see the annotationItem comment in query.star). */
  anonSubject() => .literal(genAnon()).

  -- Section 6 support functions

  segsToMarkup:(cons[stringSegment]) => cons[markup].
  segsToMarkup(Segs) => (Segs//segToMarkup).

  segToMarkup(.segment(_,S)) => .str(S).
  segToMarkup(.interpolate(_,_,_)) => .str("").

  negateConcept(.int(N)) => .int(-N).
  negateConcept(.flt(N)) => .flt(-N).

  /* Blank-node label -> concept mapping, so that repeated uses of the same
     `_:label` within one query resolve to the same node. Reset at the start
     of each top-level parse (see queryUnit in section 1); like
     rdf.triple.genAnon's own counter, this is a shared module-level ref, not
     safely reentrant across concurrent parses. */
  BlankLabels = ref ([]:cons[(string,concept)]).

  public resetBlankNodes:(){}.
  resetBlankNodes(){
    BlankLabels := []
  }

  labeledBlank:(string) => concept.
  labeledBlank(L) => valof{
    if C ?= assocLookup(L,BlankLabels!) then{
      valis C
    } else{
      New = genAnon();
      BlankLabels := [(L,New),..BlankLabels!];
      valis New
    }
  }

  assocLookup(_,[]) => .none.
  assocLookup(K,[(K,V),.._]) => .some(V).
  assocLookup(K,[_,..Rest]) => assocLookup(K,Rest).
}
