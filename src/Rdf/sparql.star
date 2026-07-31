rdf.sparql.parser{
  import star.

  import rdf.token.
  import rdf.triple.
  import rdf.sparql.query.

  -- Parse SPARQL queries -- see sparql.bnf for the reference grammar --
  -- into the query/pattern/term/expression AST defined in rdf.sparql.query.
  --
  -- A number of grammar rules below are suffixed (Clause/Expr/Rule) to avoid
  -- colliding with same-named AST constructors imported from
  -- rdf.sparql.query (e.g. the bindClause rule vs. the pattern.bind
  -- constructor) -- importing that module brings its constructor labels into
  -- scope as bare names, which clash with a rule of the same name.
  --
  -- Section 1: query forms and prologue. The following are used here but
  -- defined in later sections, and are not yet defined anywhere in this file:
  --   groupGraphPattern, constructTemplate, expression, brackettedExpression,
  --   builtInCall, functionCall, constraint, dataBlock.

  public queryUnit:() >> () --> cons[token].
  queryUnit --> sparqlQuery, [.endTok(_)].

  sparqlQuery --> prologue,
    (selectQuery | constructQuery | describeQuery | askQuery),
    valuesClause.

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

  selectQuery --> selectClause, datasetClause*, whereClause, solutionModifier.

  subSelectClause --> selectClause, whereClause, solutionModifier, valuesClause.

  selectClause --> sparqlKw("select"), ? (sparqlKw("distinct") | sparqlKw("reduced")),
    selectVars.

  selectVars --> punc("*").
  selectVars --> selectVar, selectVar*.

  selectVar --> varRef.
  selectVar --> punc("("), expression, sparqlKw("as"), varRef, punc(")").

  constructQuery --> sparqlKw("construct"), constructBody.

  constructBody --> constructTemplate, datasetClause*, whereClause, solutionModifier.
  constructBody --> datasetClause*, sparqlKw("where"), constructTemplate, solutionModifier.

  describeQuery --> sparqlKw("describe"), describeTargets,
    datasetClause*, ? whereClause, solutionModifier.

  describeTargets --> punc("*").
  describeTargets --> varOrIri, varOrIri*.

  askQuery --> sparqlKw("ask"), datasetClause*, whereClause, solutionModifier.

  -- Dataset clause

  datasetClause --> sparqlKw("from"), (defaultGraphClause | namedGraphClause).
  defaultGraphClause --> sourceSelector.
  namedGraphClause --> sparqlKw("named"), sourceSelector.
  sourceSelector --> iri.

  -- Where clause

  whereClause --> ? sparqlKw("where"), groupGraphPattern.

  -- Solution modifiers

  solutionModifier --> ? groupClause, ? havingClause, ? orderClause, ? limitOffsetClauses.

  groupClause --> sparqlKw("group"), sparqlKw("by"), groupCondition, groupCondition*.

  groupCondition --> builtInCall.
  groupCondition --> functionCall.
  groupCondition --> punc("("), expression, ? (sparqlKw("as"), varRef), punc(")").
  groupCondition --> varRef.

  havingClause --> sparqlKw("having"), havingCondition, havingCondition*.
  havingCondition --> constraint.

  orderClause --> sparqlKw("order"), sparqlKw("by"), orderCondition, orderCondition*.

  orderCondition --> (sparqlKw("asc") | sparqlKw("desc")), brackettedExpression.
  orderCondition --> constraint.
  orderCondition --> varRef.

  limitOffsetClauses --> limitClause, ? offsetClause.
  limitOffsetClauses --> offsetClause, ? limitClause.

  limitClause --> sparqlKw("limit"), integerToken.
  offsetClause --> sparqlKw("offset"), integerToken.

  -- Values clause

  valuesClause --> ? (sparqlKw("values"), dataBlock).

  -- Section 2: graph patterns. The following are used here but defined in
  -- later sections, and are not yet defined anywhere in this file:
  --   triplesSameSubjectPath, tripleTermData (section 3);
  --   expression, brackettedExpression, builtInCall, functionCall (sections 4/5);
  --   rdfLiteral, numericLiteral, booleanLiteral (section 6).

  groupGraphPattern --> punc("{"), (subSelectClause | groupGraphPatternSub), punc("}").

  groupGraphPatternSub --> ? triplesBlock,
    (graphPatternNotTriples, ? punc("."), ? triplesBlock)*.

  triplesBlock --> triplesSameSubjectPath, ? (punc("."), ? triplesBlock).

  graphPatternNotTriples --> groupOrUnionGraphPattern.
  graphPatternNotTriples --> optionalGraphPattern.
  graphPatternNotTriples --> minusGraphPattern.
  graphPatternNotTriples --> graphGraphPattern.
  graphPatternNotTriples --> serviceGraphPattern.
  graphPatternNotTriples --> filterClause.
  graphPatternNotTriples --> bindClause.
  graphPatternNotTriples --> inlineData.

  groupOrUnionGraphPattern --> groupGraphPattern, (sparqlKw("union"), groupGraphPattern)*.

  optionalGraphPattern --> sparqlKw("optional"), groupGraphPattern.

  minusGraphPattern --> sparqlKw("minus"), groupGraphPattern.

  graphGraphPattern --> sparqlKw("graph"), varOrIri, groupGraphPattern.

  serviceGraphPattern --> sparqlKw("service"), ? sparqlKw("silent"), varOrIri, groupGraphPattern.

  bindClause --> sparqlKw("bind"), punc("("), expression, sparqlKw("as"), varRef, punc(")").

  inlineData --> sparqlKw("values"), dataBlock.

  dataBlock --> inlineDataOneVar.
  dataBlock --> inlineDataFull.

  inlineDataOneVar --> varRef, punc("{"), dataBlockValue*, punc("}").

  inlineDataFull --> (rdfNil | punc("("), varRef*, punc(")")),
    punc("{"), (punc("("), dataBlockValue*, punc(")") | rdfNil)*, punc("}").

  dataBlockValue --> iri.
  dataBlockValue --> rdfLiteral.
  dataBlockValue --> numericLiteral.
  dataBlockValue --> booleanLiteral.
  dataBlockValue --> sparqlKw("undef").
  dataBlockValue --> tripleTermData.

  rdfNil --> punc("("), punc(")").

  filterClause --> sparqlKw("filter"), constraint.

  constraint --> brackettedExpression.
  constraint --> builtInCall.
  constraint --> functionCall.

  -- Section 3: triples and property paths, including RDF-star reified
  -- triples/triple terms and annotations. The following are used here but
  -- defined in later sections: rdfLiteral, numericLiteral, booleanLiteral
  -- (section 6).
  --
  -- propertyList(Path)(NotEmpty) build a plain list of (predicate,objects)
  -- pairs rather than a pattern directly, since they don't know the subject
  -- -- it's supplied by the caller (triplesSameSubject(Path)), which combines
  -- subject+pairs into the actual conjoined .basic triples via
  -- propertyPairsToPattern. Each object carries the extra `pattern` from a
  -- nested collection/blank node property list used as that object (e.g.
  -- `?s ex:p (1 2 3)`), which must still be conjoined into the result.
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

  -- A bare sequence's `>>` value is only its *last* term (per grammar.adoc),
  -- so `(verb, objectList)` alone would only capture objectList, silently
  -- dropping the verb -- this explicit pair rule avoids that.
  verbObjectPair:() >> (predicate,cons[(term,pattern,cons[annotationItem])]) --> cons[token].
  verbObjectPair >> (V,OL) --> verb >> V, objectList >> OL.

  verb:() >> predicate --> cons[token].
  verb >> P --> varOrIri >> T, {P .= .simple(T)}.
  verb >> .simple(.literal(rdfTypeConcept())) --> sparqlKw("a").

  objectList:() >> cons[(term,pattern,cons[annotationItem])] --> cons[token].
  objectList >> [O0,..Os] --> object >> O0, (punc(","), object)* >> Os.

  -- The middle `pattern` carries structure from a nested collection/blank
  -- node property list used directly as an object (e.g. `?s ex:p (1 2 3)`)
  -- -- discarding it would silently drop that structure's own triples.
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

  -- The `?` (zero-or-one) modifier is followed here by a negative lookahead:
  -- since a variable is lexed as two tokens (`punc("?")` then an identifier,
  -- see `varRef` below), a bare `punc("?")` immediately followed by an
  -- identifier token is the start of the *next* variable, not a path modifier.
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

  -- Fully-ground triple term, as used in VALUES data blocks: unlike
  -- tripleTerm above, this can't hold a variable, so it produces a plain
  -- rdf.triple.concept (reusing .tripleTerm(triple) there) rather than a
  -- query.term.
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

  -- Section 4: expressions. The following are used here but defined in later
  -- sections: iriOrFunction, expressionList, builtInCall (section 5);
  -- rdfLiteral, numericLiteral, numericLiteralPositive, numericLiteralNegative,
  -- booleanLiteral (section 6).
  --
  -- `&&`, `||` and `!=` compose from adjacent single-char tokens (`&`,`&`;
  -- `|`,`|`; `!`,`=`) already produced by rdf.lexer, the same way `{|`/`|}`
  -- did in section 3 -- no further lexer changes needed for this section.

  expression --> conditionalOrExpression.

  conditionalOrExpression --> conditionalAndExpression,
    (punc("|"), punc("|"), conditionalAndExpression)*.

  conditionalAndExpression --> valueLogical, (punc("&"), punc("&"), valueLogical)*.

  valueLogical --> relationalExpression.

  relationalExpression --> numericExpression,
    ? ( punc("="), numericExpression
      | punc("!"), punc("="), numericExpression
      | punc("<"), numericExpression
      | punc(">"), numericExpression
      | punc("<="), numericExpression
      | punc(">="), numericExpression
      | sparqlKw("in"), expressionList
      | sparqlKw("not"), sparqlKw("in"), expressionList
      ).

  numericExpression --> additiveExpression.

  additiveExpression --> multiplicativeExpression,
    ( punc("+"), multiplicativeExpression
    | punc("-"), multiplicativeExpression
    | (numericLiteralPositive | numericLiteralNegative), signedFactorTail
    )*.

  signedFactorTail --> (punc("*"), unaryExpression | punc("/"), unaryExpression)*.

  multiplicativeExpression --> unaryExpression, (punc("*"), unaryExpression | punc("/"), unaryExpression)*.

  unaryExpression --> punc("!"), unaryExpression.
  unaryExpression --> punc("+"), primaryExpression.
  unaryExpression --> punc("-"), primaryExpression.
  unaryExpression --> primaryExpression.

  primaryExpression --> brackettedExpression.
  primaryExpression --> builtInCall.
  primaryExpression --> iriOrFunction.
  primaryExpression --> rdfLiteral.
  primaryExpression --> numericLiteral.
  primaryExpression --> booleanLiteral.
  primaryExpression --> varRef.
  primaryExpression --> exprTripleTermRule.

  exprTripleTermRule --> punc("<<"), punc("("), exprTripleTermSubject, verb, exprTripleTermObject, punc(")"), punc(">>").

  exprTripleTermSubject --> iri.
  exprTripleTermSubject --> varRef.

  exprTripleTermObject --> iri.
  exprTripleTermObject --> rdfLiteral.
  exprTripleTermObject --> numericLiteral.
  exprTripleTermObject --> booleanLiteral.
  exprTripleTermObject --> varRef.
  exprTripleTermObject --> exprTripleTermRule.

  brackettedExpression --> punc("("), expression, punc(")").

  -- Section 5: built-ins, aggregates, and function calls. The following are
  -- used here but defined in section 6: rdfLiteral, numericLiteral,
  -- booleanLiteral.
  --
  -- Most BuiltInCall alternatives share one of a handful of shapes (keyword
  -- applied to 1/2/3 expressions, or to NIL); oneArgBuiltin/twoArgBuiltin/
  -- threeArgBuiltin/nilArgBuiltin/aggFn1 below are parameterized over the
  -- keyword to avoid repeating each shape ~15-20 times.

  builtInCall --> aggregateExpr.
  builtInCall --> oneArgBuiltin("str").
  builtInCall --> oneArgBuiltin("lang").
  builtInCall --> twoArgBuiltin("langmatches").
  builtInCall --> oneArgBuiltin("langdir").
  builtInCall --> oneArgBuiltin("datatype").
  builtInCall --> sparqlKw("bound"), punc("("), varRef, punc(")").
  builtInCall --> oneArgBuiltin("iri").
  builtInCall --> oneArgBuiltin("uri").
  builtInCall --> sparqlKw("bnode"), (punc("("), expression, punc(")") | rdfNil).
  builtInCall --> nilArgBuiltin("rand").
  builtInCall --> oneArgBuiltin("abs").
  builtInCall --> oneArgBuiltin("ceil").
  builtInCall --> oneArgBuiltin("floor").
  builtInCall --> oneArgBuiltin("round").
  builtInCall --> sparqlKw("concat"), expressionList.
  builtInCall --> substringExpression.
  builtInCall --> oneArgBuiltin("strlen").
  builtInCall --> strReplaceExpression.
  builtInCall --> oneArgBuiltin("ucase").
  builtInCall --> oneArgBuiltin("lcase").
  builtInCall --> oneArgBuiltin("encode_for_uri").
  builtInCall --> twoArgBuiltin("contains").
  builtInCall --> twoArgBuiltin("strstarts").
  builtInCall --> twoArgBuiltin("strends").
  builtInCall --> twoArgBuiltin("strbefore").
  builtInCall --> twoArgBuiltin("strafter").
  builtInCall --> oneArgBuiltin("year").
  builtInCall --> oneArgBuiltin("month").
  builtInCall --> oneArgBuiltin("day").
  builtInCall --> oneArgBuiltin("hours").
  builtInCall --> oneArgBuiltin("minutes").
  builtInCall --> oneArgBuiltin("seconds").
  builtInCall --> oneArgBuiltin("timezone").
  builtInCall --> oneArgBuiltin("tz").
  builtInCall --> nilArgBuiltin("now").
  builtInCall --> nilArgBuiltin("uuid").
  builtInCall --> nilArgBuiltin("struuid").
  builtInCall --> oneArgBuiltin("md5").
  builtInCall --> oneArgBuiltin("sha1").
  builtInCall --> oneArgBuiltin("sha256").
  builtInCall --> oneArgBuiltin("sha384").
  builtInCall --> oneArgBuiltin("sha512").
  builtInCall --> sparqlKw("coalesce"), expressionList.
  builtInCall --> sparqlKw("if"), punc("("), expression, punc(","), expression, punc(","), expression, punc(")").
  builtInCall --> twoArgBuiltin("strlang").
  builtInCall --> threeArgBuiltin("strlangdir").
  builtInCall --> twoArgBuiltin("strdt").
  builtInCall --> twoArgBuiltin("sameterm").
  builtInCall --> oneArgBuiltin("isiri").
  builtInCall --> oneArgBuiltin("isuri").
  builtInCall --> oneArgBuiltin("isblank").
  builtInCall --> oneArgBuiltin("isliteral").
  builtInCall --> oneArgBuiltin("isnumeric").
  builtInCall --> oneArgBuiltin("haslang").
  builtInCall --> oneArgBuiltin("haslangdir").
  builtInCall --> regexExpression.
  builtInCall --> existsFunc.
  builtInCall --> notExistsFunc.
  builtInCall --> oneArgBuiltin("istriple").
  builtInCall --> threeArgBuiltin("triple").
  builtInCall --> oneArgBuiltin("subject").
  builtInCall --> oneArgBuiltin("predicate").
  builtInCall --> oneArgBuiltin("object").

  oneArgBuiltin(Kw) --> sparqlKw(Kw), punc("("), expression, punc(")").
  twoArgBuiltin(Kw) --> sparqlKw(Kw), punc("("), expression, punc(","), expression, punc(")").
  threeArgBuiltin(Kw) --> sparqlKw(Kw), punc("("), expression, punc(","), expression, punc(","), expression, punc(")").
  nilArgBuiltin(Kw) --> sparqlKw(Kw), rdfNil.

  regexExpression --> sparqlKw("regex"), punc("("), expression, punc(","), expression,
    ? (punc(","), expression), punc(")").

  substringExpression --> sparqlKw("substr"), punc("("), expression, punc(","), expression,
    ? (punc(","), expression), punc(")").

  strReplaceExpression --> sparqlKw("replace"), punc("("), expression, punc(","), expression,
    punc(","), expression, ? (punc(","), expression), punc(")").

  existsFunc --> sparqlKw("exists"), groupGraphPattern.

  notExistsFunc --> sparqlKw("not"), sparqlKw("exists"), groupGraphPattern.

  aggregateExpr --> sparqlKw("count"), punc("("), ? sparqlKw("distinct"), (punc("*") | expression), punc(")").
  aggregateExpr --> aggFn1("sum").
  aggregateExpr --> aggFn1("min").
  aggregateExpr --> aggFn1("max").
  aggregateExpr --> aggFn1("avg").
  aggregateExpr --> aggFn1("sample").
  aggregateExpr --> sparqlKw("group_concat"), punc("("), ? sparqlKw("distinct"), expression,
    ? (punc(";"), sparqlKw("separator"), punc("="), stringLiteral), punc(")").

  aggFn1(Kw) --> sparqlKw(Kw), punc("("), ? sparqlKw("distinct"), expression, punc(")").

  iriOrFunction --> iri, ? argList.

  functionCall --> iri, argList.

  argList --> rdfNil.
  argList --> punc("("), ? sparqlKw("distinct"), expression, (punc(","), expression)*, punc(")").

  expressionList --> rdfNil.
  expressionList --> punc("("), expression, (punc(","), expression)*, punc(")").

  -- SPARQL strings don't support N3-style `$(...)` interpolation, but
  -- rdf.lexer's string reader is shared and will still fire its
  -- interpolation path for the literal text `$[` -- an edge case treated
  -- degenerately here (empty segment) rather than by giving the lexer a
  -- SPARQL-specific string mode.
  stringLiteral:() >> cons[markup] --> cons[token].
  stringLiteral >> segsToMarkup(Segs) --> [.tok(_,.strTok(Segs))].

  -- Section 6: RDF terms and literals. This closes out every forward
  -- reference from sections 1-5.
  --
  -- NumericLiteralUnsigned collapses the BNF's INTEGER/DECIMAL/DOUBLE
  -- three-way split down to the two numeric token kinds rdf.lexer actually
  -- produces (.intTok/.fltTok) -- it doesn't distinguish DECIMAL from DOUBLE
  -- itself.
  --
  -- NumericLiteralPositive/Negative are simplified: real SPARQL tokenizes
  -- `+3`/`-3` as a single signed-literal token but `+ 3`/`- 3` (with a space)
  -- as separate operator and literal tokens -- an adjacency distinction
  -- rdf.lexer doesn't make (it always emits `+`/`-` as standalone punc
  -- tokens). Here they're just `punc("+"|"-"), numericLiteralUnsigned`,
  -- which still recognizes valid signed literals correctly since these rules
  -- are only reachable from grammar positions where a signed literal is
  -- expected; it just doesn't enforce the no-whitespace rule the real
  -- grammar does.
  --
  -- LANG_DIR (`@en`, `@en-US`, `@en--Latn`, ...) simplifies to `@` followed
  -- by one identifier token: rdf.lexer's identifier scanner already treats
  -- internal `-`/`--` as ordinary identifier characters (see lexer.star's
  -- isIdentChr), so it greedily consumes the whole tag as a single .idTok
  -- without the grammar needing to spell out the subtag structure itself.

  -- The three alternatives are tried longest-first (lang tag, then datatype,
  -- then bare) so the bare `.text` case doesn't preempt a suffix that's
  -- actually present -- matching how the recognizer's `?(A|B)` tries A/B
  -- before falling back to matching nothing.
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

  -- A BLANK_NODE_LABEL's `_:` only tokenizes as a bare `_` identifier
  -- followed by `:` when nothing else is glued onto the underscore (`:` is
  -- not an identifier-continuation character, so the lexer's identifier scan
  -- stops right there) -- matching how `_:label` is always written with no
  -- space, per the SPARQL grammar. labeledBlank/genAnon both reuse
  -- rdf.triple's .anon representation -- the same one Turtle/N3 blank nodes
  -- already use -- with labeledBlank keeping same-label-same-node consistent
  -- within one query via a small assoc list reset at the start of queryUnit.
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

  -- iri produces a concept (never a variable), reused as-is anywhere a
  -- ground IRI is needed; varOrIri/term-building rules wrap it in .literal.
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

  integerToken --> [.tok(_,.intTok(_))].

  -- SPARQL keywords are case-insensitive (unlike rdf.parser's `keyword`,
  -- which is an exact case-sensitive match used by the N3 parser).
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

  -- Combine a subject with the (predicate,objects) pairs collected by
  -- propertyList(Path)(NotEmpty) into the actual conjoined .basic triples,
  -- folding in each object's extra structure (from a nested collection/
  -- blank node property list) and wrapping with .annotated where present.
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

  -- Builds the standard rdf:first/rdf:rest cons-list encoding for a
  -- Collection/CollectionPath, returning the list's anchor term (the first
  -- cell, or rdf:nil if empty) and the pattern of triples describing it.
  collectionToPattern:(cons[(term,pattern)]) => (term,pattern).
  collectionToPattern([]) => (.literal(.uri(rdfNilUri())), .nilPattern).
  collectionToPattern([(E,EPat),..Rest]) where
      Cell .= .literal(genAnon()) && (RestAnchor,RestPat) .= collectionToPattern(Rest) =>
    (Cell, conjPattern2(EPat,
      conjPattern2(.basic(Cell,.simple(.literal(.uri(rdfFirstUri()))),E),
        conjPattern2(.basic(Cell,.simple(.literal(.uri(rdfRestUri()))),RestAnchor), RestPat)))).

  -- Collapses the `? reifierClause` around a `<< ... >>`'s optional reifier
  -- (reifierClause's own result is already option[term], for its own
  -- optional id) down to a single option[term].
  annotationItemToReifierId:(option[option[term]]) => option[term].
  annotationItemToReifierId(.none) => .none.
  annotationItemToReifierId(.some(X)) => X.

  -- Placeholder subject for an annotation block's own triples: real RDF-star
  -- semantics connect these to the enclosing triple's reifier, which this
  -- pass doesn't wire up (see the annotationItem comment in query.star).
  anonSubject() => .literal(genAnon()).

  -- Section 6 support functions

  segsToMarkup:(cons[stringSegment]) => cons[markup].
  segsToMarkup(Segs) => (Segs//segToMarkup).

  segToMarkup(.segment(_,S)) => .str(S).
  segToMarkup(.interpolate(_,_,_)) => .str("").

  negateConcept(.int(N)) => .int(-N).
  negateConcept(.flt(N)) => .flt(-N).

  -- Blank-node label -> concept mapping, so that repeated uses of the same
  -- `_:label` within one query resolve to the same node. Reset at the start
  -- of each top-level parse (see queryUnit in section 1); like
  -- rdf.triple.genAnon's own counter, this is a shared module-level ref, not
  -- safely reentrant across concurrent parses.
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
