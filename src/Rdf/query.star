rdf.sparql.query{
  import star.
  import rdf.triple.

  -- The SPARQL query AST. Extends the original query/pattern/term/projection
  -- skeleton to cover the full grammar in rdf.sparql.parser: construct/
  -- describe query forms, property paths, RDF-star (reified triples/triple
  -- terms/annotations/reifiers), and the full expression/built-in surface.

  public query ::= .select(selectModifier,projection,cons[datasetClause],pattern,solutionMods)
  | .construct(cons[pattern],cons[datasetClause],pattern,solutionMods)
  | .describe(describeTargets,cons[datasetClause],option[pattern],solutionMods)
  | .ask(cons[datasetClause],pattern,solutionMods).

  public selectModifier ::= .noModifier | .distinct | .reduced.

  public projection ::= .selectAll | .vars(cons[selectVar]).

  public selectVar ::= .plain(string) | .computed(expression,string).

  public describeTargets ::= .allDescribed | .described(cons[term]).

  public datasetClause ::= .defaultGraph(term) | .namedGraph(term).

  public solutionMods ::= solutionMods{
    grouping:cons[groupCondition].
    having:cons[expression].
    ordering:cons[orderCondition].
    limit:option[integer].
    offset:option[integer].
  }.

  public noMods:()=>solutionMods.
  noMods() => solutionMods{grouping=[]. having=[]. ordering=[]. limit=.none. offset=.none}.

  public groupCondition ::= .groupExpr(expression,option[string]).

  public orderCondition ::= .asc(expression) | .desc(expression).

  public dataBlock ::= .oneVar(string,cons[option[term]]) | .full(cons[string],cons[cons[option[term]]]).

  -- Graph patterns (the body of a WHERE clause, CONSTRUCT template, etc.)
  public pattern ::= .basic(term,predicate,term)
  | .filter(expression)
  | .bind(expression,string)
  | .values(dataBlock)
  | .conj(pattern,pattern)
  | .union(pattern,pattern)
  | .optional(pattern,pattern)
  | .minus(pattern,pattern)
  | .graph(term,pattern)
  | .service(term,pattern,boolean)
  | .nilPattern
  | .subSelect(query)
  | .annotated(pattern,cons[annotationItem]).

  -- A triple's predicate position: either a plain term (non-path context,
  -- e.g. inside a construct template) or a full property path.
  public predicate ::= .simple(term) | .path(path).

  -- Property paths
  public path ::= .predicate(term)
  | .inverse(path)
  | .seq(path,path)
  | .alt(path,path)
  | .mod(path,pathMod)
  | .negated(cons[negatedPathItem])
  | .group(path).

  public pathMod ::= .pOptional | .pStar | .pPlus.

  public negatedPathItem ::= .fwd(term) | .inv(term).

  -- RDF terms as they appear in triple patterns/expressions. Ground values
  -- (IRIs, literals, blank nodes, and fully-ground triple terms -- i.e. what
  -- rdf.sparql.parser's tripleTermData recognizes, used in VALUES blocks)
  -- are all `.literal(concept)`, reusing rdf.triple's concept type -- the
  -- same representation Turtle/N3 blank nodes and (now) triple terms already
  -- use, per ground triples being a subset of triple patterns.
  -- `.tripleTermPattern` here is the more general, variable-permitting form
  -- (from tripleTerm/exprTripleTerm), which concept can't express since
  -- concept has no variable case -- named to avoid colliding with
  -- rdf.triple.concept's own (ground-only) .tripleTerm.
  public term ::= .literal(concept)
  | .var(string)
  | .tripleTermPattern(term,predicate,term)
  | .reifiedTriple(term,predicate,term,option[term]).

  -- An annotation attached to the object of a triple: either a bare reifier
  -- (`~id?`) or a `{| ... |}` block giving further properties of the
  -- reification. Kept structurally rather than expanded into synthetic
  -- rdf:reifies triples here -- that expansion belongs to a later pass, once
  -- the reifier-identity conventions it depends on are settled.
  public annotationItem ::= .reifier(option[term]) | .annotationBlock(pattern).

  -- Expressions. Most built-in function calls collapse to the generic
  -- `.call(name,args)` rather than one constructor per built-in (there are
  -- ~65 of them, and they're already grouped that way on the grammar side by
  -- oneArgBuiltin/twoArgBuiltin/threeArgBuiltin/nilArgBuiltin) -- the name
  -- plus argument list is all any consumer needs to interpret the call.
  public expression ::= .term(term)
  | .or(expression,expression)
  | .and(expression,expression)
  | .eq(expression,expression)
  | .ne(expression,expression)
  | .lt(expression,expression)
  | .gt(expression,expression)
  | .le(expression,expression)
  | .ge(expression,expression)
  | .isIn(expression,cons[expression])
  | .notIn(expression,cons[expression])
  | .add(expression,expression)
  | .sub(expression,expression)
  | .mul(expression,expression)
  | .div(expression,expression)
  | .pos(expression)
  | .neg(expression)
  | .not(expression)
  | .call(string,cons[expression])
  | .aggregate(string,option[expression],boolean)
  | .groupConcat(option[expression],boolean,option[string])
  | .funcCall(term,cons[expression],boolean)
  | .bound(string)
  | .existsPattern(pattern)
  | .notExists(pattern)
  | .exprTripleTerm(expression,expression,expression).
}
