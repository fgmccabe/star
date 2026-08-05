rdf.sparql.query{
  import star.
  import rdf.triple.

  /* The SPARQL query AST. Extends the original query/pattern/term/projection
     skeleton to cover the full grammar in rdf.sparql.parser: construct/
     describe query forms, property paths, RDF-star (reified triples/triple
     terms/annotations/reifiers), and the full expression/built-in surface.

     Every type here implements `display` right below its declaration, purely
     for debugging (e.g. in error messages or ad hoc `$(...)` interpolation
     while working on the parser/a future interpreter) -- it's not meant to
     round-trip back to SPARQL syntax. */

  public query ::= .select(selectModifier,projection,cons[datasetClause],pattern,solutionMods)
  | .construct(pattern,cons[datasetClause],pattern,solutionMods)
  | .describe(describeTargets,cons[datasetClause],option[pattern],solutionMods)
  | .ask(cons[datasetClause],pattern,solutionMods).

  public implementation display[query] => {
    disp(.select(Mod,Proj,DCs,P,Mods)) => "select($(Mod),$(Proj),$(DCs),$(P),$(Mods))".
    disp(.construct(T,DCs,P,Mods)) => "construct($(T),$(DCs),$(P),$(Mods))".
    disp(.describe(DT,DCs,WP,Mods)) => "describe($(DT),$(DCs),$(WP),$(Mods))".
    disp(.ask(DCs,P,Mods)) => "ask($(DCs),$(P),$(Mods))".
  }

  public selectModifier ::= .noModifier | .distinct | .reduced.

  public implementation display[selectModifier] => {
    disp(.noModifier) => "noModifier".
    disp(.distinct) => "distinct".
    disp(.reduced) => "reduced".
  }

  public projection ::= .selectAll | .vars(cons[selectVar]).

  public implementation display[projection] => {
    disp(.selectAll) => "selectAll".
    disp(.vars(Vs)) => "vars($(Vs))".
  }

  public selectVar ::= .plain(string) | .computed(expression,string).

  public implementation display[selectVar] => {
    disp(.plain(V)) => "plain($(V))".
    disp(.computed(E,V)) => "computed($(E),$(V))".
  }

  public describeTargets ::= .allDescribed | .described(cons[term]).

  public implementation display[describeTargets] => {
    disp(.allDescribed) => "allDescribed".
    disp(.described(Ts)) => "described($(Ts))".
  }

  public datasetClause ::= .defaultGraph(term) | .namedGraph(term).

  public implementation display[datasetClause] => {
    disp(.defaultGraph(T)) => "defaultGraph($(T))".
    disp(.namedGraph(T)) => "namedGraph($(T))".
  }

  public solutionMods ::= solutionMods{
    grouping:cons[groupCondition].
    having:cons[expression].
    ordering:cons[orderCondition].
    limit:option[integer].
    offset:option[integer].
  }.

  public implementation display[solutionMods] => {
    disp(M) => "solutionMods(grouping=$(M.grouping),having=$(M.having),ordering=$(M.ordering)," ++
      "limit=$(M.limit),offset=$(M.offset))"
  }

  public noMods:()=>solutionMods.
  noMods() => solutionMods{grouping=[]. having=[]. ordering=[]. limit=.none. offset=.none}.

  public groupCondition ::= .groupExpr(expression,option[string]).

  public implementation display[groupCondition] => {
    disp(.groupExpr(E,Alias)) => "groupExpr($(E),$(Alias))".
  }

  public orderCondition ::= .asc(expression) | .desc(expression).

  public implementation display[orderCondition] => {
    disp(.asc(E)) => "asc($(E))".
    disp(.desc(E)) => "desc($(E))".
  }

  public dataBlock ::= .oneVar(string,cons[option[term]]) | .full(cons[string],cons[cons[option[term]]]).

  public implementation display[dataBlock] => {
    disp(.oneVar(V,Vals)) => "oneVar($(V),$(Vals))".
    disp(.full(Vars,Rows)) => "full($(Vars),$(Rows))".
  }

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

  public implementation display[pattern] => {
    disp(.basic(S,P,O)) => "basic($(S),$(P),$(O))".
    disp(.filter(E)) => "filter($(E))".
    disp(.bind(E,V)) => "bind($(E),$(V))".
    disp(.values(D)) => "values($(D))".
    disp(.conj(A,B)) => "conj($(A),$(B))".
    disp(.union(A,B)) => "union($(A),$(B))".
    disp(.optional(A,B)) => "optional($(A),$(B))".
    disp(.minus(A,B)) => "minus($(A),$(B))".
    disp(.graph(T,P)) => "graph($(T),$(P))".
    disp(.service(T,P,Silent)) => "service($(T),$(P),$(Silent))".
    disp(.nilPattern) => "nilPattern".
    disp(.subSelect(Q)) => "subSelect($(Q))".
    disp(.annotated(P,As)) => "annotated($(P),$(As))".
  }

  /* A triple's predicate position: either a plain term (non-path context,
     e.g. inside a construct template) or a full property path. */
  public predicate ::= .simple(term) | .path(path).

  public implementation display[predicate] => {
    disp(.simple(T)) => "simple($(T))".
    disp(.path(P)) => "path($(P))".
  }

  -- Property paths
  public path ::= .predicate(term)
  | .inverse(path)
  | .seq(path,path)
  | .alt(path,path)
  | .mod(path,pathMod)
  | .negated(cons[negatedPathItem])
  | .group(path).

  public implementation display[path] => {
    disp(.predicate(T)) => "predicate($(T))".
    disp(.inverse(P)) => "inverse($(P))".
    disp(.seq(A,B)) => "seq($(A),$(B))".
    disp(.alt(A,B)) => "alt($(A),$(B))".
    disp(.mod(P,M)) => "mod($(P),$(M))".
    disp(.negated(Is)) => "negated($(Is))".
    disp(.group(P)) => "group($(P))".
  }

  public pathMod ::= .pOptional | .pStar | .pPlus.

  public implementation display[pathMod] => {
    disp(.pOptional) => "pOptional".
    disp(.pStar) => "pStar".
    disp(.pPlus) => "pPlus".
  }

  public negatedPathItem ::= .fwd(term) | .inv(term).

  public implementation display[negatedPathItem] => {
    disp(.fwd(T)) => "fwd($(T))".
    disp(.inv(T)) => "inv($(T))".
  }

  /* RDF terms as they appear in triple patterns/expressions. Ground values
     (IRIs, literals, blank nodes, and fully-ground triple terms -- i.e. what
     rdf.sparql.parser's tripleTermData recognizes, used in VALUES blocks)
     are all `.literal(concept)`, reusing rdf.triple's concept type -- the
     same representation Turtle/N3 blank nodes and (now) triple terms already
     use, per ground triples being a subset of triple patterns.
     `.tripleTermPattern` here is the more general, variable-permitting form
     (from tripleTerm/exprTripleTerm), which concept can't express since
     concept has no variable case -- named to avoid colliding with
     rdf.triple.concept's own (ground-only) .tripleTerm. */
  public term ::= .literal(concept)
  | .var(string)
  | .tripleTermPattern(term,predicate,term)
  | .reifiedTriple(term,predicate,term,option[term]).

  public implementation display[term] => {
    disp(.literal(C)) => "literal($(C))".
    disp(.var(V)) => "var($(V))".
    disp(.tripleTermPattern(S,P,O)) => "tripleTermPattern($(S),$(P),$(O))".
    disp(.reifiedTriple(S,P,O,R)) => "reifiedTriple($(S),$(P),$(O),$(R))".
  }

  /* An annotation attached to the object of a triple: either a bare reifier
     (`~id?`) or a `{| ... |}` block giving further properties of the
     reification. Kept structurally rather than expanded into synthetic
     rdf:reifies triples here -- that expansion belongs to a later pass, once
     the reifier-identity conventions it depends on are settled. */
  public annotationItem ::= .reifier(option[term]) | .annotationBlock(pattern).

  public implementation display[annotationItem] => {
    disp(.reifier(R)) => "reifier($(R))".
    disp(.annotationBlock(P)) => "annotationBlock($(P))".
  }

  /* Expressions. Most built-in function calls collapse to the generic
     `.call(name,args)` rather than one constructor per built-in (there are
     ~65 of them, and they're already grouped that way on the grammar side by
     oneArgBuiltin/twoArgBuiltin/threeArgBuiltin/nilArgBuiltin) -- the name
     plus argument list is all any consumer needs to interpret the call. */
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

  public implementation display[expression] => {
    disp(.term(T)) => "term($(T))".
    disp(.or(A,B)) => "or($(A),$(B))".
    disp(.and(A,B)) => "and($(A),$(B))".
    disp(.eq(A,B)) => "eq($(A),$(B))".
    disp(.ne(A,B)) => "ne($(A),$(B))".
    disp(.lt(A,B)) => "lt($(A),$(B))".
    disp(.gt(A,B)) => "gt($(A),$(B))".
    disp(.le(A,B)) => "le($(A),$(B))".
    disp(.ge(A,B)) => "ge($(A),$(B))".
    disp(.isIn(A,Bs)) => "isIn($(A),$(Bs))".
    disp(.notIn(A,Bs)) => "notIn($(A),$(Bs))".
    disp(.add(A,B)) => "add($(A),$(B))".
    disp(.sub(A,B)) => "sub($(A),$(B))".
    disp(.mul(A,B)) => "mul($(A),$(B))".
    disp(.div(A,B)) => "div($(A),$(B))".
    disp(.pos(A)) => "pos($(A))".
    disp(.neg(A)) => "neg($(A))".
    disp(.not(A)) => "not($(A))".
    disp(.call(Nm,Args)) => "call($(Nm),$(Args))".
    disp(.aggregate(Nm,Arg,Dist)) => "aggregate($(Nm),$(Arg),$(Dist))".
    disp(.groupConcat(Arg,Dist,Sep)) => "groupConcat($(Arg),$(Dist),$(Sep))".
    disp(.funcCall(T,Args,Dist)) => "funcCall($(T),$(Args),$(Dist))".
    disp(.bound(V)) => "bound($(V))".
    disp(.existsPattern(P)) => "existsPattern($(P))".
    disp(.notExists(P)) => "notExists($(P))".
    disp(.exprTripleTerm(S,P,O)) => "exprTripleTerm($(S),$(P),$(O))".
  }
}
