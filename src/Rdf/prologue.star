rdf.sparql.prologue{
  import star.
  import rdf.parser.
  import rdf.sparql.query.

  /* Resolves every .named(prefix,local) term produced by rdf.sparql.parser into a proper
     .uri(...) using the prefixDict built from the query's own PREFIX declarations -- reusing
     rdf.parser's resolve, the same function the Turtle/N3 side already uses, so a query term
     and a stored graph term end up structurally comparable (.uri to .uri) instead of silently
     never matching (.named to .uri, which concept's equality never pairs up -- see triple.star).

     This walks the whole query AST exhaustively: every node that carries a term, directly or
     nested inside a pattern/expression/path, needs its own resolve* clause below. */

  public resolveQuery:(prefixDict,query) => query.
  resolveQuery(D,.select(Mod,Proj,DCs,P,Mods)) =>
    .select(Mod,resolveProjection(D,Proj),resolveDatasetClauses(D,DCs),resolvePattern(D,P),resolveSolutionMods(D,Mods)).
  resolveQuery(D,.construct(T,DCs,P,Mods)) =>
    .construct(resolvePattern(D,T),resolveDatasetClauses(D,DCs),resolvePattern(D,P),resolveSolutionMods(D,Mods)).
  resolveQuery(D,.describe(DT,DCs,WP,Mods)) =>
    .describe(resolveDescribeTargets(D,DT),resolveDatasetClauses(D,DCs),resolveOptPattern(D,WP),resolveSolutionMods(D,Mods)).
  resolveQuery(D,.ask(DCs,P,Mods)) =>
    .ask(resolveDatasetClauses(D,DCs),resolvePattern(D,P),resolveSolutionMods(D,Mods)).

  resolveOptPattern(_,.none) => .none.
  resolveOptPattern(D,.some(P)) => .some(resolvePattern(D,P)).

  resolveProjection(_,.selectAll) => .selectAll.
  resolveProjection(D,.vars(Vs)) => .vars(Vs//((V) => resolveSelectVar(D,V))).

  resolveSelectVar(_,.plain(V)) => .plain(V).
  resolveSelectVar(D,.computed(E,V)) => .computed(resolveExpr(D,E),V).

  resolveDatasetClauses(D,DCs) => (DCs//((DC) => resolveDatasetClause(D,DC))).
  resolveDatasetClause(D,.defaultGraph(T)) => .defaultGraph(resolveTerm(D,T)).
  resolveDatasetClause(D,.namedGraph(T)) => .namedGraph(resolveTerm(D,T)).

  resolveDescribeTargets(_,.allDescribed) => .allDescribed.
  resolveDescribeTargets(D,.described(Ts)) => .described(Ts//((T) => resolveTerm(D,T))).

  resolveSolutionMods(D,Mods) => solutionMods{
    grouping = Mods.grouping//((G) => resolveGroupCondition(D,G)).
    having = Mods.having//((E) => resolveExpr(D,E)).
    ordering = Mods.ordering//((O) => resolveOrderCondition(D,O)).
    limit = Mods.limit.
    offset = Mods.offset.
  }.

  resolveGroupCondition(D,.groupExpr(E,Alias)) => .groupExpr(resolveExpr(D,E),Alias).

  resolveOrderCondition(D,.asc(E)) => .asc(resolveExpr(D,E)).
  resolveOrderCondition(D,.desc(E)) => .desc(resolveExpr(D,E)).

  resolvePattern(D,.basic(S,P,O)) => .basic(resolveTerm(D,S),resolvePredicate(D,P),resolveTerm(D,O)).
  resolvePattern(D,.filter(E)) => .filter(resolveExpr(D,E)).
  resolvePattern(D,.bind(E,V)) => .bind(resolveExpr(D,E),V).
  resolvePattern(D,.values(DB)) => .values(resolveDataBlock(D,DB)).
  resolvePattern(D,.conj(A,B)) => .conj(resolvePattern(D,A),resolvePattern(D,B)).
  resolvePattern(D,.union(A,B)) => .union(resolvePattern(D,A),resolvePattern(D,B)).
  resolvePattern(D,.optional(A,B)) => .optional(resolvePattern(D,A),resolvePattern(D,B)).
  resolvePattern(D,.minus(A,B)) => .minus(resolvePattern(D,A),resolvePattern(D,B)).
  resolvePattern(D,.graph(T,P)) => .graph(resolveTerm(D,T),resolvePattern(D,P)).
  resolvePattern(D,.service(T,P,Silent)) => .service(resolveTerm(D,T),resolvePattern(D,P),Silent).
  resolvePattern(_,.nilPattern) => .nilPattern.
  resolvePattern(D,.subSelect(Q)) => .subSelect(resolveQuery(D,Q)).
  resolvePattern(D,.annotated(P,As)) => .annotated(resolvePattern(D,P),As//((A) => resolveAnnotationItem(D,A))).

  resolveAnnotationItem(D,.reifier(R)) => .reifier(resolveOptTerm(D,R)).
  resolveAnnotationItem(D,.annotationBlock(P)) => .annotationBlock(resolvePattern(D,P)).

  resolveOptTerm(_,.none) => .none.
  resolveOptTerm(D,.some(T)) => .some(resolveTerm(D,T)).

  resolvePredicate(D,.simple(T)) => .simple(resolveTerm(D,T)).
  resolvePredicate(D,.path(P)) => .path(resolvePath(D,P)).

  resolvePath(D,.predicate(T)) => .predicate(resolveTerm(D,T)).
  resolvePath(D,.inverse(P)) => .inverse(resolvePath(D,P)).
  resolvePath(D,.seq(A,B)) => .seq(resolvePath(D,A),resolvePath(D,B)).
  resolvePath(D,.alt(A,B)) => .alt(resolvePath(D,A),resolvePath(D,B)).
  resolvePath(D,.mod(P,M)) => .mod(resolvePath(D,P),M).
  resolvePath(D,.negated(Is)) => .negated(Is//((I) => resolveNegatedPathItem(D,I))).
  resolvePath(D,.group(P)) => .group(resolvePath(D,P)).

  resolveNegatedPathItem(D,.fwd(T)) => .fwd(resolveTerm(D,T)).
  resolveNegatedPathItem(D,.inv(T)) => .inv(resolveTerm(D,T)).

  public resolveTerm:(prefixDict,term) => term.
  resolveTerm(D,.literal(C)) => .literal(resolve(C,D)).
  resolveTerm(_,.var(V)) => .var(V).
  resolveTerm(D,.tripleTermPattern(S,P,O)) => .tripleTermPattern(resolveTerm(D,S),resolvePredicate(D,P),resolveTerm(D,O)).
  resolveTerm(D,.reifiedTriple(S,P,O,R)) => .reifiedTriple(resolveTerm(D,S),resolvePredicate(D,P),resolveTerm(D,O),resolveOptTerm(D,R)).

  resolveDataBlock(D,.oneVar(V,Vals)) => .oneVar(V,resolveOptTerms(D,Vals)).
  resolveDataBlock(D,.full(Vars,Rows)) => .full(Vars,(Rows//((Row) => resolveOptTerms(D,Row)))).

  resolveOptTerms(D,Vals) => (Vals//((Vl) => resolveOptTerm(D,Vl))).

  public resolveExpr:(prefixDict,expression) => expression.
  resolveExpr(D,.term(T)) => .term(resolveTerm(D,T)).
  resolveExpr(D,.or(A,B)) => .or(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.and(A,B)) => .and(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.eq(A,B)) => .eq(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.ne(A,B)) => .ne(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.lt(A,B)) => .lt(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.gt(A,B)) => .gt(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.le(A,B)) => .le(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.ge(A,B)) => .ge(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.isIn(A,Bs)) => .isIn(resolveExpr(D,A),Bs//((B) => resolveExpr(D,B))).
  resolveExpr(D,.notIn(A,Bs)) => .notIn(resolveExpr(D,A),Bs//((B) => resolveExpr(D,B))).
  resolveExpr(D,.add(A,B)) => .add(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.sub(A,B)) => .sub(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.mul(A,B)) => .mul(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.div(A,B)) => .div(resolveExpr(D,A),resolveExpr(D,B)).
  resolveExpr(D,.pos(A)) => .pos(resolveExpr(D,A)).
  resolveExpr(D,.neg(A)) => .neg(resolveExpr(D,A)).
  resolveExpr(D,.not(A)) => .not(resolveExpr(D,A)).
  resolveExpr(D,.call(Nm,Args)) => .call(Nm,Args//((A) => resolveExpr(D,A))).
  resolveExpr(D,.aggregate(Nm,Arg,Dist)) => .aggregate(Nm,resolveOptExpr(D,Arg),Dist).
  resolveExpr(D,.groupConcat(Arg,Dist,Sep)) => .groupConcat(resolveOptExpr(D,Arg),Dist,Sep).
  resolveExpr(D,.funcCall(T,Args,Dist)) => .funcCall(resolveTerm(D,T),Args//((A) => resolveExpr(D,A)),Dist).
  resolveExpr(_,.bound(V)) => .bound(V).
  resolveExpr(D,.existsPattern(P)) => .existsPattern(resolvePattern(D,P)).
  resolveExpr(D,.notExists(P)) => .notExists(resolvePattern(D,P)).
  resolveExpr(D,.exprTripleTerm(S,P,O)) => .exprTripleTerm(resolveExpr(D,S),resolveExpr(D,P),resolveExpr(D,O)).

  resolveOptExpr(_,.none) => .none.
  resolveOptExpr(D,.some(E)) => .some(resolveExpr(D,E)).
}
