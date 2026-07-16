rdf.sparql.parser{
  import star.

  import star.location.

  import rdf.errors.
  import rdf.meta.
  import rdf.token.
  import rdf.triple.

  import rdf.parser.

  import rdf.sparql.query.

  -- Parse sparql queries

  public query:() >> query --> cons[token].
  query() >> Q --> preamble >> Prefix,
  ( selectQuery(Prefix) >> Q || askQuery(Prefix) >> Q).

  selectQuery(PrMap) >> .select(Result,DataSets,Cond,Mod) -->
    keyword("query"),
    selectClause(PrMap) >> Result,
    datasetClause(PrMap) * >> DataSets, 
    whereClause(PrMap) >> Cond,
    solutionModifier(PrMap) >> Mod.

  askQuery(PrMap) >> .ask(Cond,DataSets,Mods) -->
    keyword("ask"),
    datasetClause(PrMap) * >> DataSets, 
    whereClause(PrMap) >> Cond,
    solutionModifier(PrMap) >> Mod.

  datasetClause >> Source -->
    keyword("from"),
    (defaultGraphClause >> Source || namedGraphClause >> Source).

  whereClause >> Pattern --> keyword("where"), groupGraphPattern >> Pattern.

  defaultGraphClause >> .defaultGraph(Source) --> sourceSelector >> Source.
  namedGraphClause >> .namedGraph(Source) --> keyword("named"), sourceSelector >> Source.

  groupGraphPattern >> Pattern --> punc("{"),
  (subSelect >> Pattern || groupGraphPatternSub >> Pattern ), punc("}").

  groupGraphPatternSub >> Triples ++ NotTriples ++ More -->
    ? triplesBlock >> Triples,
    (graphPatternNotTriples >> NotTriples, punc("."), triplesBlock? >> More)* >> Rest.

  triplesBlock >> Tr ++ Ipls --> triplesSameSubjectPath >> Tr,
    ? ( punc("."), ? triplesBlock >> Ts) >> Ipls.

  sourceSelector >> Source --> iri >> Source.

  iri >> U --> [.tok(_,.uriTok(U))].  
  
  
