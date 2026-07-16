rdf.sparql.query{
  import star.
  import rdf.triple.

  public query ::= .select(projection,pattern,cons[modification])
  | .ask(pattern,cons[modification]).

  public pattern ::= .basic(term,term,term)
  | .filter(expression)
  | .conj(pattern,pattern)
  | .union(pattern,pattern)
  | .optional(pattern,pattern)
  | .minus(pattern,pattern).

  public term ::= .literal(concept)
  | .var(string)
  .

  public projection ::= .all | .vars(cons[expression]).
  
  
}
