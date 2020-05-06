rdf.graph{
  import star.

  public concept ::= concept(string) | text(string).
  public triple ::=  triple(concept,concept,concept).
  public graph <~ {}.

  contract all t,e ~~ '$name'[t->>e] ::= {
    '$name':(t)=>e
  }

  implementation '$name'[graph->>concept] => {.
    '$name'(graph(_,Nm,_,_,_,_)) => Nm
  .}

  graph:(
    integer,
    concept,
    map[integer,triple],
    map[string,integer],
    map[string,integer],
    map[string,integer]) <=> graph.

  public implementation display[concept] => {.
    disp(concept(T)) => ss(T).
    disp(text(T)) => disp(T).
  .}
  
  public implementation display[triple] => {.
    disp(triple(S,P,O)) =>
      ssSeq([disp(S),ss("$"),disp(P),ss("!"),disp(O),ss(".")]).
  .}

  public implementation display[graph] => {.
    disp:(graph)=>ss.
    disp(G) => ssSeq([
	disp(G.name),
	ss("{"),
	ssSeq(ixLeft((_,Tr,X)=>[disp(Tr),..X],[],G.triples)),
	ss("}")]).
  .}

/*  public addTriple:(graph,concept,concept,concept)=>graph.
  addTriple(G,S,P,O) where 
    Nx .= G.count+1;
    S1 .= addToIndex(G.subjects,S,Nx);
    P1 .= addToIndex(G.predicates,P,Nx);
    O1 .= addToIndex(G.objects,O,Nx);
  */  
  

}
