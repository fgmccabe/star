rdf.types{
  import star.

  public concept ::= concept(string) | text(string).
  public triple ::=  triple(concept,concept,concept).
  public graph ::= graph(concept,set[triple]).

  public implementation display[concept] => {.
    disp(concept(T)) => ss(T).
    disp(text(T)) => disp(T).
  .}
  
  public implementation display[triple] => {.
    disp(triple(S,P,O)) =>
      ssSeq([disp(S),ss("$"),disp(P),ss("!"),disp(O),ss(".")]).
  .}

  public implementation display[graph] => {.
    disp(graph(Nm,Trps)) => ssSeq([
	disp(Nm),ss("{"),
	ssSeq(foldLeft((Tr,X)=>[disp(Tr),..X],[],Trps)),
	ss("}")]).
  .}

}
