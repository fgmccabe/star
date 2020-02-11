test.mastermind{
  import star.

  color ::= blue |
    green |
    purple |
    red |
    white |
    yellow.

  score ::= score{white:integer. black:integer}.

  score:(list[color],list[color])=>score.
  score(Secret,Guess) => let{
    blacks([],[],Cnt,SoFar) => (Cnt,SoFar).
    blacks([C,..Ss],[C,..Gs],Cnt,SoFar) =>
      blacks(Ss,Gs,Cnt+1,SoFar).
    blacks([SC,..Ss],[GC,..Gs],Cnt,(S,G)) =>
      blacks(Ss,Gs,Cnt,(incColor(S,SC),incColor(G,GC))).
  } in score{white=0. black=blacks
    
