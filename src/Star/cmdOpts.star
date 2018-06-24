star.cmdOpts{
  import star.
  import star.either.

  -- Process command line list of strings to produce a set of Options
  public all o ~~ optionsProcessor[o] <~ {
    shortForm : string.
    alternatives : list[string].
    usage : string.
    validator : option[(string)=>boolean].
    setOption: (string,o) => o.
  }

  public processOptions:all o ~~ (list[string],list[optionsProcessor[o]],o) =>
    either[(o,list[string]),string].
  processOptions(Raw,Specs,Opts) => processAll(Raw,Specs,Opts).

  processAll:all o ~~ (list[string],list[optionsProcessor[o]],o) => either[(o,list[string]),string].
  processAll([],_,SoFar) => either((SoFar,[])).
  processAll(["--",..A],_,SoFar) => either((SoFar,A)).
  processAll([A,..L],Specs,SoFar) => processOption(A,L,Specs,SoFar).

  processOption:all o ~~ (string,list[string],list[optionsProcessor[o]],o) => either[(o,list[string]),string].
  processOption(A,L,Specs,SoFar) => checkOption(L,O.shortForm,O.validator,O.setOption,Specs,SoFar) :-
    (listEl(O,Specs), (A==O.shortForm | A in O.alternatives))!.
  processOption(A,L,_,SoFar) => either((SoFar,[A,..L])).

  checkOption:all o ~~ (list[string],string,option[(string){}],(string,o) => o,list[optionsProcessor[o]],o) =>
    either[(o,list[string]),string].
  checkOption(Args,O,none,setter,Specs,SoFar) => processAll(Args,Specs,setter(O,SoFar)).
  checkOption([A,..Args],O,some(V),Setter,Specs,SoFar) =>
        processAll(Args,Specs,Setter(A,SoFar)) :- V(A).
  checkOption(_,_,_,_,Specs,_) => other(collectUsage(Specs)).

  collectUsage:all o ~~ (list[optionsProcessor[o]]) => string.
  collectUsage(Specs) => formatSS(ssSeq([ss("Usage: \n"),..interleave(Specs//((O)=>O.usage),"\n")//((X)=>ss(X))])).
}
