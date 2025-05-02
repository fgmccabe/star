star.cmdOpts{
  import star.
  import star.sort.

  -- Process command line list of strings to produce a set of Options
  public all o ~~ cmdOption[o] ::= cmdOption{
    shortForm : string.
    alternatives : cons[string].
    usage : string.
    validator : option[(string)=>boolean].
    setOption: (string,o) => o.
  }

  public processOptions:all o ~~ (cons[string],cons[cmdOption[o]],o) =>
    (o,cons[string]) throws string.
  processOptions(Raw,Specs,Opts) => processAll(Raw,Specs,Opts).

  processAll:all o ~~ (cons[string],cons[cmdOption[o]],o) => (o,cons[string]) throws string.
  processAll([],_,SoFar) => (SoFar,[]).
  processAll(["--",..A],_,SoFar) => (SoFar,A).
  processAll([A,..L],Specs,SoFar) => processOption(A,L,Specs,SoFar).

  processOption:all o ~~ (string,cons[string],cons[cmdOption[o]],o) =>
    (o,cons[string]) throws string.
  processOption("-h",L,Specs,SoFar) => throw collectUsage(Specs).
  processOption("--help",L,Specs,SoFar) => throw collectUsage(Specs).
  processOption(A,L,Specs,SoFar) where
      O ?= search(Specs,(e)=>(e.shortForm==A|| _ ?=search(e.alternatives,(o)=>o==A))) => 
    checkOption(L,O.shortForm,O.validator,O.setOption,Specs,SoFar).
  processOption(A,_,Specs,_) where strPrefix("-",A) =>
    throw "Invalid option: $(A)\n#(collectUsage(Specs))".
  processOption(A,L,_,SoFar) => (SoFar,[A,..L]).

  checkOption:all o ~~ (cons[string],string,option[(string)=>boolean],(string,o) => o,cons[cmdOption[o]],o) =>
    (o,cons[string]) throws string.
  checkOption(Args,O,.none,setter,Specs,SoFar) => processAll(Args,Specs,setter(O,SoFar)).
  checkOption([A,..Args],O,.some(V),Setter,Specs,SoFar)
      where V(A) => processAll(Args,Specs,Setter(A,SoFar)).
  checkOption(_,_,_,_,Specs,_) => throw collectUsage(Specs).

  collectUsage:all o ~~ (cons[cmdOption[o]]) => string.
  collectUsage(Specs) => "Usage: \n#(interleave(sort(Specs,compareFlag)//((O)=>O.usage),"\n")*)".

  compareFlag:all e ~~ (cmdOption[e],cmdOption[e])=>boolean.
  compareFlag(O1,O2) => O1.shortForm<O2.shortForm.
}
