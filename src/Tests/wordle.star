test.wordle{
  import star.
  import star.io.
  import star.mbox.
  import star.assert.
  import star.sort.

  import test.lib.text.

  parseWordFile:(string)=>cons[cons[char]] throws ioException.
  parseWordFile(Fn) => valof{
    Lns = readLines(Fn);

    valis (Lns//explode)
  }

  result ::= .grey(char,integer) | .yellow(char,integer) | .green(char,integer).

  indexOf(.grey(_,Ix)) => Ix.
  indexOf(.yellow(_,Ix)) => Ix.
  indexOf(.green(_,Ix)) => Ix.

  compResult(R1,R2) => indexOf(R1)<indexOf(R2).

  implementation display[result] => {
    disp(.grey(Ch,Ix)) => "Grey\e[47m#(Ch::string)\e[0m($(Ix))".
    disp(.yellow(Ch,Ix)) => "Yellow\e[43m#(Ch::string)\e[0m($(Ix))".
    disp(.green(Ch,Ix)) => "Green\e[42m#(Ch::string)\e[0m($(Ix))".
  }

  score:(cons[char],cons[char])=>cons[result].
  score(S,G) => valof{
    (Gn,Ss,Gs) = greenScore(S,zip(G,iota(0,size(G))),[],[],[]);
    show Gn;
    show Ss;
    show Gs;
    Score = sort(yellowScore(Gs,Ss,Gn),compResult);
    show Score;
    valis Score
  }

  greenScore:(cons[char],cons[(char,integer)],cons[result],cons[char],cons[(char,integer)]) =>
    (cons[result],cons[char],cons[(char,integer)]).
  greenScore([],[],Grn,Ss,Gs) => (Grn,Ss,Gs).
  greenScore([C,..Cs],[(C,Ix),..Rs],Grn,Ss,Gs) =>
    greenScore(Cs,Rs,[.green(C,Ix),..Grn],Ss,Gs).
  greenScore([S,..Sx],[G,..Gx],Grn,Ss,Gs) =>
    greenScore(Sx,Gx,Grn,[S,..Ss],[G,..Gs]).

  yellowScore:(cons[(char,integer)],cons[char],cons[result]) =>
    (cons[result]).
  yellowScore([(Ch,Ix),..Gs],S,Sc) where Ch .<. S =>
    yellowScore(Gs,S\-Ch,[.yellow(Ch,Ix),..Sc]).
  yellowScore([(C,Ix),..Gs],S,Sc) =>
    yellowScore(Gs,S,[.grey(C,Ix),..Sc]).
  yellowScore([],_,Sc) => Sc.
      
  main:()=>().
  main() => valof{
    try{
      Words = parseWordFile("wordle.txt");
      Cnt = [|Words|];
      if Guess ?= Words[_irand(Cnt)] &&
	  Secret ?= Words[_irand(Cnt)] then{
	show Guess;
	show Secret;
	show score(Secret,Guess);
	  }
    } catch {
      M => showMsg("We got an exception: $(M)")
    };
    valis ()
  }
}
    
