test.wordle{
  import star.
  import star.io.
  import star.mbox.
  import star.assert.
  import star.sort.
  import star.system.

  import test.lib.text.

  parseWordFile:(string)=>cons[string] throws ioException.
  parseWordFile(Fn) => readLines(Fn).

  result ::= .grey(char,integer) | .yellow(char,integer) | .green(char,integer).

  indexOf(.grey(_,Ix)) => Ix.
  indexOf(.yellow(_,Ix)) => Ix.
  indexOf(.green(_,Ix)) => Ix.

  compResult(R1,R2) => indexOf(R1)<indexOf(R2).

  implementation display[result] => {
    disp(.grey(Ch,Ix)) => "\e[107m #(Ch::string) \e[0m".
    disp(.yellow(Ch,Ix)) => "\e[103m #(Ch::string) \e[0m".
    disp(.green(Ch,Ix)) => "\e[102m #(Ch::string) \e[0m".
  }

  showScore:(cons[result])=>string.
  showScore(Sc) => (Sc//disp)*.

  score:(cons[char],cons[char])=>string.
  score(S,G) => valof{
    (Gn,Ss,Gs) = greenScore(S,zip(G,iota(0,size(G))),[],[],[]);
    valis showScore(sort(yellowScore(Gs,Ss,Gn),compResult));
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

  -- yellows:(cons[(char,integer)],cons[char],cons[result]) => cons[result].
  -- yellows(GCodes,SChars,SoFar) =>
  --   foldLeft((((Ch,Ix),(SChrs,Sc)) =>
  --     (Ch.<.SChrs ??
  --     (SChrs\-Ch,[.yellow(Ch,Ix),..Sc]) ||
  -- 	(SChrs,[.grey(Ch,Ix),..Sc]))),
  --   (SChars,SoFar),
  --   GCodes).

  filterGuesses:(string,string,cons[string])=>cons[string].
  filterGuesses(Guess,Score,Words) => 
    { C | C in Words && score(C::cons[char],Guess::cons[char])==Score }.

  makeAGuess(Words) => _optval(Words[_irand([|Words|])]).

  autoplay:(string,cons[string]) => integer.
  autoplay(Secret,Words) => valof{
    WordCnt = [|Words|];
    SecretChars = Secret::cons[char];
      
    Cnt := 0;
    Possibles := Words;
    while Cnt!<6 && ~isEmpty(Possibles!) do{
      Cnt := Cnt!+1;
      Guess = makeAGuess(Possibles!);
      Score = score(SecretChars,Guess::cons[char]);
      showMsg("Guess $(Cnt!)\: #(Score)");
      if Guess==Secret then{
	showMsg("Success in $(Cnt!) goes");
	valis Cnt!
      }
      else{
	Possibles := filterGuesses(Guess,Score,Possibles!);
      }
    };
    showMsg("Failed after $(Cnt!) goes");
    valis Cnt!
  }

  readGuess:() => string throws ioException.
  readGuess() => valof{
    wrText(stdout,"Next: ");
    _flushall();
    valis rdLine(stdin)
  }

  play:(string,cons[string]) => integer.
  play(Secret,Words) => valof{
    Cnt := 0;
    SecretChars = Secret::cons[char];
    while Cnt! < 7 do{
      try{
	Guess = readGuess();
	Score = score(SecretChars,Guess::cons[char]);
	Cnt := Cnt!+1;
	showMsg("Guess: $(Cnt!)\:#(Score)");
	if Guess==Secret then{
	  showMsg("Success in $(Cnt!) goes");
	  valis Cnt!
	}
      } catch {
	| .pastEof => {
	  showMsg("Ending after $(Cnt!) moves");
	  exit(0)
	}
	| E => {
	  showMsg("IO error: $(E)");
	  valis 0
	}
      }
    };
    showMsg("Failed after $(Cnt!) goes");
    valis Cnt!
  }
         
  main:()=>().
  main() => valof{
    try{
      Words = parseWordFile("wordle.txt");
      _seed(_ticks());
      Cnt = [|Words|];
      
      if Secret ?= Words[_irand(Cnt)] then{
	autoplay(Secret,Words);
--	play(Secret,Words);
      }
    } catch {
      M => showMsg("We got an exception: $(M)")
    };
    valis ()
  }
}
    
