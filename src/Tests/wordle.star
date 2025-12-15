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

  color ::= .grey | .yellow | .green.

  coloredChar:(color,char)=>string.
  coloredChar(.grey,Ch) => "\e[107m #(Ch::string) \e[0m".
  coloredChar(.green,Ch) => "\e[102m #(Ch::string) \e[0m".
  coloredChar(.yellow,Ch) => "\e[103m #(Ch::string) \e[0m".

  result ::= result{
    ch:char.
    index:integer.
    col:color
  }.

  implementation display[result] => {
    disp(result{ch=Ch. col=Col}) => coloredChar(Col,Ch)
  }

  showScore:(cons[result])=>string.
  showScore(Sc) => (Sc//disp)*.

  score:(string,string)=>string.
  score(Secret,Guess) => valof{
    S = explode(Secret);
    G = explode(Guess);
    Ix = iota(0,size(G));
    Grns = greens(S,G,Ix);
    (RestSecret,RestGuess,RestIx) = unzip3(nongreens(S,G,Ix));
    Ylws = yellows(RestSecret,RestGuess,RestIx);
    Score = sort(Grns++Ylws,compIndex);
    valis showScore(Score)
  }

  winningScore:(cons[result]) => boolean.
  winningScore([]) => .true.
  winningScore([result{col=.green},..Rest]) =>
    winningScore(Rest).
  winningScore(_) default => .false.

  scoreWord:(cons[result])=>string.
  scoreWord(Sc) => (Sc//((result{ch=C})=>C))::string.

  compIndex:all x,y ~~ x<~{index:integer}, y<~{index:integer} |= (x,y)=>boolean.
  compIndex(X,Y) => X.index<Y.index.

  greens:(cons[char],cons[char],cons[integer]) => cons[result].
  greens(Secret,Guess,Index) =>
    { result{index=X. col=.green. ch=C} | (C,C,X) in zip3(Secret,Guess,Index)}.

  nongreens:(cons[char],cons[char],cons[integer]) => cons[(char,char,integer)].
  nongreens(Secret,Guess,Index) =>
    { (S,G,X) | (S,G,X) in zip3(Secret,Guess,Index) && S~=G}.

  yellows:(cons[char],cons[char],cons[integer]) => cons[result].
  yellows(Se,Gu,Ix) => collect{
    SecretChars := Se;
    for (Ch,X) in zip(Gu,Ix) do{
      if Ch.<.SecretChars! then{
	SecretChars := SecretChars!\-Ch;
	elemis result{col=.yellow. ch=Ch. index=X}
      } else
      elemis result{col=.grey. ch=Ch. index=X}
    }}

  filterGuesses:(string,string,cons[string])=>cons[string].
  filterGuesses(Guess,Score,Words) => 
    { C | C in Words && score(C,Guess)==Score }.

  makeAGuess(Words) => _optval(Words[_irand([|Words|])]).

  autoplay:(string,cons[string]) => integer.
  autoplay(Secret,Words) => valof{
    Cnt := 0;
    Possibles := Words;
    while Cnt!<6 && ~isEmpty(Possibles!) do{
      Cnt := Cnt!+1;
      Guess = makeAGuess(Possibles!);
      Score = score(Secret,Guess);
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

  readGuess:(cons[string]) => string throws ioException.
  readGuess(Words) => valof{
    while .true do{
      wrText(stdout,"Next: ");
      _flushall();
      if W.=rdLine(stdin) && W.<.Words then
	valis W
      else
	wrText(stdout,"Not a known word\n");
    };
    throw .ioError
  }

  play:(string,cons[string]) => integer.
  play(Secret,Words) => valof{
    Cnt := 0;
    while Cnt! < 6 do{
      try{
	Guess = readGuess(Words);
	Score = score(Secret,Guess);
	Cnt := Cnt!+1;
	showMsg("$(Cnt!)\:#(Score)");
	if Guess==Secret then{
	  showMsg("Success in $(Cnt!) goes");
	  valis Cnt!
	}
      } catch {
	| .pastEof do {
	  showMsg("Ending after $(Cnt!) moves");
	  exit(0)
	}
	| E do {
	  showMsg("IO error: $(E)");
	  valis 0
	}
      }
    };
    showMsg("Failed after $(Cnt!) goes\nSecret was #(Secret)");
    valis Cnt!
  }

  playerone:()=>integer.
  playerone() => valof{
    try{
      Words = parseWordFile("wordle.txt");
      _seed(_ticks());
      Cnt = [|Words|];
      
      if Secret ?= Words[_irand(Cnt)] then{
	play(Secret,Words);
      }
    } catch {
      M do showMsg("We got an exception: $(M)")
    };
    valis 0
  }
  
  playertwo:(string)=>integer.
  playertwo(Secret) => valof{
    try{
      Words = parseWordFile("wordle.txt");
      autoplay(Secret,Words)
    } catch {
      M do showMsg("We got an exception: $(M)")
    };
    valis 0
  }

  askNextGuess:() => cons[char] throws ioException.
  askNextGuess() => valof{
    wrText(stdout,"Score? ");
    _flushall();
    valis rdLine(stdin)::cons[char]
  }

  -- Parse a score from user input
  parseScore:(integer) >> cons[result] --> cons[char].
  parseScore(_) >> [] --> end.
  parseScore(Cx) >> [Sc,..Rest] --> letterScore(Cx) >> Sc, parseScore(Cx+1)>>Rest.

  letterScore(Cx) >> result{ch=C. col=.yellow. index=Cx} -->
    [`.`,`y`], [C], {isLetter(C)}.
  letterScore(Cx) >> result{ch=C. col=.green. index=Cx} -->
    [`.`,`g`], [C], {isLetter(C)}.
  letterScore(Cx) >> result{ch=C. col=.grey. index=Cx} --> [C], {isLetter(C)}.

  makeGuesses:(cons[string])=>integer.
  makeGuesses(Words) => valof{
    Cnt := 0;
    Possibles := Words;
    while Cnt!<6 && ~isEmpty(Possibles!) do{
      try{
	if Score ?= (parseScore(0) --> askNextGuess()) then{
	  Cnt := Cnt!+1;
	  word = scoreWord(Score);
	  if winningScore(Score) then{
	    showMsg("Success in $(Cnt!) goes");
	    valis Cnt!
	  }
	  else{
	    Possibles := filterGuesses(word,showScore(Score),Possibles!);
	    showMsg("$([|Possibles!|]) choices left");
	    showMsg("I suggest #(makeAGuess(Possibles!))");
	  }
	}
      } catch{
	_ do showMsg("some kind of error")
      }
    };
    showMsg("Failed after $(Cnt!) goes");
    valis Cnt!
  }

  coach:()=>integer.
  coach() => valof{
    try{
      Words = parseWordFile("wordle.txt");
      makeGuesses(Words)
    } catch {
      M do showMsg("We got an exception: $(M)")
    };
    valis 0
  }    

  _main:(cons[string])=>integer.
  _main(["--coach"]) => coach().
  _main(["--play"]) => playerone().
  _main([S,G]) => valof{ showMsg(score(S,G)); valis 0}.
  _main([S]) => playertwo(S).
  _main([]) => valof{ showMsg(score("skate","melon")); valis 0}.
}
    
