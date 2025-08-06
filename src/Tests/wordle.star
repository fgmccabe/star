test.wordle{
  import star.
  import star.io.
  import star.mbox.
  import star.assert.

  import test.lib.text.

  parseWordFile:(string)=>cons[cons[char]] throws ioException.
  parseWordFile(Fn) => valof{
    Lns = readLines(Fn);

    valis (Lns//explode)
  }

  result ::= .grey | .yellow(char) | .green(integer,char).

  score:(cons[char],cons[char])=>cons[result].
  score(W,G) => valof{
    (GS,G1) = greenScore(W,zip(G,iota(0,size(G))),[],[]);
    valis yellowScore(W,G1,GS)
  }

  greenScore([],[],Grn,Oth) => (Grn,Oth).
  greenScore([C,..Cs],[(C,Ix),..Rs],Grn,Oth) =>
    greenScore(Cs,Rs,[.green(Ix,C),..Grn],Oth).
  greenScore([_,..Cs],[C,..Rs],Grn,[C,..Oth]).

  main:()=>().
  main() => valof{
    try{
      Words = parseWordFile("wordle.txt");
      show Words
    } catch {
      M => showMsg("We got an exception: $(M)")
    };
    valis ()
  }
}
    
