star.compiler.misc{
  import star.
  import star.pkg.

  public genSym:(string) => string.
  genSym(Pre) => _str_gen(Pre).

  public genNewName:(string,string) => string.
  genNewName(Path,Prefix) => qualifiedName(Path,.valMark,genSym(Prefix)).

  public markerType ::= .typeMark |
    .valMark |
    .conMark |
    .overMark |
    .pkgMark |
    .closMark.

  public markerString:(markerType)=>string.
  markerString(.typeMark)=>"*".
  markerString(.closMark) => "^".
  markerString(.valMark)=>"@".
  markerString(.conMark)=>"#".
  markerString(.overMark)=>"!".
  markerString(.pkgMark) => "#".

  public qualifiedName:(string,markerType,string) => string.
  qualifiedName(_,Mrk,Nm) where Glue.=markerString(Mrk) &&
      Ix .= _str_find(Nm,Glue,0) && Ix>=0 => Nm.
  qualifiedName(Pth,Mrk,Nm) => _str_concat(Pth,_str_concat(markerString(Mrk),Nm)).

  public localName:(string,markerType)=>string.
  localName(QNm,M) where Ix.=_str_find(QNm,markerString(M),0) &&
      Ix>=0 && MX .= _str_len(markerString(M)) =>
    _sub_str(QNm,Ix+MX,_str_len(QNm)-Ix-MX).
  localName(Nm,_) default => Nm.

  public packageVar:(pkg)=>string.
  packageVar(pkg(P,_)) => qualifiedName(P,.pkgMark,"").

  public packageVarName:(string,string)=>string.
  packageVarName(P,L) => qualifiedName(P,.pkgMark,L).

  public packageName:(pkg)=>string.
  packageName(pkg(P,_))=>P.

  public pickFailures:all e,x ~~ (cons[either[e,x]])=>either[e,cons[x]].
  pickFailures(Ls) => let{
    pick:all e,x ~~ (cons[either[e,x]],cons[x])=>either[e,cons[x]].
    pick([],L) => either(reverse(L)).
    pick([either(X),..Els],L) => pick(Els,[X,..L]).
    pick([other(E),.._],_) => other(E).
  } in pick(Ls,[]).

  somePrimes:cons[integer].
  somePrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,
    101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,
    197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,
    311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,
    431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,
    557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,
    661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,
    809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,
    937,941,947,953,967,971,977,983,991,997].

  public nextPrime:(integer)=>integer.
  nextPrime(X) where Nxt ^= head(somePrimes^/(P)=>P>=X) => Nxt.

  sieve:(cons[integer]) => cons[integer].
  sieve([]) => [].
  sieve([C,..I]) => [C,..sieve(filterDups(C,I))].

  filterDups:(integer,cons[integer])=>cons[integer].
  filterDups(C,I) => (I^/(X)=>X%C=!=0).
}
