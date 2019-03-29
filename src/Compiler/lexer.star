star.compiler.lexer{
  import star.
  import star.compiler.operators.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.token.
  import star.pkg.

  -- The Star compiler lexer
  public allTokens:(tokenState) => cons[token].
  allTokens(St) => let{
    allToks(Strm,SoFr) where (Nx,Tk)^=nextToken(Strm) => allToks(Nx,[SoFr..,Tk]).
    allToks(_,SoFr) default => SoFr.
  } in allToks(St,[]).

  public initSt:(locn,list[integer])=>tokenState.
  initSt(locn(P,Line,Col,Start,_),Txt) => tokenState(P,Line,Col,Start,Txt).

  public nextToken:(tokenState) => option[(tokenState,token)].
  nextToken(St) => nxTok(skipToNx(St)).

  nxTok:(tokenState) => option[(tokenState,token)].
  nxTok(St) where (Nx,Chr) ^= nextChr(St) =>
    nxxTok(Chr,Nx,St).
  nxTok(_) default => none.

  nxxTok:(integer,tokenState,tokenState) => option[(tokenState,token)].
  nxxTok(0c0,St,St0) where (Nx,Ch)^=nextChr(St) => let{
    numericTok(0cc,St1) where (Nxt,ChC) ^= charRef(St1) =>
      some((Nxt,tok(makeLoc(St0,Nxt),intTok(ChC)))).
    numericTok(0cx,St1) where (Nxt,Hx) ^= hexChars(Nx,0) =>
      some((Nxt,tok(makeLoc(St0,Nxt),intTok(Hx)))).
    numericTok(_,_) default => readNumber(St0).
  } in numericTok(Ch,Nx).
  nxxTok(NCh,_,St0) where isDigit(NCh) => readNumber(St0).
  nxxTok(0c',St,St0) where (Nxt,Id) ^= readQuoted(St,0c',[]) =>
    some((Nxt,tok(makeLoc(St0,Nxt),idQTok(Id)))).
  nxxTok(0c",St,St0) where Nx ^= lookingAt(St,[0c",0c"]) =>
    stringBlob(Nx,St0,[]).
  nxxTok(0c",St,St0) where (Nxt,Str) ^= readString(St,[]) =>
    some((Nxt,tok(makeLoc(St0,Nxt),strTok(Str)))).
  nxxTok(0c.,St,St0) where (Nx,0c\n) ^= nextChr(St) =>
    some((Nx,tok(makeLoc(St0,Nx),idTok(". ")))).
  nxxTok(Chr,St,St0) where Ld ^= follows("",Chr) => let{
    graphFollow(Strm,SoF,Deflt) where (Nx,Ch) ^= nextChr(Strm) && SoF1 ^= follows(SoF,Ch) =>
      graphFollow(Nx,SoF1,finalist(SoF1,Nx,Deflt)).
    graphFollow(Strm,Id,Deflt) default => Deflt.

    finalist(SoFr,Str,Deflt) where final(SoFr) =>
      ( bkt(SoFr,Lbl,_,_) ^= isBracket(SoFr) ? some((Str,tok(makeLoc(St0,Str),lftTok(Lbl)))) ||
        bkt(_,Lbl,SoFr,_) ^= isBracket(SoFr) ? some((Str,tok(makeLoc(St0,Str),rgtTok(Lbl)))) ||
        some((Str,tok(makeLoc(St0,Str),idTok(SoFr))))).
    finalist(_,_,Deflt) => Deflt.
  } in graphFollow(St,Ld,finalist(Ld,St,none)).
  nxxTok(Chr,St,St0) where isIdentifierStart(Chr) => readIden(St,St0,[Chr]).
  nxxTok(_,_,_) default => none.

  isIdentifierStart(Ch) => (Ch==0c_ || isLetter(Ch)).

  readIden:(tokenState,tokenState,list[integer]) => option[(tokenState,token)].
  readIden(St,St0,SoF) where (Nx,Chr) ^= nextChr(St) && (isIdentifierStart(Chr) || _isNdChar(Chr)) =>
    readIden(Nx,St0,[SoF..,Chr]).
  readIden(St,St0,SoF) default => some((St,tok(makeLoc(St0,St),idTok(SoF::string)))).

  readQuoted:(tokenState,integer,list[integer]) => option[(tokenState,string)].
  readQuoted(St,Qt,Chrs) where (Nx,Qt) ^= nextChr(St) => some((Nx,Chrs::string)).
  readQuoted(St,Qt,Chrs) where (Nx,Ch) ^= charRef(St) => readQuoted(Nx,Qt,[Chrs..,Ch]).
  readQuoted(_,_,_) => none.

  readString:(tokenState,list[stringSegment]) => option[(tokenState,list[stringSegment])].
  readString(St,SoFar) where (Nx,0c") ^= nextChr(St) => some((Nx,SoFar)).
  readString(St,SoFar) where
    (Nx,0c$) ^= nextChr(St) &&
    (_,0c() ^= nextChr(Nx) &&
    (St1,Inter) ^= interpolation(Nx) => readString(St1,[SoFar..,Inter]).
  readString(St,SoFar) where (St1,Seg) ^= readStr(St,[]) =>
    readString(St1,[SoFar..,segment(makeLoc(St,St1),Seg)]).

  readStr:(tokenState,list[integer]) => option[(tokenState,string)].
  readStr(St,Chrs) where  0c" ^= hedChar(St) => some((St,Chrs::string)).
  readStr(St,Chrs) where Nx ^= lookingAt(St,[0c$,0c(]) => some((St,Chrs::string)).
  readStr(St,Chrs) where (Nx,Ch) ^= charRef(St) => readStr(Nx,[Chrs..,Ch]).
  readStr(_,_) => none.

  interpolation:(tokenState) => option[(tokenState,stringSegment)].
  interpolation(St) where
      (St1,Chr) ^= nextChr(St) &&
      (St2,Inter) ^= bracketCount(St,St1,Chr,[],[]) &&
      (St3,Format) ^= readFormat(St2) =>
        some((St3,interpolate(makeLoc(St,St3),allTokens(interSt(St1,Inter)),Format))).

  bracketCount:(tokenState,tokenState,integer,list[integer],list[integer]) => option[(tokenState,string)].
  bracketCount(_,St1,Cl,[Cl,..Stk],Chrs) where (St2,Ch)^=nextChr(St1) =>
    bracketCount(St1,St2,Ch,Stk,[Chrs..,Cl]).
  bracketCount(_,St1,0c(,Stk,Chrs) where (St2,Ch)^=nextChr(St1) =>
    bracketCount(St1,St2,Ch,[0c),..Stk],[Chrs..,0c(]).
  bracketCount(_,St1,0c{,Stk,Chrs) where (St2,Ch)^=nextChr(St1) =>
    bracketCount(St1,St2,Ch,[0c},..Stk],[Chrs..,0c{]).
  bracketCount(_,St1,0c[,Stk,Chrs) where (St2,Ch)^=nextChr(St1) =>
    bracketCount(St1,St2,Ch,[0c],..Stk],[Chrs..,0c[]).
  bracketCount(St,_,_,[],Chrs) => some((St,Chrs::string)).
  bracketCount(_,St1,C,Stk,Chrs) where (St2,Ch) ^= nextChr(St1) =>
    bracketCount(St1,St2,Ch,Stk,[Chrs..,C]).

  readFormat(St) where (St1,0c:) ^= nextChr(St) => readUntil(St1,0c;,[]).
  readFormat(St) => some((St,"")).

  readUntil:(tokenState,integer,list[integer]) => option[(tokenState,string)].
  readUntil(St,Qt,Chrs) where (Nx,Qt) ^= nextChr(St) => some((Nx,Chrs::string)).
  readUntil(St,Qt,Chrs) where (Nx,Ch) ^= charRef(St) => readUntil(Nx,Qt,[Chrs..,Ch]).
  readUntil(_,_,_) => none.

  stringBlob:(tokenState,tokenState,list[integer]) => option[(tokenState,token)].
  stringBlob(St,St0,Sf) where St1 ^= lookingAt(St,[0c\",0c\",0c\"]) &&
    Lc .= makeLoc(St0,St1) =>
    some((St1,tok(Lc,strTok([segment(Lc,Sf::string)])))).
  stringBlob(St,St0,Sf) where (St1,Nx) ^= nextChr(St) => stringBlob(St1,St0,[Sf..,Nx]).

  charRef(St) where Nx ^= lookingAt(St,[0c\\]) && (Nxt,Ch) ^= nextChr(Nx) => backslashRef(Nxt,Ch).
  charRef(St) => nextChr(St).

  backslashRef:(tokenState,integer) => option[(tokenState,integer)].
  backslashRef(St,0ca) => some((St,0c\a)).
  backslashRef(St,0cb) => some((St,0c\b)).
  backslashRef(St,0ce) => some((St,0c\e)).
  backslashRef(St,0ct) => some((St,0c\t)).
  backslashRef(St,0cn) => some((St,0c\n)).
  backslashRef(St,0cr) => some((St,0c\r)).
  backslashRef(St,0cu) => hexChars(St,0).
  backslashRef(St,Ch) => some((St,Ch)).

  hexChars:(tokenState,integer) => option[(tokenState,integer)].
  hexChars(St,Hx) where Nx^=lookingAt(St,[0c;]) => some((Nx,Hx)).
  hexChars(St,Hx) where Hd^=hedChar(St) && Dg^=isHexDigit(Hd) =>
    hexChars(nxtSt(St),Hx*16+Dg).
  hexChars(St,Hx) => some((St,Hx)).

  readNumber:(tokenState) => option[(tokenState,token)].
  readNumber(St) where (Nx,Mn) ^= readNatural(St,0) => readMore(Nx,St,Mn).

  readNatural:(tokenState,integer) => option[(tokenState,integer)].
  readNatural(St,Sf) where (Nx,Dg)^=nextChr(St) && isDigit(Dg) => readNatural(Nx,Sf*10+digitVal(Dg)).
  readNatural(St,Sf) => some((St,Sf)).

  readInt:(tokenState) => option[(tokenState,integer)].
  readInt(St) where Nx^=lookingAt(St,[0c-]) && (St1,Nt)^=readNatural(Nx,0) => some((St1,-Nt)).
  readInt(St) => readNatural(St,0).

  readMore:(tokenState,tokenState,integer) => option[(tokenState,token)].
  readMore(St,St0,Sf) where St1^=lookingAt(St,[0c.]) && Hd^=hedChar(St1) && isDigit(Hd) =>
    readFraction(St1,St0,Sf::float,0.1).
  readMore(St,St0,Sf) => some((St,tok(makeLoc(St0,St),intTok(Sf)))).

  readFraction:(tokenState,tokenState,float,float) => option[(tokenState,token)].
  readFraction(St,St0,Sf,Scle) where (St1,Hd)^=nextChr(St) && isDigit(Hd) =>
    readFraction(St1,St0,Sf+(digitVal(Hd)::float)*Scle,Scle*0.1).
  readFraction(St,St0,Sf,_) where St1^=lookingAt(St,[0ce]) => readExponent(St1,St0,Sf).
  readFraction(St,St0,Sf,_) => some((St,tok(makeLoc(St0,St),fltTok(Sf)))).

  readExponent:(tokenState,tokenState,float) => option[(tokenState,token)].
  readExponent(St,St0,Mn) where (St1,Ix)^=readInt(St) =>
    some((St1,tok(makeLoc(St0,St1),fltTok(Mn*(10.0**(Ix::float)))))).
  readExponent(St,St0,Mn) => some((St,tok(makeLoc(St0,St),fltTok(Mn)))).

  -- We define a tracking state to allow us to collect locations
  public tokenState ::= tokenState(pkg,integer,integer,integer,list[integer]).

  atEof:(tokenState) => boolean.
  atEof(tokenState(_,_,_,_,Str)) => _eof(Str).

  nextChr:(tokenState) => option[(tokenState,integer)].
  nextChr(St) where tokenState(_,_,_,_,Txt).=St && Ch^=Txt[0] => some((nxtSt(St),Ch)).
  nextChr(_) default => none.

  hedChar:(tokenState) => option[integer].
  hedChar(tokenState(_,_,_,_,Txt)) where size(Txt)>0 => Txt[0].
  hedChar(_) default => none.

  preChar:(tokenState,integer) => tokenState.
  preChar(tokenState(Pkg,Line,Col,Off,Txt),Chr) =>
    tokenState(Pkg,Line,Col-1,Off-1,[Chr,..Txt]).

  interSt:(tokenState,string) => tokenState.
  interSt(tokenState(P,Ln,Cl,Off,_),Txt) => tokenState(P,Ln,Cl,Off,Txt::list[integer]).

  nxtSt:(tokenState) => tokenState.
  nxtSt(tokenState(Pk,Line,Col,Off,[0c\n,..Txt])) =>
    tokenState(Pk,Line+1,1,Off+1,Txt).
  nxtSt(tokenState(Pk,Line,Col,Off,[_,..Txt])) =>
    tokenState(Pk,Line,Col+1,Off+1,Txt).

  lookingAt:(tokenState,list[integer]) => option[tokenState].
  lookingAt(St,[]) => some(St).
  lookingAt(St,[Ch,..Nxt]) where Ch^=hedChar(St) => lookingAt(nxtSt(St),Nxt).
  lookingAt(_,_) default => none.

  makeLoc:(tokenState,tokenState)=>locn.
  makeLoc(tokenState(Pk,Line,Col,Start,_),tokenState(_,_,_,End,_)) => locn(Pk,Line,Col,Start,End-Start).


  skipToNx:(tokenState) => tokenState.
  skipToNx(St) where Ch ^= hedChar(St) && isNonPrint(Ch) => skipToNx(nxtSt(St)).
  skipToNx(St) where Nx ^= lookingAt(St,[0c-,0c-,0c ]) => skipToNx(lineComment(Nx)).
  skipToNx(St) where Nx ^= lookingAt(St,[0c/,0c*]) => skipToNx(blockComment(Nx)).
  skipToNx(St) => St.

  lineComment(St) where Ch^=hedChar(St) => ((Ch==0c\n||_isZlChar(Ch)) ? nxtSt(St) || lineComment(nxtSt(St))).
  lineComment(St) => St.

  blockComment(St) where Nx^=lookingAt(St,[0c*,0c/]) => Nx.
  blockComment(St) where atEof(St) => St.
  blockComment(St) default => blockComment(nxtSt(St)).

  isNonPrint:(integer) => boolean.
  isNonPrint(Ch) => (_isZlChar(Ch) || _isZsChar(Ch) || _isZpChar(Ch) || _isCcChar(Ch)).

  public implementation display[tokenState] => {.
    disp(tokenState(Pk,Line,Col,Off,_)) => disp(locn(Pk,Line,Col,Off,0)).
  .}

  public implementation hasLoc[tokenState] => {.
    locOf(tokenState(Pkg,Line,Col,Start,_)) => locn(Pkg,Line,Col,Start,0).
  .}
}
