star.compiler.lexer{
  import star.
  import star.compiler.operators.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.token.
  import star.pkg.

  -- The Star compiler lexer
  public nextToken:(tokenState) => option[(tokenState,token)].
  nextToken(St) => nxTok(skipToNx(St)).

  nxTok:(tokenState) => option[(tokenState,token)].
  nxTok(St) where Nx ^= lookingAt(St,[0c0,0cc]) && (Nxt,Ch) ^= charRef(Nx) =>
    some((Nxt,tok(makeLoc(St,Nxt),intTok(Ch)))).
  nxTok(St) where Nx ^= lookingAt(St,[0c0,0cx]) && (Nxt,Hx) ^= hexChar(Nx,0) =>
    some((Nxt,tok(makeLoc(St,Nxt),intTok(Hx)))).
  nxTok(St) where Nx ^= hedChar(St) && isDigit(Nx) => readNumber(St).
  nxTok(St) where Nx ^= lookingAt(St,[0c']) && (Nxt,Id) ^= readQuoted(Nx,0c',[]) =>
    some((Nxt,tok(makeLoc(St,Nxt),idQTok(Id)))).
  nxtTok(St) where Nx ^= lookingAt(St,[0c",0c",0c"]) =>
    stringBlob(Nx,St,[]).
  nxtTok(St) where 0c\" ^= hedChar(St) where (St1,Segs) ^= readString(St,[]) =>
    some((makeLoc(St,St1),stringTok(Segs))).


  nxTok(_) default => none.

  readQuoted:(tokenState,integer,list[integer]) => option[(tokenState,string)].
  readQuoted(St,Qt,Chrs) where (Nx,Qt) ^= nextChr(St) => some((Nx,Chrs::string)).
  readQuoted(St,Qt,Chrs) where (Nx,Ch) ^= charRef(St) => readQuoted(Nx,Qt,[Chrs..,Ch]).
  readQuoted(_,_,_) => none.

  stringBlob:(tokenState,tokenState,list[integer]) => option[(tokenState,token)].
  stringBlob(St,St0,Sf) where St1 ^= lookingAt(St,[0c\",0c\",0c\"]) =>
    some((St1,tok(makeLoc(St0,St1),strTok([segment(Sf::string)])))).
  stringBlob(St,St0,Sf) where (St1,Nx) ^= nextChr(St) => stringBlob(St1,St0,[Sf..,Nx]).

  readString:(tokenState,list[stringSeg]) => option[(tokenState,list[stringSeg])].
  readString(St,Segs) where Nx^=lookingAt(St,[0c\"]) && (St1,Seg) ^= readStrSeg(Nx) =>
    readString(St1,[Segs..,Seg]).

  readMoreString(St,So,Segs) where St1 ^= lookingAt(St,[0c\"]) => some((St1,[Segs..,segment(So::string)])).
  readStringSeg(St,So,Segs) where St1 ^= lookingAt(St,[0c\\,0c(]) =>
    interpolation()

  charRef(St) where Nx ^= lookingAt(St,[0c\\]) && (Nxt,Ch) ^= nextChr(Nx) => backslashRef(Nxt,Ch).
  charRef(St) => nextChr(St).

  backslashRef:(tokenState,integer) => option[(tokenState,integer)].
  backslashRef(St,0ca) => some((St,0c\a)).
  backslashRef(St,0cb) => some((St,0c\b)).
  backslashRef(St,0ce) => some((St,0c\e)).
  backslashRef(St,0ct) => some((St,0c\t)).
  backslashRef(St,0cn) => some((St,0c\n)).
  backslashRef(St,0cr) => some((St,0c\r)).
  backslashRef(St,0cu) => hexChar(St,0).
  backslashRef(St,Ch) => some((St,Ch)).

  hexChar:(tokenState,integer) => option[(tokenState,integer)].
  hexChar(St,Hx) where Nx^=lookingAt(St,[0c;]) => some((Nx,Hx)).
  hexChar(St,Hx) where Hd^=hedChar(St) && Dg^=isHexDigit(Hd) =>
    hexChar(nxtSt(St),Hx*16+Dg).
  hexChar(St,Hx) => some((St,Hx)).

  isHexDigit:(integer) => option[integer].
  isHexDigit(Ch) where isDigit(Ch) => some(digitVal(Ch)).
  isHexDigit(0ca) => some(10).
  isHexDigit(0cb) => some(11).
  isHexDigit(0cc) => some(12).
  isHexDigit(0cd) => some(13).
  isHexDigit(0ce) => some(14).
  isHexDigit(0cf) => some(15).
  isHexDigit(0cA) => some(10).
  isHexDigit(0cB) => some(11).
  isHexDigit(0cC) => some(12).
  isHexDigit(0cD) => some(13).
  isHexDigit(0cE) => some(14).
  isHexDigit(0cF) => some(15).
  isHexDigit(_) default => none.

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

  hedHedChar:(tokenState) => option[integer].
  hedHedChar(tokenState(_,_,_,_,Txt)) where size(Txt)>1 => Txt[1].
  hedHedChar(_) default => none.

  hedHedHedChar:(tokenState) => option[integer].
  hedHedHedChar(tokenState(_,_,_,_,Txt)) where size(Txt)>2 => Txt[2].
  hedHedHedChar(_) default => none.

  public initSt:(pkg,list[integer])=>tokenState.
  initSt(P,Txt) => tokenState(P,1,0,0,Txt).

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
  makeLoc(tokenState(Pk,Line,Col,Start,_),tokenState(_,_,_,End,_)) => locn(Pk,Line,Col,End-Start).

  skipToNx:(tokenState) => tokenState.
  skipToNx(St) where Ch ^= hedChar(St) && isNonPrint(Ch) => skipToNx(nxtSt(St)).
  skipToNx(St) where Nx ^= lookingAt(St,[0c-,0c-,0c ]) => skipToNx(lineComment(Nx)).
  skipToNx(St) where Nx ^= lookingAt(St,[0c/,0c*]) => skipToNx(blockComment(Nx)).
  skipToNx(St) => St.

  lineComment(St) where Ch^=hedChar(St) => ((Ch==0c\n||_isZlChar(Ch)) ? nxtSt(St) | lineComment(nxtSt(St))).
  lineComment(St) => St.

  blockComment(St) where Nx^=lookingAt(St,[0c*,0c/]) => Nx.
  blockComment(St) where atEof(St) => St.
  blockComment(St) default => blockComment(nxtSt(St)).

  isNonPrint:(integer) => boolean.
  isNonPrint(Ch) => (_isZlChar(Ch) || _isZsChar(Ch) || _isZpChar(Ch) || _isCcChar(Ch)).

  public implementation display[tokenState] => {.
    disp(tokenState(Pk,Line,Col,Off,_)) => disp(locn(Pk,Line,Col,Off)).
  .}
}
