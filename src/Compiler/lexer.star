star.compiler.lexer{
  import star.
  import star.compiler.operators.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.token.
  import star.pkg.

  -- The Star compiler lexer
  public allTokens:(tokenState) => (cons[token],reports).
  allTokens(St) where (Stx,Toks) .= allTks(St) => (Toks,stateReport(Stx)).

  allTks:(tokenState)=>(tokenState,cons[token]).
  allTks(St) => let{
    allToks(Strm,SoFr) where (Nx,some(Tk)).=nextToken(Strm) => allToks(Nx,[Tk,..SoFr]).
    allToks(Strm,SoFr) default => (Strm,reverse(SoFr)).
  } in allToks(St,[]).
  
  public initSt:(locn,cons[char],reports)=>tokenState.
  initSt(locn(P,Line,Col,Start,_),Txt,Rp) => tokenState(P,Line,Col,Start,Txt,Rp).

  lexerr(tokenState(P,Line,Col,Start,Txt,Rp),Msg,Lc) =>
    tokenState(P,Line,Col,Start,Txt,reportError(Rp,Msg,Lc)).

  stateReport(tokenState(_P,_Line,_Col,_Start,_Txt,Rp))=>Rp.

  public nextToken:(tokenState) => (tokenState,option[token]).
  nextToken(St) => nxTok(skipToNx(St)).

  nxTok:(tokenState) => (tokenState,option[token]).
  nxTok(St) where (Nx,some(Chr)) .= nextChr(St) =>
    nxxTok(Chr,Nx,St).
  nxTok(St) default => (St,.none).

  nxxTok:(char,tokenState,tokenState) => (tokenState,option[token]).
  nxxTok(`0`,St,St0) where (Nx,some(Ch)).=nextChr(St) => let{
    numericTok(`x`,St1) where (Nxt,some(Hx)) .= hexChars(Nx,0) =>
      (Nxt,some(tok(makeLoc(St0,Nxt),intTok(Hx)))).
    numericTok(_,_) default => readNumber(St0).
  } in numericTok(Ch,Nx).
  nxxTok(`1`,_,St0) => readNumber(St0).
  nxxTok(`2`,_,St0) => readNumber(St0).
  nxxTok(`3`,_,St0) => readNumber(St0).
  nxxTok(`4`,_,St0) => readNumber(St0).
  nxxTok(`5`,_,St0) => readNumber(St0).
  nxxTok(`6`,_,St0) => readNumber(St0).
  nxxTok(`7`,_,St0) => readNumber(St0).
  nxxTok(`8`,_,St0) => readNumber(St0).
  nxxTok(`9`,_,St0) => readNumber(St0).
  nxxTok(`'`,St,St0) where (Nxt,some(Id)) .= readQuoted(St,`'`,[]) =>
    (Nxt,some(tok(makeLoc(St0,Nxt),idQTok(Id)))).
  nxxTok(`\"`,St,St0) where Nx ^= lookingAt(St,[`\"`,`\"`]) => stringBlob(Nx,St0,[]).
  nxxTok(`\"`,St,St0) where (Nxt,some(Str)) .= readString(St,[]) =>
    (Nxt,some(tok(makeLoc(St0,Nxt),strTok(Str)))).
  nxxTok(`\``,St,St0) where
      (St1,some(ChC)) .= charRef(St) &&
      Stx ^= lookingAt(St1,[`\``]) =>
    (Stx,some(tok(makeLoc(St0,Stx),chrTok(ChC)))).
  nxxTok(`.`,St,St0) where (Nx,some(`\n`)) .= nextChr(St) =>
    (Nx,some(tok(makeLoc(St0,Nx),idTok(". ")))).
  nxxTok(`.`,St,St0) where (Nx,some(`\t`)) .= nextChr(St) =>
    (Nx,some(tok(makeLoc(St0,Nx),idTok(". ")))).
  nxxTok(Chr,St,St0) where Ld ^= first(Chr) => let{
  graphFollow:(tokenState,string,option[token]) => (tokenState,option[token]).
    graphFollow(Strm,SoF,Deflt) where (Nx,some(Ch)) .= nextChr(Strm) && SoF1 ^= follows(SoF,Ch) =>
      graphFollow(Nx,SoF1,finalist(SoF1,Nx,Deflt)).
    graphFollow(Strm,Id,Deflt) default => (Strm,Deflt).

    finalist(SoFr,Str,Deflt) where final(SoFr) =>
    ( bkt(SoFr,Lbl,_,_,_) ^= isBracket(SoFr) ? some(tok(makeLoc(St0,Str),lftTok(Lbl))) ||
      bkt(_,Lbl,SoFr,_,_) ^= isBracket(SoFr) ? some(tok(makeLoc(St0,Str),rgtTok(Lbl))) ||
      some(tok(makeLoc(St0,Str),idTok(SoFr)))).
    finalist(_,_,Deflt) => Deflt.
  } in graphFollow(St,Ld,finalist(Ld,St,.none)).
  nxxTok(Chr,St,St0) where isIdentifierStart(Chr) => readIden(St,St0,[Chr]).
  nxxTok(Chr,St,St0) default => nextToken(lexerr(St0,"illegal char in token: '$(Chr):c;'",makeLoc(St,St0))).

  isIdentifierStart(Ch) => (Ch==`_` || isLetter(Ch)).

  readIden:(tokenState,tokenState,cons[char]) => (tokenState,option[token]).
  readIden(St,St0,SoF) where (Nx,some(Chr)) .= nextChr(St) && (isIdentifierStart(Chr) || _isNdChar(Chr)) =>
    readIden(Nx,St0,[Chr,..SoF]).
  readIden(St,St0,SoF) default => (St,some(tok(makeLoc(St0,St),idTok(reverse(SoF)::string)))).

  readQuoted:(tokenState,char,cons[char]) => (tokenState,option[string]).
  readQuoted(St,Qt,Chrs) where (Nx,some(Qt)) .= nextChr(St) => (Nx,some(reverse(Chrs)::string)).
  readQuoted(St,Qt,Chrs) where (Nx,some(Ch)) .= charRef(St) => readQuoted(Nx,Qt,[Ch,..Chrs]).
  readQuoted(Nx,_,_) => (Nx,.none).

  readString:(tokenState,cons[stringSegment]) => (tokenState,option[cons[stringSegment]]).
  readString(St,SoFar) where (Nx,some(`\"`)) .= nextChr(St) => (Nx,some(reverse(SoFar))).
  readString(St,SoFar) where
    (Nx,some(`$`)) .= nextChr(St) &&
    (_,some(`(`)) .= nextChr(Nx) &&
    (St1,some(Inter)) .= interpolation(Nx) => readString(St1,[Inter,..SoFar]).
  readString(St,SoFar) where
    (Nx,some(`#`)) .= nextChr(St) &&
    (_,some(`\(`)) .= nextChr(Nx) &&
	  (St1,some(Inter)) .= evaluation(Nx) => readString(St1,[Inter,..SoFar]).
  readString(St,SoFar) where (St1,some(Seg)) .= readStr(St,[]) =>
    readString(St1,[segment(makeLoc(St,St1),Seg),..SoFar]).

  readStr:(tokenState,cons[char]) => (tokenState,option[string]).
  readStr(St,Chrs) where  `\"` ^= hedChar(St) => (St,some(reverse(Chrs)::string)).
  readStr(St,Chrs) where Nx ^= lookingAt(St,[`$`,`(`]) => (St,some(reverse(Chrs)::string)).
  readStr(St,Chrs) where Nx ^= lookingAt(St,[`#`,`(`]) => (St,some(reverse(Chrs)::string)).
  readStr(St,Chrs) where (Nx,some(Ch)) .= charRef(St) => readStr(Nx,[Ch,..Chrs]).
  readStr(St,_) => (St,.none).

  interpolation:(tokenState) => (tokenState,option[stringSegment]).
  interpolation(St) where
      (St1,some(Chr)) .= nextChr(St) &&
    (St2,some(Inter)) .= bracketCount(St,St1,Chr,[],[]) &&
    (St3,some(Format)) .= readFormat(St2) &&
    (St4,IToks) .= allTks(interSt(St2,Inter)) =>
   (mergeReports(St3,St4),some(interpolate(makeLoc(St,St3),IToks,Format))).

  evaluation:(tokenState) => (tokenState,option[stringSegment]).
  evaluation(St) where
    (St1,some(Chr)) .= nextChr(St) &&
    (St2,some(Inter)) .= bracketCount(St,St1,Chr,[],[]) &&
    (St3,IToks) .= allTks(interSt(St2,Inter)) =>
  (mergeReports(St2,St3),some(evaluate(makeLoc(St,St2),IToks))).

  bracketCount:(tokenState,tokenState,char,cons[char],cons[char]) => (tokenState,option[string]).
  bracketCount(_,St1,Cl,[Cl],Chrs) => (St1,some(reverse([Cl,..Chrs])::string)).
  bracketCount(_,St1,Cl,[Cl,..Stk],Chrs) where (St2,some(Ch)).=nextChr(St1) =>
    bracketCount(St1,St2,Ch,Stk,[Cl,..Chrs]).
  bracketCount(_,St1,`(`,Stk,Chrs) where (St2,some(Ch)).=nextChr(St1) =>
    bracketCount(St1,St2,Ch,[`)`,..Stk],[`\(`,..Chrs]).
  bracketCount(_,St1,`{`,Stk,Chrs) where (St2,some(Ch)).=nextChr(St1) =>
    bracketCount(St1,St2,Ch,[`}`,..Stk],[`\{`,..Chrs]).
  bracketCount(_,St1,`[`,Stk,Chrs) where (St2,some(Ch)).=nextChr(St1) =>
    bracketCount(St1,St2,Ch,[`]`,..Stk],[`\[`,..Chrs]).
  bracketCount(St,_,_,[],Chrs) => (St,some(reverse(Chrs)::string)).
  bracketCount(_,St1,C,Stk,Chrs) where (St2,some(Ch)) .= nextChr(St1) =>
    bracketCount(St1,St2,Ch,Stk,[C,..Chrs]).

  readFormat(St) where (St1,some(`:`)) .= nextChr(St) => readUntil(St1,`;`,[]).
  readFormat(St) => (St,some("")).

  readUntil:(tokenState,char,cons[char]) => (tokenState,option[string]).
  readUntil(St,Qt,Chrs) where (Nx,some(Qt)) .= nextChr(St) => (Nx,some(reverse(Chrs)::string)).
  readUntil(St,Qt,Chrs) where (Nx,some(Ch)) .= charRef(St) => readUntil(Nx,Qt,[Ch,..Chrs]).
  readUntil(St,_,_) => (St,.none).

  stringBlob:(tokenState,tokenState,cons[char]) => (tokenState,option[token]).
  stringBlob(St,St0,Sf) where St1 ^= lookingAt(St,[`\"`,`\"`,`\"`]) &&
    Lc .= makeLoc(St0,St1) =>
    (St1,some(tok(Lc,strTok([segment(Lc,reverse(Sf)::string)])))).
  stringBlob(St,St0,Sf) where (St1,some(Nx)) .= nextChr(St) =>
    stringBlob(St1,St0,[Nx,..Sf]).

  charRef(St) where Nx ^= lookingAt(St,[`\\`]) && (Nxt,some(Ch)) .= nextChr(Nx) => backslashRef(Nxt,Ch).
  charRef(St) => nextChr(St).

  backslashRef:(tokenState,char) => (tokenState,option[char]).
  backslashRef(St,`a`) => (St,some(`\a`)).
  backslashRef(St,`b`) => (St,some(`\b`)).
  backslashRef(St,`e`) => (St,some(`\e`)).
  backslashRef(St,`t`) => (St,some(`\t`)).
  backslashRef(St,`n`) => (St,some(`\n`)).
  backslashRef(St,`r`) => (St,some(`\r`)).
  backslashRef(St,`u`) where (St1,some(Hx)) .= hexChars(St,0) &&
      Stx ^= lookingAt(St1,[`;`]) => (Stx,some(Hx::char)).
  backslashRef(St,Ch) => (St,some(Ch)).

  hexChars:(tokenState,integer) => (tokenState,option[integer]).

  hexChars(St,Hx) where Hd^=hedChar(St) && Dg^=isHexDigit(Hd) =>
    hexChars(nxtSt(St),Hx*16+Dg).
  hexChars(St,Hx) => (St,some(Hx)).

  readNumber:(tokenState) => (tokenState,option[token]).
  readNumber(St) where (Nx,some(Mn)) .= readNatural(St,0) => readMore(Nx,St,Mn).

  readNatural:(tokenState,integer) => (tokenState,option[integer]).
  readNatural(St,Sf) where (Nx,some(Dg)).=nextChr(St) && isDigit(Dg) => readNatural(Nx,Sf*10+digitVal(Dg)).
  readNatural(St,Sf) => (St,some(Sf)).

  readInt:(tokenState) => (tokenState,option[integer]).
  readInt(St) where Nx^=lookingAt(St,[`-`]) && (St1,some(Nt)).=readNatural(Nx,0) => (St1,some(-Nt)).
  readInt(St) => readNatural(St,0).

  readMore:(tokenState,tokenState,integer) => (tokenState,option[token]).
  readMore(St,St0,Sf) where St1^=lookingAt(St,[`.`]) && Hd^=hedChar(St1) && isDigit(Hd) =>
    readFraction(St1,St0,Sf::float,0.1).
  readMore(St,St0,Sf) => (St,some(tok(makeLoc(St0,St),intTok(Sf)))).

  readFraction:(tokenState,tokenState,float,float) => (tokenState,option[token]).
  readFraction(St,St0,Sf,Scle) where (St1,some(Hd)).=nextChr(St) && isDigit(Hd) =>
    readFraction(St1,St0,Sf+(digitVal(Hd)::float)*Scle,Scle*0.1).
  readFraction(St,St0,Sf,_) where St1^=lookingAt(St,[`e`]) => readExponent(St1,St0,Sf).
  readFraction(St,St0,Sf,_) => (St,some(tok(makeLoc(St0,St),fltTok(Sf)))).

  readExponent:(tokenState,tokenState,float) => (tokenState,option[token]).
  readExponent(St,St0,Mn) where (St1,some(Ix)).=readInt(St) =>
    (St1,some(tok(makeLoc(St0,St1),fltTok(Mn*(10.0**(Ix::float)))))).
  readExponent(St,St0,Mn) => (St,some(tok(makeLoc(St0,St),fltTok(Mn)))).

  -- We define a tracking state to allow us to collect locations
  public tokenState ::= tokenState(string,integer,integer,integer,cons[char],reports).

  atEof:(tokenState) => boolean.
  atEof(tokenState(_,_,_,_,Str,_)) => _eof(Str).

  nextChr:(tokenState) => (tokenState,option[char]).
  nextChr(St) where tokenState(_,_,_,_,cons(Ch,_),_).=St  => (nxtSt(St),some(Ch)).
  nextChr(St) default => (St,.none).

  hedChar:(tokenState) => option[char].
  hedChar(tokenState(_,_,_,_,Txt,_)) => head(Txt).

  preChar:(tokenState,char) => tokenState.
  preChar(tokenState(Pkg,Line,Col,Off,Txt,Rp),Chr) =>
    tokenState(Pkg,Line,Col-1,Off-1,[Chr,..Txt],Rp).

  interSt:(tokenState,string) => tokenState.
  interSt(tokenState(P,Ln,Cl,Off,_,Rp),Txt) => tokenState(P,Ln,Cl,Off,Txt::cons[char],Rp).

  mergeReports(tokenState(P,Ln,Cl,Off,Txt,_),tokenState(_,_,_,_,_,Rp)) => tokenState(P,Ln,Cl,Off,Txt,Rp).

  nxtSt:(tokenState) => tokenState.
  nxtSt(tokenState(Pk,Line,Col,Off,cons(`\n`,Txt),Rp)) =>
    tokenState(Pk,Line+1,1,Off+1,Txt,Rp).
  nxtSt(tokenState(Pk,Line,Col,Off,cons(_,Txt),Rp)) =>
    tokenState(Pk,Line,Col+1,Off+1,Txt,Rp).

  lookingAt:(tokenState,cons[char]) => option[tokenState].
  lookingAt(St,[]) => some(St).
  lookingAt(St,[Ch,..Nxt]) where Ch^=hedChar(St) => lookingAt(nxtSt(St),Nxt).
  lookingAt(_,_) default => .none.

  makeLoc:(tokenState,tokenState)=>locn.
  makeLoc(tokenState(Pk,Line,Col,Start,_,_),tokenState(_,_,_,End,_,_)) =>
    locn(Pk,Line,Col,Start,End-Start).

  skipToNx:(tokenState) => tokenState.
  skipToNx(St) where Ch ^= hedChar(St) && isNonPrint(Ch) => skipToNx(nxtSt(St)).
  skipToNx(St) where Nx ^= lookingAt(St,[`-`,`-`,` `]) => skipToNx(lineComment(Nx)).
  skipToNx(St) where Nx ^= lookingAt(St,[`-`,`-`,`\n`]) => skipToNx(Nx).
  skipToNx(St) where Nx ^= lookingAt(St,[`-`,`-`,`\t`]) => skipToNx(lineComment(Nx)).
  skipToNx(St) where Nx ^= lookingAt(St,[`/`,`*`]) => skipToNx(blockComment(Nx)).
  skipToNx(St) => St.

  lineComment(St) where Ch^=hedChar(St) => ((Ch==`\n`||_isZlChar(Ch)) ? nxtSt(St) || lineComment(nxtSt(St))).
  lineComment(St) => St.

  blockComment(St) where Nx^=lookingAt(St,[`*`,`/`]) => Nx.
  blockComment(St) where atEof(St) => St.
  blockComment(St) default => blockComment(nxtSt(St)).

  isNonPrint:(char) => boolean.
  isNonPrint(Ch) => (_isZlChar(Ch) || _isZsChar(Ch) || _isZpChar(Ch) || _isCcChar(Ch)).

  public implementation display[tokenState] => {.
    disp(tokenState(Pk,Line,Col,Off,_,_)) => disp(locn(Pk,Line,Col,Off,0)).
  .}

  public implementation hasLoc[tokenState] => {.
    locOf(tokenState(Pkg,Line,Col,Start,_,_)) => locn(Pkg,Line,Col,Start,0).
  .}
}
