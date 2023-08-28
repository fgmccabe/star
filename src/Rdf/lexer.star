rdf.lexer{
  import star.
  import star.location.
  import star.uri.

  import rdf.errors.
  import rdf.meta.
  import rdf.token.

  -- The lexer for the N3 style notation

  initSt:(locn,cons[char])=>tokenState.
  initSt(.locn(P,Line,Col,Start,_),Txt) => .tokenState(P,Line,Col,Start,Txt).

  public allTokens:(locn,cons[char]) => cons[token].
  allTokens(Lc,Chrs) where (Stx,Toks) .= allTks(initSt(Lc,Chrs)) => Toks.

  allTks:(tokenState)=>(tokenState,cons[token]).
  allTks(St) => let{.
    allToks(Strm,SoFr) where (Nx,.some(Tk)).=nextToken(Strm) => allToks(Nx,[Tk,..SoFr]).
    allToks(Strm,SoFr) default => (Strm,reverse([.endTok(makeLoc(Strm,Strm)),..SoFr])).
  .} in allToks(St,[]).
  
  nextToken:(tokenState) => (tokenState,option[token]).
  nextToken(St) => valof{
    Tok = nxTok(skipToNx(St));
    if traceLex! then
      logMsg("next token: $(Tok)");
    valis Tok
  }

  nxTok:(tokenState) => (tokenState,option[token]).
  nxTok(St) where (Nx,.some(Chr)) .= nextChr(St) =>
    nxxTok(Chr,Nx,St).
  nxTok(St) default => (St,.none).

  nxxTok:(char,tokenState,tokenState) => (tokenState,option[token]).
  nxxTok(`0`,St,St0) where (Nx,.some(Ch)).=nextChr(St) => let{
    numericTok(`x`,St1) where (Nxt,.some(Hx)) .= hexChars(Nx,0) =>
      (Nxt,.some(.tok(makeLoc(St0,Nxt),.intTok(Hx)))).
    numericTok(_,_) default => readNumber(St0).
  } in numericTok(Ch,Nx).
  nxxTok(`0`,St,St0) where (Nx,.some(Ch)).=nextChr(St) =>
    case Ch in {
      `x` => valof{
	(Nxt,.some(Hx)) = hexChars(Nx,0);
	valis (Nxt,.some(.tok(makeLoc(St0,Nxt),.intTok(Hx))))
      }.
      `c` => valof{
	(Nxt,.some(ChC)) = charRef(Nx);
	valis (Nxt,.some(.tok(makeLoc(St0,Nxt),.intTok(ChC::integer))))
      }.
    }.
  nxxTok(`1`,_,St0) => readNumber(St0).
  nxxTok(`2`,_,St0) => readNumber(St0).
  nxxTok(`3`,_,St0) => readNumber(St0).
  nxxTok(`4`,_,St0) => readNumber(St0).
  nxxTok(`5`,_,St0) => readNumber(St0).
  nxxTok(`6`,_,St0) => readNumber(St0).
  nxxTok(`7`,_,St0) => readNumber(St0).
  nxxTok(`8`,_,St0) => readNumber(St0).
  nxxTok(`9`,_,St0) => readNumber(St0).
  nxxTok(`'`,St,St0) where (Nxt,.some(Id)) .= readQuoted(St,`'`,[]) =>
    (Nxt,.some(.tok(makeLoc(St0,Nxt),.idQTok(Id)))).
  nxxTok(`\"`,St,St0) where Nx ?= lookingAt(St,[`\"`,`\"`]) => stringBlob(Nx,St0,[]).
  nxxTok(`\"`,St,St0) where (Nxt,.some(Str)) .= readString(St,[]) =>
    (Nxt,.some(.tok(makeLoc(St0,Nxt),.strTok(Str)))).
  nxxTok(`\``,St,St0) where
      (St1,.some(ChC)) .= charRef(St) &&
      Stx ?= lookingAt(St1,[`\``]) =>
    (Stx,.some(.tok(makeLoc(St0,Stx),.chrTok(ChC)))).
  nxxTok(`.`,St,St0) where (Nx,.some(`\n`)) .= nextChr(St) =>
    (Nx,.some(.tok(makeLoc(St0,Nx),.idTok(". ")))).
  nxxTok(`.`,St,St0) where (Nx,.some(`\t`)) .= nextChr(St) =>
    (Nx,.some(.tok(makeLoc(St0,Nx),.idTok(". ")))).
  nxxTok(Chr,St,St0) where Pnc ?= punc(Chr) =>
    (St,.some(.tok(makeLoc(St0,St),.pncTok(Pnc)))).
  nxxTok(`<`,St,St0) where (Nxt,.some(Txt)) .= readQuoted(St,`>`,[]) =>
    (U ?= parseUri(Txt) ?? (Nxt,.some(.tok(makeLoc(St,Nxt),.uriTok(U)))) ||
      valof{
	reportError("could not parse uri $(Txt)",.some(makeLoc(St,Nxt)));
	valis nextToken(Nxt)
      }).
  nxxTok(Chr,St,St0) where isIdentifierStart(Chr) => readIden(St,St0,[Chr]).
  nxxTok(Chr,St,St0) default => valof{
    reportError("illegal char in token: '$(Chr):c;'",.some(makeLoc(St,St0)));
    valis nextToken(St0)
  }.

  isIdentifierStart(Ch) => (Ch==`_` || isLetter(Ch) || Ch==`-`).

  isIdentChr(Ch) => (_isNdChar(Ch) || isIdentifierStart(Ch)).

  readIden:(tokenState,tokenState,cons[char]) => (tokenState,option[token]).
  readIden(St,St0,SoF) where (Nx,.some(Chr)).=nextChr(St) && isIdentChr(Chr) =>
    readIden(Nx,St0,[Chr,..SoF]).
  readIden(St,St0,SoF) default => (St,.some(.tok(makeLoc(St0,St),.idTok(reverse(SoF)::string)))).

  readQuoted:(tokenState,char,cons[char]) => (tokenState,option[string]).
  readQuoted(St,Qt,Chrs) where (Nx,.some(Qt)) .= nextChr(St) => (Nx,.some(reverse(Chrs)::string)).
  readQuoted(St,Qt,Chrs) where (Nx,.some(Ch)) .= charRef(St) => readQuoted(Nx,Qt,[Ch,..Chrs]).
  readQuoted(Nx,_,_) => (Nx,.none).

  readString:(tokenState,cons[stringSegment]) => (tokenState,option[cons[stringSegment]]).
  readString(St,SoFar) where (Nx,.some(`\"`)) .= nextChr(St) => (Nx,.some(reverse(SoFar))).
  readString(St,SoFar) where
    (Nx,.some(`$`)) .= nextChr(St) &&
    (_,.some(`(`)) .= nextChr(Nx) &&
    (St1,.some(Inter)) .= interpolation(Nx) => readString(St1,[Inter,..SoFar]).
  readString(St,SoFar) where
    (Nx,.some(`#`)) .= nextChr(St) &&
    (_,.some(`\(`)) .= nextChr(Nx) &&
	  (St1,.some(Inter)) .= evaluation(Nx) => readString(St1,[Inter,..SoFar]).
  readString(St,SoFar) where (St1,.some(Seg)) .= readStr(St,[]) =>
    readString(St1,[.segment(makeLoc(St,St1),Seg),..SoFar]).

  readStr:(tokenState,cons[char]) => (tokenState,option[string]).
  readStr(St,Chrs) where  `\"` ?= hedChar(St) => (St,.some(reverse(Chrs)::string)).
  readStr(St,Chrs) where Nx ?= lookingAt(St,[`$`,`(`]) => (St,.some(reverse(Chrs)::string)).
  readStr(St,Chrs) where Nx ?= lookingAt(St,[`#`,`(`]) => (St,.some(reverse(Chrs)::string)).
  readStr(St,Chrs) where (Nx,.some(Ch)) .= charRef(St) => readStr(Nx,[Ch,..Chrs]).
  readStr(St,_) => (St,.none).

  interpolation:(tokenState) => (tokenState,option[stringSegment]).
  interpolation(St) where
      (St1,.some(Chr)) .= nextChr(St) &&
    (St2,.some(Inter)) .= bracketCount(St,St1,Chr,[],[]) &&
    (St3,.some(Format)) .= readFormat(St2) &&
    (St4,IToks) .= allTks(interSt(St2,Inter)) =>
   (St3,.some(.interpolate(makeLoc(St,St3),IToks,Format))).

  evaluation:(tokenState) => (tokenState,option[stringSegment]).
  evaluation(St) where
    (St1,.some(Chr)) .= nextChr(St) &&
    (St2,.some(Inter)) .= bracketCount(St,St1,Chr,[],[]) &&
    (St3,IToks) .= allTks(interSt(St2,Inter)) =>
  (St2,.some(.evaluate(makeLoc(St,St2),IToks))).

  bracketCount:(tokenState,tokenState,char,cons[char],cons[char]) => (tokenState,option[string]).
  bracketCount(_,St1,Cl,[Cl],Chrs) => (St1,.some(reverse([Cl,..Chrs])::string)).
  bracketCount(_,St1,Cl,[Cl,..Stk],Chrs) where (St2,.some(Ch)).=nextChr(St1) =>
    bracketCount(St1,St2,Ch,Stk,[Cl,..Chrs]).
  bracketCount(_,St1,`(`,Stk,Chrs) where (St2,.some(Ch)).=nextChr(St1) =>
    bracketCount(St1,St2,Ch,[`)`,..Stk],[`\(`,..Chrs]).
  bracketCount(_,St1,`{`,Stk,Chrs) where (St2,.some(Ch)).=nextChr(St1) =>
    bracketCount(St1,St2,Ch,[`}`,..Stk],[`\{`,..Chrs]).
  bracketCount(_,St1,`[`,Stk,Chrs) where (St2,.some(Ch)).=nextChr(St1) =>
    bracketCount(St1,St2,Ch,[`]`,..Stk],[`\[`,..Chrs]).
  bracketCount(St,_,_,[],Chrs) => (St,.some(reverse(Chrs)::string)).
  bracketCount(_,St1,C,Stk,Chrs) where (St2,.some(Ch)) .= nextChr(St1) =>
    bracketCount(St1,St2,Ch,Stk,[C,..Chrs]).

  readFormat(St) where (St1,.some(`:`)) .= nextChr(St) => readUntil(St1,`;`,[]).
  readFormat(St) => (St,.some("")).

  readUntil:(tokenState,char,cons[char]) => (tokenState,option[string]).
  readUntil(St,Qt,Chrs) where (Nx,.some(Qt)) .= nextChr(St) => (Nx,.some(reverse(Chrs)::string)).
  readUntil(St,Qt,Chrs) where (Nx,.some(Ch)) .= charRef(St) => readUntil(Nx,Qt,[Ch,..Chrs]).
  readUntil(St,_,_) => (St,.none).

  stringBlob:(tokenState,tokenState,cons[char]) => (tokenState,option[token]).
  stringBlob(St,St0,Sf) where St1 ?= lookingAt(St,[`\"`,`\"`,`\"`]) &&
    Lc .= makeLoc(St0,St1) =>
    (St1,.some(.tok(Lc,.strTok([.segment(Lc,reverse(Sf)::string)])))).
  stringBlob(St,St0,Sf) where (St1,.some(Nx)) .= nextChr(St) =>
    stringBlob(St1,St0,[Nx,..Sf]).

  charRef(St) where Nx ?= lookingAt(St,[`\\`]) && (Nxt,.some(Ch)) .= nextChr(Nx) => backslashRef(Nxt,Ch).
  charRef(St) => nextChr(St).

  backslashRef:(tokenState,char) => (tokenState,option[char]).
  backslashRef(St,`a`) => (St,.some(`\a`)).
  backslashRef(St,`b`) => (St,.some(`\b`)).
  backslashRef(St,`e`) => (St,.some(`\e`)).
  backslashRef(St,`t`) => (St,.some(`\t`)).
  backslashRef(St,`n`) => (St,.some(`\n`)).
  backslashRef(St,`r`) => (St,.some(`\r`)).
  backslashRef(St,`u`) where (St1,.some(Hx)) .= hexChars(St,0) &&
      Stx ?= lookingAt(St1,[`;`]) => (Stx,.some(Hx::char)).
  backslashRef(St,Ch) => (St,.some(Ch)).

  hexChars:(tokenState,integer) => (tokenState,option[integer]).

  hexChars(St,Hx) where Hd?=hedChar(St) && Dg?=isHexDigit(Hd) =>
    hexChars(nxtSt(St),Hx*16+Dg).
  hexChars(St,Hx) => (St,.some(Hx)).

  readNumber:(tokenState) => (tokenState,option[token]).
  readNumber(St) where (Nx,.some(Mn)) .= readNatural(St,[]) => readMore(Nx,St,Mn::integer).

  readNatural:(tokenState,cons[char]) => (tokenState,option[string]).
  readNatural(St,Sf) where (Nx,.some(Dg)).=nextChr(St) && isDigit(Dg) =>
    readNatural(Nx,[Dg,..Sf]).
  readNatural(St,Sf) => (St,.some(reverse(Sf)::string)).

  readInt:(tokenState) => (tokenState,option[integer]).
  readInt(St) where Nx?=lookingAt(St,[`-`]) && (St1,.some(Nt)).=readNatural(Nx,[]) => (St1,.some(-(Nt::integer))).
  readInt(St) where (NxSt,.some(N)) .= readNatural(St,[]) => (NxSt,.some(N::integer)).

  readMore:(tokenState,tokenState,integer) => (tokenState,option[token]).
  readMore(St,St0,Sf) where St1?=lookingAt(St,[`.`]) && Hd?=hedChar(St1) && isDigit(Hd) =>
    readFraction(St1,St0,Sf::float,0.1).
  readMore(St,St0,Sf) => (St,.some(.tok(makeLoc(St0,St),.intTok(Sf)))).

  readFraction:(tokenState,tokenState,float,float) => (tokenState,option[token]).
  readFraction(St,St0,Sf,Scle) where (St1,.some(Hd)).=nextChr(St) && isDigit(Hd) =>
    readFraction(St1,St0,Sf+(digitVal(Hd)::float)*Scle,Scle*0.1).
  readFraction(St,St0,Sf,_) where St1?=lookingAt(St,[`e`]) => readExponent(St1,St0,Sf).
  readFraction(St,St0,Sf,_) => (St,.some(.tok(makeLoc(St0,St),.fltTok(Sf)))).

  readExponent:(tokenState,tokenState,float) => (tokenState,option[token]).
  readExponent(St,St0,Mn) where (St1,.some(Ix)).=readInt(St) =>
    (St1,.some(.tok(makeLoc(St0,St1),.fltTok(Mn*(10.0**(Ix::float)))))).
  readExponent(St,St0,Mn) => (St,.some(.tok(makeLoc(St0,St),.fltTok(Mn)))).


  -- We define a tracking state to allow us to collect locations
  tokenState ::= .tokenState(string,integer,integer,integer,cons[char]).

  atEof:(tokenState) => boolean.
  atEof(.tokenState(_,_,_,_,Str)) => _eof(Str).

  nextChr:(tokenState) => (tokenState,option[char]).
  nextChr(St) where .tokenState(_,_,_,_,.cons(Ch,_)).=St  => (nxtSt(St),.some(Ch)).
  nextChr(St) default => (St,.none).

  hedChar:(tokenState) => option[char].
  hedChar(.tokenState(_,_,_,_,Txt)) => head(Txt).

  interSt:(tokenState,string) => tokenState.
  interSt(.tokenState(P,Ln,Cl,Off,_),Txt) => .tokenState(P,Ln,Cl,Off,Txt::cons[char]).

  nxtSt:(tokenState) => tokenState.
  nxtSt(.tokenState(Pk,Line,Col,Off,.cons(`\n`,Txt))) =>
    .tokenState(Pk,Line+1,1,Off+1,Txt).
  nxtSt(.tokenState(Pk,Line,Col,Off,.cons(_,Txt))) =>
    .tokenState(Pk,Line,Col+1,Off+1,Txt).

  lookingAt:(tokenState,cons[char]) => option[tokenState].
  lookingAt(St,[]) => .some(St).
  lookingAt(St,[Ch,..Nxt]) where Ch?=hedChar(St) => lookingAt(nxtSt(St),Nxt).
  lookingAt(_,_) default => .none.

  makeLoc:(tokenState,tokenState)=>locn.
  makeLoc(.tokenState(Pk,Line,Col,Start,_),.tokenState(_,_,_,End,_)) =>
    .locn(Pk,Line,Col,Start,End-Start).

  skipToNx:(tokenState) => tokenState.
  skipToNx(St) where Ch ?= hedChar(St) && isNonPrint(Ch) => skipToNx(nxtSt(St)).
  skipToNx(St) where Nx ?= lookingAt(St,[`-`,`-`,` `]) => skipToNx(lineComment(Nx)).
  skipToNx(St) where Nx ?= lookingAt(St,[`-`,`-`,`\n`]) => skipToNx(Nx).
  skipToNx(St) where Nx ?= lookingAt(St,[`-`,`-`,`\t`]) => skipToNx(lineComment(Nx)).
  skipToNx(St) where Nx ?= lookingAt(St,[`/`,`*`]) => skipToNx(blockComment(Nx)).
  skipToNx(St) => St.

  lineComment(St) where Ch?=hedChar(St) => ((Ch==`\n`||_isZlChar(Ch)) ?? nxtSt(St) || lineComment(nxtSt(St))).
  lineComment(St) => St.

  blockComment(St) where Nx?=lookingAt(St,[`*`,`/`]) => Nx.
  blockComment(St) where atEof(St) => St.
  blockComment(St) default => blockComment(nxtSt(St)).

  isNonPrint:(char) => boolean.
  isNonPrint(Ch) => (_isZlChar(Ch) || _isZsChar(Ch) || _isZpChar(Ch) || _isCcChar(Ch)).

  punc(Op) => case Op in {
    `%` => .some("%").
    `&` => .some("&").
    `(` => .some("(").
    `)` => .some(")").
    `,` => .some(",").
    `.` => .some(".").
    `/` => .some("/").
    `{` => .some("{").
    `|` => .some("|").
    `}` => .some("}").
    `[` => .some("[").
    `]` => .some("]").
    `^` => .some("^").
    `:` => .some(":").
    `;` => .some(";").
    `=` => .some("=").
    `?` => .some("?").
    `@` => .some("@").
    `!` => .some("!").
    `#` => .some("#").
    _ default => .none.
  }

  implementation display[tokenState] => {
    disp(.tokenState(Pk,Line,Col,Off,_)) => disp(.locn(Pk,Line,Col,Off,0)).
  }

  implementation hasLoc[tokenState] => {
    locOf(.tokenState(Pkg,Line,Col,Start,_)) => .some(.locn(Pkg,Line,Col,Start,0)).
  }
}
  
