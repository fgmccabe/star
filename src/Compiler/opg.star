star.compiler.opg{
  import star.
  import star.parse.
  import star.compiler.ast.
  import star.compiler.operators.
  import star.compiler.errors.
  import star.compiler.lexer.
  import star.compiler.location.
  import star.compiler.token.
  import star.pkg.

  needsTerm ::= noNeed | needOne.

  public astParser:(reports) => parser[cons[token],(ast,reports)].
  astParser(Rp) --> (A,Rp1,L) <- term(Rp,2000);
                    Rpx <- terminatorCheck(Rp1,L) ^^ (A,Rpx).

  term:(reports,integer) => parser[cons[token],(ast,reports,needsTerm)].
  term(Rpt,Priority) -->
    (Left,LeftPrior,Rpt1,Trail) <- termLeft(Rpt,Priority);
    termRight(Left,LeftPrior,Priority,Rpt1,Trail).

  termLeft:(reports,integer) => parser[cons[token],(ast,integer,reports,needsTerm)].
  termLeft(Rp,Pr) -->
       [tok(Lc,idTok(Id))]; ^+ [tok(_,rgtTok(_))] ^^ (nme(Lc,Id),0,Rp,needOne)
    || (Lc,Op,OPr,Rpr,Rp0) <- prefixOp(Rp,Pr);
        (Arg,Rpx,Lst) <- term(Rp,Rpr) ^^ (unary(mergeLoc(Lc,locOf(Arg)),Op,Arg),OPr,Rpx,Lst)
    || (T,Rpx,Lst) <- term0(Rp) ^^ (T,0,Rpx,Lst).

  prefixOp:(reports,integer) => parser[cons[token],(locn,string,integer,integer,reports)].
  prefixOp(Rp,Pr) -->
       [tok(Lc,idTok(Op))]; {(PPr,PRgt) ^= isPrefixOp(Op) && PPr=<Pr} ^^ (Lc,Op,PPr,PRgt,Rp).

  termRight:(ast,integer,integer,reports,boolean) => parser[cons[token],(ast,reports,needsTerm)].
  termRight(Lhs,LPr,Pr,Rp,Last) -->
        [tok(Lc,idTok(Op))] ;
          {(ILft,IPr,IRgt) ^= isInfixOp(Op)  && IPr=<Pr && ILft>=LPr &&
          (PLft,PPr) ^= isPostfixOp(Op) && PPr=<Pr && PLft>=LPr };
        legalNextRight(IRgt); -- use infix?
        (Rhs,Rp1,RLast) <- term(Rp,IRgt);
        termRight(binary(mergeLoc(locOf(Lhs),mergeLoc(Lc,locOf(Rhs))),Op,Lhs,Rhs),
                  IRgt,Pr,Rp1,RLast)
    || [tok(Lc,idTok(Op))] ; {(PLft,PPr) ^= isPostfixOp(Op) && PPr=<Pr && PLft>=LPr};
        termRight(unary(mergeLoc(locOf(Lhs),Lc),Op,Lhs), PPr, Pr,Rp, needOne)
    || [tok(Lc,idTok(Op))] ;
        { (ILft,IPr,IRgt) ^= isInfixOp(Op) && IPr=<Pr && ILft>=LPr };
        (Rhs,Rp1,RLast) <- term(Rp,IRgt);
        termRight(binary(mergeLoc(locOf(Lhs),locOf(Rhs)),Op,Lhs,Rhs),IRgt,Pr,Rp1,RLast)
    || return (Lhs,Rp,Last).

  legalNextRight:(integer)=>parser[cons[token],()].
  legalNextRight(Pr) --> ^+ [tok(_Lc,Tk) where legalRight(Tk,Pr)] ^^ ().

  legalRight:(tk,integer) => boolean.
  legalRight(idTok(Op),Pr) where (PPr,_) ^= isPrefixOp(Op) => PPr=<Pr.
  legalRight(idTok(Op),_) => \+ isOperator(Op).
  legalRight(idQTok(_),_) => true.
  legalRight(intTok(_),_) => true.
  legalRight(fltTok(_),_) => true.
  legalRight(strTok(_),_) => true.
  legalRight(lftTok(_),_) => true.
  legalRight(_,_) default => false.

  term0:(reports) => parser[cons[token],(ast,reports,needsTerm)].
  term0(Rpt) -->
    [tok(Lc,intTok(Ix))] ^^ (lit(Lc,intgr(Ix)),Rpt,needOne) ||
    [tok(Lc,fltTok(Dx))] ^^ (lit(Lx,flot(Dx)),Rpt,needOne) ||
    [tok(Lc,strTok(Sx))]; handleInterpolation(Lc,Sx,Rpt) ||
    [tok(Lc,lftTok("{}"))]; [tok(Lc1,rgtTok("{}"))] ^^ (tpl(mergeLoc(Lc,Lc1),"{}",[]),Rpt,needOne) ||
    [tok(Lc,lftTok("{}"))];
      (Els,Rpt1,_) <- terms(Rpt,2000,tok(Lc,rgtTok("{}")));
      (Lc2,Rpt2) <- checkToken(tok(Lc,rgtTok("{}")),Rpt1) ^^ (tpl(mergeLoc(Lc,Lc2),"{}",Els),Rpt2,noNeed) ||
    [tok(Lc,lftTok("{..}"))]; [tok(Lc1,rgtTok("{..}"))] ^^ (tpl(mergeLoc(Lc,Lc1),"{..}",[]),Rpt,noNeed) ||
    [tok(Lc,lftTok("{..}"))];
      (Els,Rpt1,_) <- terms(Rpt,2000,tok(Lc,rgtTok("{..}")));
      (Lc2,Rpt2) <- checkToken(tok(Lc,rgtTok("{..}")),Rpt1) ^^ (tpl(mergeLoc(Lc,Lc2),"{..}",Els),Rpt2,noNeed) ||
    (Lhs,Rpt1,Lst) <- term00(Rpt); termArgs(Lhs,Rpt1,Lst).

  term00:(reports) => parser[cons[token],(ast,reports,needsTerm)].
  term00(Rpt) -->
    [tok(Lc,idTok(Nm))]; (^+[tok(_,rgtTok(_))] || \+isOperator(Nm)) ^^ (nme(Lc,Nm),Rpt,needOne) ||
    [tok(Lc,idQTok(Nm))] ^^ (nme(Lc,Nm),Rpt,needOne) ||
    [tok(Lc,lftTok(Lbl))];
       ([tok(Lc1,rgtTok(Lbl))] ^^ (tpl(mergeLoc(Lc,Lc1),Lbl,[])) ||
       { bkt(_,Lbl,_,Inner) ^= isBracket(Lbl) }; (I,Rp1,_) <- term(Rpt,Inner);
         ([tok(Lcx,rgtTok(Lbl))] ^^ (tpl(mergeLoc(Lc,Lcx),Lbl,deComma(I)),Rpt,noNeed) ||
            zed ^^ (tpl(Lc,Lbl,deComma(I)),reportError("missing close paren",Lc),noNeed))
     ).

  deComma:(ast)=>list[ast].
  deComma(Trm) where (_,",",Lhs,Rhs) ^= isBinary(Trm) => [Lhs,..deComma(Rhs)].
  deComma(Trm) => [Trm].

  handleInterpolation([segment(Lc,Str)],Rp,_) => (lit(Lc,strg(Str)),Rp,false).
  handleInterpolation([],Rp,Lc) => (lit(Lc,strg("")),Rp,false).
  handleInterpolation(Segments,Rp,Lc) where
    (Segs,Rpx) .= stringSegments(Segments,Rp,[]) => (binary(Lc,"::",unary(Lc,"ssSeq",tpl(Lc,"[]",Segs)),nme(Lc,"string")),Rpx,false).

  stringSegments:(list[stringSegment],reports,list[ast]) => (list[ast],reports).
  stringSegments([],Rp,SoFar) => (SoFar,Rp).
  stringSegments([Seg,..More],Rp,SoFar) where
    (Sg,Rp1).=stringSegment(Seg,Rp) => stringSegments(More,Rp1,[SoFar..,Sg]).

  stringSegment:(stringSegment,reports) => (ast,reports).
  stringSegment(segment(Lc,Str),Rp) => (unary(Lc,"ss",lit(Lc,strg(Str))),Rp).
  stringSegment(interpolate(Lc,Toks,Fmt),Rp) where
    ((A,Rpx,_),_) ^= head(parse(term(Rp,2000),Toks)) =>
      (Fmt=="" ?
        (unary(Lc,"disp",A),Rpx) ||
        (binary(Lc,"frmt",A,lit(Lc,strg(Fmt))),Rpx)).

  terminatorCheck:(reports,boolean) => parser[cons[token],reports].
  terminatorCheck(R,true) => terminator(R).
  terminatorCheck(R,false) => return R.

  terminator:(reports)=>parser[cons[token],reports].
  terminator(R) => (_sat((tok(Lc,Tk))=>Tk==idTok(". ")) >>= (_) => return R)
               +++ (_hed(_item >>= (Tk)=>return reportError(R,"terminator expected",locOf(Tk)))).

  isRightBracket:(token) => boolean.
  isRightBracket(tok(_,idTok(B))) => bkt(_,_,B,_) ^= isBracket(B).

  checkToken:(token,reports) => parser[cons[token],reports].
  checkToken(tok(Lc,Tk),Rp) => (_tk(tok(Lc,Tk)) >>= (_) => return Rp) +++
   (^+[tok(Lc2,T)] ^^ (reportError(Rp,"missing close bracket, left at \(Lc)",Lc2))).
}
