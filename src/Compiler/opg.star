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

  public astParser:(reports) => parser[list[token],(ast,reports)].
  astParser(Rp) --> (A,Rp1,L) <- term(Rp,2000);
                    Rpx <- terminatorCheck(Rp1,L) ^^ (A,Rpx).

  term:(reports,integer) => parser[list[token],(ast,reports,boolean)].
  term(Rp,Pr) => termLeft(Rp,Pr) >>= ((Lft,LPr,Rp1,Last)) => termRight(Lft,LPr,Pr,Rp1,Last).

  termLeft:(reports,integer) => parser[list[token],(ast,integer,reports,boolean)].
  termLeft(Rp,Pr) -->
       [tok(Lc,idTok(Id))]; ^+ [tok(_,rgtTok(_Id))] ^^ (nme(Lc,Id),0,Rp,false)
    || (Lc,Op,OPr,Rpr,Rp0) <- prefixOp(Rp,Pr);
        (Arg,Rpx,Lst) <- term(Rp,Rpr) ^^ (unary(mergeLoc(Lc,locOf(Arg)),Op,Arg),OPr,Rpx,Lst)
    || (T,Rpx,Lst) <- term0(Rp) ^^ (T,0,Rpx,Lst).

  prefixOp:(reports,integer) => parser[list[token],(locn,string,integer,integer,reports)].
  prefixOp(Rp,Pr) -->
       [tok(Lc,idTok(Op))]; {(PPr,PRgt) ^= isPrefixOp(Op) && PPr=<Pr} ^^ (Lc,Op,PPr,PRgt,Rp).

  termRight:(ast,integer,integer,reports,boolean) => parser[list[token],(ast,reports,boolean)].
  termRight(Lhs,LPr,Pr,Rp,Last) -->
        [tok(Lc,idTok(Op))] ;
          {(ILft,IPr,IRgt) ^= isInfixOp(Op)  && IPr=<Pr && ILft>=LPr &&
          (PLft,PPr) ^= isPostfixOp(Op) && PPr=<Pr && PLft>=LPr };
        legalNextRight(IRgt); -- use infix?
        (Rhs,Rp1,RLast) <- term(Rp,IRgt);
        termRight(binary(mergeLoc(locOf(Lhs),mergeLoc(Lc,locOf(Rhs))),Op,Lhs,Rhs),
                  IRgt,Pr,Rp1,RLast)
    || [tok(Lc,idTok(Op))] ; {(PLft,PPr) ^= isPostfixOp(Op) && PPr=<Pr && PLft>=LPr};
        termRight(unary(mergeLoc(locOf(Lhs),Lc),Op,Lhs), PPr, Pr,Rp, false)
    || [tok(Lc,idTok(Op))] ;
        { (ILft,IPr,IRgt) ^= isInfixOp(Op) && IPr=<Pr && ILft>=LPr };
        (Rhs,Rp1,RLast) <- term(Rp,IRgt);
        termRight(binary(mergeLoc(locOf(Lhs),locOf(Rhs)),Op,Lhs,Rhs),IRgt,Pr,Rp1,RLast)
    || return (Lhs,Rp,Last).

  legalNextRight:(integer)=>parser[list[token],()].
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

  term0:(reports) => parser[list[token],(ast,reports,boolean)].
  term0(Rp) => let{
    tt:(token) => (ast,reports,boolean).
    tt(tok(Lc,intTok(Ix))) => (lit(Lc,intgr(Ix)),Rp,false).
    tt(tok(Lc,fltTok(Dx))) => (lit(Lc,flot(Dx)),Rp,false).
    tt(tok(Lc,strTok(Sx))) => handleInterpolation(Sx,Rp,Lc).
  } in (_item >>= (Tk) => return tt(Tk)).

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

  terminatorCheck:(reports,boolean) => parser[list[token],reports].
  terminatorCheck(R,true) => terminator(R).
  terminatorCheck(R,false) => return R.

  terminator:(reports)=>parser[list[token],reports].
  terminator(R) => (_sat((tok(Lc,Tk))=>Tk==idTok(". ")) >>= (_) => return R)
               +++ (_hed(_item >>= (Tk)=>return reportError(R,"terminator expected",locOf(Tk)))).

  isRightBracket:(token) => boolean.
  isRightBracket(tok(_,idTok(B))) => bkt(_,_,B,_) ^= isBracket(B).
}
