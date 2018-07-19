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


  astParser:(reports) => parser[token,(ast,reports)].
  astParser(Rp) --> (A,Rp1,L) <- term(Tp,2000),
                    Rpx <- terminatorCheck(Rp1,L) ^^ (A,Rpx).

  term:(reports,integer) => parser[token,(ast,reports,boolean)].
  term(Rp,Pr) => termLeft(Rp,Pr) >>= ((Lft,LPr,Rp1,Last)) => termRight(Lft,LPr,Pr,Rp1,Last).

  termLeft:(reports,integer) => parser[token,(ast,integer,reports,boolean)].
  termLeft(Rp,Pr) -->
       [tok(Lc,idTok(Id))], ^+ [tok(Lc,rgtTok(_))] ^^ nme(Lc,Id)
    || [tok(Lc,idTok(Op)) where (PPr,PRgt) ^= isPrefixOp(Op) && PPr=<Pr],
        (Arg,Rpx,Lst) <- term(Rp,RPr) ^^ (unary(mergeLoc(locOf(Op),locOf(Arg)),Op,Arg),RPr,Rpx,Lst)
    || (T,Rpx,Lst) <- term0(Rp) ^^ (T,0,Rpx,Lst).

  termRight:(ast,integer,integer,reports,boolean) => parser[token,(ast,reports,boolean)].
  termRight(Lhs,LPr,Pr,Rp,Last) -->
        [tok(Lc,idTok(Op))] where
          (ILft,IPr,IRgt) ^= isInfixOp(Op)  && IPr=<Pr && ILft>=LPr &&
          (PLft,PPr) ^= isPostfixOp(Op) && PPr=<Pr && PLft>=LPr,
        legalNextRight, -- use infix
        (Rhs,Rp1,RLast) <- term(Rp,IRgt),
        termRight(binary(mergeLoc(locOf(L),mergeLoc(Lc),locOf(Rhs)),nme(Lc,Op),Lhs,Rhs),
                  PrOp,Pr,Rp1,RLast)
    || [tok(Lc,idTok(Op))] where (PLft,PPr) ^= isPostfixOp(Op) && PPr=<Pr && PLft>=LPr,
        termRight(unary(mergeLoc(locOf(L),Lc),nme(Lc,Op),Lhs), PPr, Pr,Rp, false)
    || [tok(Lc,idTok(Op))] where
                          (ILft,IPr,IRgt) ^= isInfixOp(Op)  && IPr=<Pr && ILft>=LPr &&
        (Rhs,Rp1,RLast) <- term(Rp,IRgt),
        termRight(binary(mergeLoc(locOf(L),mergeLoc(Lc),locOf(Rhs)),nme(Lc,Op),Lhs,Rhs),
                  PrOp,Pr,Rp1,RLast)
    || return (Lhs,Rp,Last).

  legalNextRight:(integer)=>parser[token,()].
  legalNextRight(Pr) --> ^+ [tok(_,Tk) where legalRight(Tk,Pr)] ^^ ().

  legalRight:(tk,integer) => boolean.
  legalRight(idTok(Op),Pr) where (PPr,_) ^= isPrefixOp(Op) => PPr=<Pr.
  legalRight(idTok(Op),_) => \+ isOperator(Op).
  legalRight(idQTok(_),_) => true.
  legalRight(intTok(_),_) => true.
  legalRight(fltTok(_),_) => true.
  legalRight(strTok(_),_) => true.
  legalRight(lftTok(_),_) => true.
  legalRight(_,_) default => false.

  term0:(reports) => parser[token,(ast,reports,boolean)].
  term0(Rp) => let{
    tt(tok(Lc,intTok(Ix))) => lit(Lc,intgr(Ix)).
    tt(tok(Lc,fltTok(Dx))) => lit(Lc,flot(Dx)).
    tt(tok(Lc,strTok(Sx))) => lit(Lc,strg(Sx)).
  } in _item >>= (Tk) => return (tt(Tk),Rp,false).

  terminatorCheck:(ast,reports,boolean) => parser[token,(ast,reports)].
  terminatorCheck(A,R,true) => terminator(A,R).
  terminatorCheck(A,R,false) => return (A,R).

  terminator:(ast,reports)=>parser[token,(ast,reports)].
  terminator(A,R) => (_sat((tok(_,Tk))=>Tk==idTok(". ")) >>= (_) => return (A,R))
               +++ (_hed(_item >>= (Tk)=>return (A,reportError(R,"terminator expected",locOf(Tk))))).


  isRightBracket:(token) => boolean.
  isRightBracket(tok(_,idTok(B))) => bkt(_,_,B,_) ^= isBracket(B).
}
