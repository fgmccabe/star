star.compiler.opg{
  import star.

  import star.compiler.ast.
  import star.compiler.operators.
  import star.compiler.errors.
  import star.compiler.lexer.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.token.
  import star.compiler.wff.
  import star.pkg.

  needsTerm ::= .noNeed | .needOne.

  public astParse:(cons[token],reports) => (ast,reports,cons[token]).
  astParse(Toks,Rpt) where
      (Term,_,Toks1,Rpt1,Needs) .= term(Toks,Rpt,2000) &&
      (Rpt2,Toksx) .= checkTerminator(Rpt1,Toks1,Needs) => (Term,Rpt2,Toksx).

  term:(cons[token],reports,integer) => (ast,integer,cons[token],reports,needsTerm).
  term(Toks,Rpt,Priority) => termRight(termLeft(Toks,Rpt,Priority),Priority).

  termLeft:(cons[token],reports,integer) => (ast,integer,cons[token],reports,needsTerm).
  termLeft([tok(Lc,idTok(Id)),tok(Lc1,rgtTok(Par)),..Toks],Rpt,_) =>
    (nme(Lc,Id),0,[tok(Lc1,rgtTok(Par)),..Toks],Rpt,.needOne).
  termLeft([tok(Lc,idTok(Op)),..Toks],Rpt,Priority) where
      (PPr,PRgt)^=isPrefixOp(Op) && PPr=<Priority &&
      (Arg,_,RToks,Rpt1,Needs) .= term(Toks,Rpt,PRgt) =>
    (unary(mergeLoc(Lc,locOf(Arg)),Op,Arg),PPr,RToks,Rpt1,Needs).
  termLeft([endTok(Lc),..Toks],Rpt,_) => (nme(Lc,"_eof"),0,[endTok(Lc),..Toks],reportError(Rpt,"end of file on input",Lc),.noNeed).
  
  termLeft(Toks,Rpt,_) => term0(Toks,Rpt).

  termRight:((ast,integer,cons[token],reports,needsTerm),integer) => (ast,integer,cons[token],reports,needsTerm).
  termRight((Lhs,LeftPriority,[tok(Lc,idTok(Op)),..Toks],Rpt,LeftNeed),Priority) where
      (ILft,IPr,IRgt) ^= isInfixOp(Op) && IPr=<Priority && ILft>=LeftPriority &&
      (PLft,PPr) ^= isPostfixOp(Op) && PPr=<Priority && PLft>=LeftPriority &&
    legalNextRight(Toks,IRgt) && -- Use infix
      (Rhs,RPriority,RToks,Rpt1,Needs) .= term(Toks,Rpt,IRgt) =>
    termRight((binary(mergeLoc(locOf(Lhs),locOf(Rhs)),Op,Lhs,Rhs),RPriority,RToks,Rpt1,Needs),Priority).
  termRight((Lhs,LeftPriority,[tok(Lc,idTok(Op)),..Toks],Rpt,LeftNeed),Priority) where
      (ILft,IPr,IRgt) ^= isInfixOp(Op) && IPr=<Priority && ILft>=LeftPriority &&
    -- _ .= _logmsg("$(Op) is an infix operator") &&
    (Rhs,RPriority,RToks,Rpt1,Needs) .= term(Toks,Rpt,IRgt) =>
    termRight((binary(mergeLoc(locOf(Lhs),locOf(Rhs)),Op,Lhs,Rhs),RPriority,RToks,Rpt1,Needs),Priority).
  termRight((Lhs,LeftPriority,[tok(Lc,idTok(Op)),..Toks],Rpt,LeftNeed),Priority) where
      (PLft,PPr) ^= isPostfixOp(Op) &&
      PPr=<Priority && PLft>=LeftPriority &&
      ~legalNextRight(Toks,PPr) =>
    termRight((unary(mergeLoc(locOf(Lhs),Lc),Op,Lhs),PPr,Toks,Rpt,.needOne),Priority).
  termRight(Left,_) => Left.

  legalNextRight:(cons[token],integer) => boolean.
  legalNextRight([tok(_,Tk),.._],Priority) => legalRight(Tk,Priority).
  legalNextRight([],_) => .false.

  legalRight:(tk,integer) => boolean.
  legalRight(idTok(". "),_) => .false.
  legalRight(idTok(Op),Pr) where (PPr,_) ^= isPrefixOp(Op) => PPr=<Pr.
  legalRight(idTok(Op),_) => ~ isOperator(Op).
  legalRight(idQTok(_),_) => .true.
  legalRight(intTok(_),_) => .true.
  legalRight(bigTok(_),_) => .true.
  legalRight(fltTok(_),_) => .true.
  legalRight(chrTok(_),_) => .true.
  legalRight(strTok(_),_) => .true.
  legalRight(lftTok(_),_) => .true.
  legalRight(_,_) default => .false.

  term0:(cons[token],reports) => (ast,integer,cons[token],reports,needsTerm).
  term0([tok(Lc,intTok(Ix)),..Toks],Rpt) => (int(Lc,Ix),0,Toks,Rpt,.needOne).
  term0([tok(Lc,bigTok(Ix)),..Toks],Rpt) => (big(Lc,Ix),0,Toks,Rpt,.needOne).
  term0([tok(Lc,fltTok(Dx)),..Toks],Rpt) => (num(Lc,Dx),0,Toks,Rpt,.needOne).
  term0([tok(Lc,chrTok(Ch)),..Toks],Rpt) => (chr(Lc,Ch),0,Toks,Rpt,.needOne).
  term0([tok(Lc,strTok(Sx)),..Toks],Rpt) where (Term,Rptx).=interpolateString(Sx,Rpt,Lc) =>
    (Term,0,Toks,Rptx,.needOne).
  term0([tok(Lc,lftTok("{}")),tok(Lc1,rgtTok("{}")),..Toks],Rpt) => (tpl(mergeLoc(Lc,Lc1),"{}",[]),0,Toks,Rpt,.noNeed).
  term0([tok(Lc,lftTok("{}")),..Toks],Rpt) where
      (Els,Rpt1,Toks1) .= terms(Toks,rgtTok("{}"),Rpt,[]) &&
      (Lc2,Rpt2,Toksx) .= checkToken(rgtTok("{}"),Rpt1,Toks1) =>
        (tpl(mergeLoc(Lc,Lc2),"{}",Els),0,Toksx,Rpt2,.noNeed).
  term0([tok(Lc,lftTok("@<@>")),tok(Lc1,rgtTok("@<@>")),..Toks],Rpt) => (tpl(mergeLoc(Lc,Lc1),"@<@>",[]),0,Toks,Rpt,.noNeed).
  term0([tok(Lc,lftTok("@<@>")),..Toks],Rpt) where
      (Els,Rpt1,Toks1) .= terms(Toks,rgtTok("@<@>"),Rpt,[]) &&
      (Lc2,Rpt2,Toksx) .= checkToken(rgtTok("@<@>"),Rpt1,Toks1) =>
    (tpl(mergeLoc(Lc,Lc2),"@<@>",Els),0,Toksx,Rpt2,.noNeed).
  term0(Toks,Rpt) => termArgs(term00(Toks,Rpt)).

  term00:(cons[token],reports) => (ast,cons[token],reports,needsTerm).
  term00([tok(Lc,idTok(Nm)),..Toks],Rpt) where
    [tok(_,rgtTok(_)),.._].=Toks => (nme(Lc,Nm),Toks,Rpt,.needOne).
  term00([tok(Lc,idTok(Nm)),..Toks],Rpt) where ~isOperator(Nm) => (nme(Lc,Nm),Toks,Rpt,.needOne).
  term00([tok(Lc,idQTok(Nm)),..Toks],Rpt) => (nme(Lc,Nm),Toks,Rpt,.needOne).
  term00([tok(Lc,lftTok(Lbl)),tok(Lc1,rgtTok(Lbl)),..Toks],Rpt) =>
    (tpl(mergeLoc(Lc,Lc1),Lbl,[]),Toks,Rpt,.needOne).
  term00([tok(Lc,lftTok(Lbl)),..Toks],Rpt) where
      bkt(_,Lbl,_,_,Inner) ^= isBracket(Lbl) &&
    (Arg,_,Toks1,Rpt1,_) .= term(Toks,Rpt,Inner) &&
    (Lc2,Rpt2,Toks2) .= checkToken(rgtTok(Lbl),Rpt1,Toks1) =>
    (genBkt(mergeLoc(Lc,Lc2),Lbl,Arg),Toks2,Rpt2,.needOne).
  term00([Tk,..Toks],Rp) =>
    term00(Toks,reportError(Rp,"problem with $(Tk)",locOf(Tk))).

  genBkt(Lc,"[]",Arg)=>tpl(Lc,"[]",deComma(Arg)).
  genBkt(Lc,"()",Arg)=>tpl(Lc,"()",deComma(Arg)).
  genBkt(Lc,"(||)",Arg)=>unary(Lc,"(||)",Arg).
  genBkt(Lc,"[||]",Arg)=>unary(Lc,"[||]",Arg).
  genBkt(Lc,"<||>",Arg)=>unary(Lc,"<||>",Arg).

  termArgs:((ast,cons[token],reports,needsTerm)) => (ast,integer,cons[token],reports,needsTerm).
  termArgs((Left,[tok(Lc,lftTok("()")),tok(Lcx,rgtTok("()")),..Toks],Rpt,_)) =>
    termArgs((app(mergeLoc(locOf(Left),Lcx),Left,tpl(mergeLoc(Lc,Lcx),"()",[])),Toks,Rpt,.needOne)).
  termArgs((Lhs,[tok(Lc,lftTok("()")),..Toks],Rpt,_)) where
      (Arg,_,Toks1,Rpt1,_) .= term(Toks,Rpt,2000) &&
      (Lc2,Rpt2,Toks2) .= checkToken(rgtTok("()"),Rpt1,Toks1) =>
        termArgs((app(mergeLoc(locOf(Lhs),Lc2),Lhs,tpl(mergeLoc(Lc,Lc2),"()",deComma(Arg))),Toks2,Rpt,.needOne)).
  termArgs((Left,[tok(Lc,lftTok("[]")),tok(Lcx,rgtTok("[]")),..Toks],Rpt,_)) =>
    termArgs((app(mergeLoc(locOf(Left),Lcx),Left,tpl(mergeLoc(Lc,Lcx),"[]",[])),Toks,Rpt,.needOne)).
  termArgs((Lhs,[tok(Lc,lftTok("[]")),..Toks],Rpt,_)) where
      (Arg,_,Toks1,Rpt1,_) .= term(Toks,Rpt,2000) &&
      (Lc2,Rpt2,Toks2) .= checkToken(rgtTok("[]"),Rpt1,Toks1) =>
    termArgs((app(mergeLoc(locOf(Lhs),Lc2),Lhs,tpl(mergeLoc(Lc,Lc2),"[]",deComma(Arg))),Toks2,Rpt,.needOne)).
  termArgs((Left,[tok(Lc,lftTok("{}")),tok(Lcx,rgtTok("{}")),..Toks],Rpt,_)) =>
    (app(mergeLoc(locOf(Left),Lcx),Left,tpl(mergeLoc(Lc,Lcx),"{}",[])),0,Toks,Rpt,.noNeed).
  termArgs((Lhs,[tok(Lc,lftTok("{}")),..Toks],Rpt,_)) where
      (Els,Rpt1,Toks1) .= terms(Toks,rgtTok("{}"),Rpt,[]) &&
      (Lc2,Rpt2,Toks2) .= checkToken(rgtTok("{}"),Rpt1,Toks1) =>
        (app(mergeLoc(locOf(Lhs),Lc2),Lhs,tpl(mergeLoc(Lc,Lc2),"{}",Els)),0,Toks2,Rpt,.noNeed).
  termArgs((Lhs,[tok(Lc,lftTok("@<@>")),tok(Lcx,rgtTok("@<@>")),..Toks],Rpt,_)) =>
    (app(mergeLoc(locOf(Lhs),Lcx),Lhs,tpl(mergeLoc(Lc,Lcx),"@<@>",[])),0,Toks,Rpt,.noNeed).
  termArgs((Lhs,[tok(Lc,lftTok("@<@>")),..Toks],Rpt,_)) where
      (Els,Rpt1,Toks1) .= terms(Toks,rgtTok("@<@>"),Rpt,[]) &&
      (Lc2,Rpt2,Toks2) .= checkToken(rgtTok("@<@>"),Rpt1,Toks1) =>
        (app(mergeLoc(locOf(Lhs),Lc2),Lhs,tpl(mergeLoc(Lc,Lc2),"@<@>",Els)),0,Toks2,Rpt,.noNeed).
  termArgs((Left,[tok(Lc,idTok(".")),tok(Lcx,idTok(Fld)),..Toks],Rpt,_)) =>
    termArgs((binary(mergeLoc(locOf(Left),Lcx),".",Left,nme(Lcx,Fld)),Toks,Rpt,.needOne)).
  termArgs((Left,Toks,Rpt,Needs)) => (Left,0,Toks,Rpt,Needs).

  terms:(cons[token],tk,reports,cons[ast]) => (cons[ast],reports,cons[token]).
  terms([tok(Lc,Tk),..Toks],Tk,Rpt,SoFar) => (reverse(SoFar),Rpt,[tok(Lc,Tk),..Toks]).
  terms(Toks,Term,Rpt,SoFar) where
    (El,Rpt1,Toks1) .= astParse(Toks,Rpt) =>
    terms(Toks1,Term,Rpt1,[El,..SoFar]).
  terms(Toks,_,Rpt,SoFar) => (SoFar,Rpt,Toks).

  deComma:(ast)=>cons[ast].
  deComma(Trm) where (_,Lhs,Rhs) ^= isBinary(Trm,",") => [Lhs,..deComma(Rhs)].
  deComma(Trm) => [Trm].

  interpolateString:(cons[stringSegment],reports,locn) => (ast,reports).
  interpolateString([Seg],Rp,_) => handleInterpolation(Seg,Rp).
  interpolateString(Els,Rp,Lc) where size(Els)>1 && (Interpolation,Rp1) .= handleInterpolations(Els,Rp,Lc) =>
    (unary(Lc,"_str_multicat",Interpolation),Rp1).

  handleInterpolations:(cons[stringSegment],reports,locn) => (ast,reports).
  handleInterpolations(.nil,Rp,Lc) => (enum(Lc,"nil"),Rp).
  handleInterpolations(cons(H,T),Rp,Lc) where
      (HH,Rp0) .= handleInterpolation(H,Rp) &&
      (TT,Rp1) .= handleInterpolations(T,Rp0,Lc) =>
    (binary(Lc,"cons",HH,TT),Rp1).

  handleInterpolation:(stringSegment,reports) => (ast,reports).
  handleInterpolation(segment(Lc,Str),Rpt) => (str(Lc,Str),Rpt).
  handleInterpolation(interpolate(Lc,Toks,""),Rpt) where
      (A,Rpt1,_) .= astParse(Toks,Rpt) => (unary(Lc,"disp",A),Rpt1).
  handleInterpolation(interpolate(Lc,Toks,Frmt),Rpt) where
      (A,Rpt1,_) .= astParse(Toks,Rpt) => (binary(Lc,"frmt",A,str(Lc,Frmt)),Rpt1).
  handleInterpolation(evaluate(Lc,Toks),Rp) where (A,Rpt1,_) .= astParse(Toks,Rp) => (A,Rp).
  
  checkToken:(tk,reports,cons[token]) => (locn,reports,cons[token]).
  checkToken(Tk,Rpt,[tok(Lc,Tk),..Toks]) => (Lc,Rpt,Toks).
  checkToken(Tk,Rpt,[tok(Lc,T),..Toks]) => (Lc,reportError(Rpt,"missing $(Tk)",Lc),[tok(Lc,T),..Toks]).
  checkToken(Tk,Rpt,[endTok(Lc),..Toks]) => (Lc,reportError(Rpt,"missing $(Tk) - end of input",Lc),[endTok(Lc),..Toks]).

  checkTerminator:(reports,cons[token],needsTerm) => (reports,cons[token]).
  checkTerminator(Rpt,[tok(_,idTok(". ")),..Toks],_) => (Rpt,Toks).
  checkTerminator(Rpt,[endTok(Lc)],_) => (Rpt,[]).
  checkTerminator(Rpt,[],_) => (Rpt,[]).
  checkTerminator(Rpt,Toks,.noNeed) => (Rpt,Toks).
  checkTerminator(Rpt,[tok(Lc,rgtTok("{}")),..Toks],.needOne) => (Rpt,[tok(Lc,rgtTok("{}")),..Toks]).
  checkTerminator(Rpt,[tok(Lc,rgtTok("{..}")),..Toks],.needOne) => (Rpt,[tok(Lc,rgtTok("{..}")),..Toks]).
  checkTerminator(Rpt,[tok(Lc,T),..Toks],.needOne) default => (reportError(Rpt,"missing terminator",Lc),[tok(Lc,T),..Toks]).

  implementation display[needsTerm] => {
    disp(.needOne) => "needs terminator".
    disp(.noNeed) => "terminator optional".
  }
}
