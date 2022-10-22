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

  public astParse:(cons[token]) => (ast,cons[token]).
  astParse(Toks) where
      (Term,_,Toks1,Needs) .= term(Toks,2000)
    => (Term,checkTerminator(Toks1,Needs)).

  term:(cons[token],integer) => (ast,integer,cons[token],needsTerm).
  term(Toks,Priority) => termRight(termLeft(Toks,Priority),Priority).

  termLeft:(cons[token],integer) => (ast,integer,cons[token],needsTerm).
  termLeft([.tok(Lc,.idTok(Id)),.tok(Lc1,.rgtTok(Par)),..Toks],_) =>
    (nme(.some(Lc),Id),0,[.tok(Lc1,.rgtTok(Par)),..Toks],.needOne).
  termLeft([.tok(Lc,.idTok(Op)),..Toks],Priority) where
      (PPr,PRgt)?=isPrefixOp(Op) && PPr=<Priority &&
      (Arg,_,RToks,Needs) .= term(Toks,PRgt) =>
    (unary(mergeLoc(.some(Lc),locOf(Arg)),Op,Arg),PPr,RToks,Needs).
  termLeft([.endTok(Lc),..Toks],_) => valof{
    reportError("end of file on input",.some(Lc));
    valis (nme(.some(Lc),"_eof"),0,[endTok(Lc),..Toks],.noNeed)
  }
  
  termLeft(Toks,_) => term0(Toks).

  termRight:((ast,integer,cons[token],needsTerm),integer) => (ast,integer,cons[token],needsTerm).
  termRight((Lhs,LeftPriority,[.tok(Lc,.idTok(Op)),..Toks],LeftNeed),Priority) where
      (ILft,IPr,IRgt) ?= isInfixOp(Op) && IPr=<Priority && ILft>=LeftPriority &&
      (PLft,PPr) ?= isPostfixOp(Op) && PPr=<Priority && PLft>=LeftPriority &&
      legalNextRight(Toks,IRgt) &&	-- Use infix
      (Rhs,RPriority,RToks,Needs) .= term(Toks,IRgt) =>
    termRight((binary(mergeLoc(locOf(Lhs),locOf(Rhs)),Op,Lhs,Rhs),RPriority,RToks,Needs),Priority).
  termRight((Lhs,LeftPriority,[.tok(Lc,.idTok(Op)),..Toks],LeftNeed),Priority) where
      (ILft,IPr,IRgt) ?= isInfixOp(Op) && IPr=<Priority && ILft>=LeftPriority &&
      legalNextRight(Toks,IRgt) &&
    (Rhs,RPriority,RToks,Needs) .= term(Toks,IRgt) =>
    termRight((binary(mergeLoc(locOf(Lhs),locOf(Rhs)),Op,Lhs,Rhs),RPriority,RToks,Needs),Priority).
  termRight((Lhs,LeftPriority,[.tok(Lc,.idTok(Op)),..Toks],LeftNeed),Priority) where
      (PLft,PPr) ?= isPostfixOp(Op) &&
      PPr=<Priority && PLft>=LeftPriority &&
      ~legalNextRight(Toks,PPr) =>
    termRight((unary(mergeLoc(locOf(Lhs),.some(Lc)),Op,Lhs),PPr,Toks,.needOne),Priority).
  termRight(Left,_) => Left.

  legalNextRight:(cons[token],integer) => boolean.
  legalNextRight([.tok(_,Tk),.._],Priority) => legalRight(Tk,Priority).
  legalNextRight([],_) => .false.

  legalRight:(tk,integer) => boolean.
  legalRight(.idTok(". "),_) => .false.
  legalRight(.idTok(Op),Pr) where (PPr,_) ?= isPrefixOp(Op) => PPr=<Pr.
  legalRight(.idTok(Op),_) => ~ isOperator(Op).
  legalRight(.idQTok(_),_) => .true.
  legalRight(.intTok(_),_) => .true.
  legalRight(.bigTok(_),_) => .true.
  legalRight(.fltTok(_),_) => .true.
  legalRight(.chrTok(_),_) => .true.
  legalRight(.strTok(_),_) => .true.
  legalRight(.lftTok(_),_) => .true.
  legalRight(_,_) default => .false.

  term0:(cons[token]) => (ast,integer,cons[token],needsTerm).
  term0([.tok(Lc,.intTok(Ix)),..Toks]) => (.int(.some(Lc),Ix),0,Toks,.needOne).
  term0([.tok(Lc,.bigTok(Ix)),..Toks]) => (.big(.some(Lc),Ix),0,Toks,.needOne).
  term0([.tok(Lc,.fltTok(Dx)),..Toks]) => (.num(.some(Lc),Dx),0,Toks,.needOne).
  term0([.tok(Lc,.chrTok(Ch)),..Toks]) => (.chr(.some(Lc),Ch),0,Toks,.needOne).
  term0([.tok(Lc,.strTok(Sx)),..Toks]) where Term.=interpolateString(Sx,.some(Lc)) =>
    (Term,0,Toks,.needOne).
  term0([.tok(Lc,.lftTok("{}")),.tok(Lc1,.rgtTok("{}")),..Toks]) =>
    (.tpl(mergeLoc(.some(Lc),.some(Lc1)),"{}",[]),0,Toks,.noNeed).
  term0([.tok(Lc,.lftTok("{}")),..Toks]) where
      (Els,Toks1) .= terms(Toks,.rgtTok("{}"),[]) &&
      (Lc2,Toksx) .= checkToken(.rgtTok("{}"),Toks1) =>
    (.tpl(mergeLoc(.some(Lc),Lc2),"{}",Els),0,Toksx,.noNeed).
  term0([.tok(Lc,.lftTok("{..}")),.tok(Lc1,.rgtTok("{..}")),..Toks]) =>
    (.tpl(mergeLoc(.some(Lc),.some(Lc1)),"{..}",[]),0,Toks,.noNeed).
  term0([.tok(Lc,.lftTok("{..}")),..Toks]) where
      (Els,Toks1) .= terms(Toks,.rgtTok("{..}"),[]) &&
      (Lc2,Toksx) .= checkToken(.rgtTok("{..}"),Toks1) =>
    (.tpl(mergeLoc(.some(Lc),Lc2),"{..}",Els),0,Toksx,.noNeed).
  term0(Toks) => termArgs(term00(Toks)).

  term00:(cons[token]) => (ast,cons[token],needsTerm).
  term00([.tok(Lc,.idTok(Nm)),..Toks]) where
      [.tok(_,.rgtTok(_)),.._].=Toks => (.nme(.some(Lc),Nm),Toks,.needOne).
  term00([.tok(Lc,.idTok(Nm)),..Toks]) where ~isOperator(Nm) =>
    (.nme(.some(Lc),Nm),Toks,.needOne).
  term00([.tok(Lc,.idQTok(Nm)),..Toks]) => (.nme(.some(Lc),Nm),Toks,.needOne).
  term00([.tok(Lc,.lftTok(Lbl)),.tok(Lc1,.rgtTok(Lbl)),..Toks]) =>
    (.tpl(mergeLoc(.some(Lc),.some(Lc1)),Lbl,[]),Toks,.needOne).
  term00([.tok(Lc,.lftTok(Lbl)),..Toks]) where
      .bkt(_,Lbl,_,_,Inner) ?= isBracket(Lbl) &&
    (Arg,_,Toks1,_) .= term(Toks,Inner) &&
    (Lc2,Toks2) .= checkToken(.rgtTok(Lbl),Toks1) =>
    (genBkt(mergeLoc(.some(Lc),Lc2),Lbl,Arg),Toks2,.needOne).
  term00([Tk,..Toks]) => valof{
    reportError("problem with $(Tk)",locOf(Tk));
    valis term00(Toks)
  }

  genBkt(Lc,"[]",Arg)=>.tpl(Lc,"[]",deComma(Arg)).
  genBkt(Lc,"()",Arg)=>.tpl(Lc,"()",deComma(Arg)).
  genBkt(Lc,"(||)",Arg)=>unary(Lc,"(||)",Arg).
  genBkt(Lc,"[||]",Arg)=>unary(Lc,"[||]",Arg).
  genBkt(Lc,"<||>",Arg)=>unary(Lc,"<||>",Arg).

  termArgs:((ast,cons[token],needsTerm)) => (ast,integer,cons[token],needsTerm).
  termArgs((Left,[.tok(Lc,.lftTok("()")),.tok(Lcx,.rgtTok("()")),..Toks],_)) =>
    termArgs((app(mergeLoc(locOf(Left),.some(Lcx)),Left,.tpl(mergeLoc(.some(Lc),.some(Lcx)),"()",[])),Toks,.needOne)).
  termArgs((Lhs,[.tok(Lc,.lftTok("()")),..Toks],_)) where
      (Arg,_,Toks1,_) .= term(Toks,2000) &&
      (Lc2,Toks2) .= checkToken(.rgtTok("()"),Toks1) =>
    termArgs((app(mergeLoc(locOf(Lhs),Lc2),Lhs,.tpl(mergeLoc(.some(Lc),Lc2),"()",deComma(Arg))),Toks2,.needOne)).
  termArgs((Left,[.tok(Lc,.lftTok("[]")),.tok(Lcx,.rgtTok("[]")),..Toks],_)) =>
    termArgs((app(mergeLoc(locOf(Left),.some(Lcx)),Left,.tpl(mergeLoc(.some(Lc),.some(Lcx)),"[]",[])),Toks,.needOne)).
  termArgs((Lhs,[.tok(Lc,.lftTok("[]")),..Toks],_)) where
      (Arg,_,Toks1,_) .= term(Toks,2000) &&
      (Lc2,Toks2) .= checkToken(.rgtTok("[]"),Toks1) =>
    termArgs((app(mergeLoc(locOf(Lhs),Lc2),Lhs,.tpl(mergeLoc(.some(Lc),Lc2),"[]",deComma(Arg))),Toks2,.needOne)).
  termArgs((Left,[.tok(Lc,.lftTok("{}")),.tok(Lcx,.rgtTok("{}")),..Toks],_)) =>
    (app(mergeLoc(locOf(Left),.some(Lcx)),Left,.tpl(mergeLoc(.some(Lc),.some(Lcx)),"{}",[])),0,Toks,.noNeed).
  termArgs((Lhs,[.tok(Lc,.lftTok("{}")),..Toks],_)) where
      (Els,Toks1) .= terms(Toks,.rgtTok("{}"),[]) &&
      (Lc2,Toks2) .= checkToken(.rgtTok("{}"),Toks1) =>
    (app(mergeLoc(locOf(Lhs),Lc2),Lhs,.tpl(mergeLoc(.some(Lc),Lc2),"{}",Els)),0,Toks2,.noNeed).
  termArgs((Lhs,[.tok(Lc,.lftTok("{..}")),.tok(Lcx,.rgtTok("{..}")),..Toks],_)) =>
    (app(mergeLoc(locOf(Lhs),.some(Lcx)),Lhs,.tpl(mergeLoc(.some(Lc),.some(Lcx)),"{..}",[])),0,Toks,.noNeed).
  termArgs((Lhs,[.tok(Lc,.lftTok("{..}")),..Toks],_)) where
      (Els,Toks1) .= terms(Toks,.rgtTok("{..}"),[]) &&
      (Lc2,Toks2) .= checkToken(.rgtTok("{..}"),Toks1) =>
    (app(mergeLoc(locOf(Lhs),Lc2),Lhs,.tpl(mergeLoc(.some(Lc),Lc2),"{..}",Els)),0,Toks2,.noNeed).
  termArgs((Left,[.tok(Lc,.idTok(".")),.tok(Lcx,.idTok(Fld)),..Toks],_)) =>
    termArgs((binary(mergeLoc(locOf(Left),.some(Lcx)),".",Left,.nme(.some(Lcx),Fld)),Toks,.needOne)).
  termArgs((Left,Toks,Needs)) => (Left,0,Toks,Needs).

  terms:(cons[token],tk,cons[ast]) => (cons[ast],cons[token]).
  terms([.tok(Lc,Tk),..Toks],Tk,SoFar) => (reverse(SoFar),[.tok(Lc,Tk),..Toks]).
  terms(Toks,Term,SoFar) where
    (El,Toks1) .= astParse(Toks) =>
    terms(Toks1,Term,[El,..SoFar]).
  terms(Toks,_,SoFar) => (SoFar,Toks).

  deComma:(ast)=>cons[ast].
  deComma(Trm) where (_,Lhs,Rhs) ?= isBinary(Trm,",") => [Lhs,..deComma(Rhs)].
  deComma(Trm) => [Trm].

  interpolateString:(cons[stringSegment],option[locn]) => ast.
  interpolateString([Seg],_) => handleInterpolation(Seg).
  interpolateString(Els,Lc) where size(Els)>1 =>
    unary(Lc,"_str_multicat",handleInterpolations(Els,Lc)).

  handleInterpolations:(cons[stringSegment],option[locn]) => ast.
  handleInterpolations(.nil,Lc) => enum(Lc,"nil").
  handleInterpolations(.cons(H,T),Lc) where
      HH .= handleInterpolation(H) &&
      TT .= handleInterpolations(T,Lc) =>
    binary(Lc,"cons",HH,TT).

  handleInterpolation:(stringSegment) => ast.
  handleInterpolation(.segment(Lc,Str)) => .str(.some(Lc),Str).
  handleInterpolation(.interpolate(Lc,Toks,"")) where
      (A,_) .= astParse(Toks) => unary(.some(Lc),"disp",A).
  handleInterpolation(.interpolate(Lc,Toks,Frmt)) where
      (A,_) .= astParse(Toks) => binary(.some(Lc),"frmt",A,.str(.some(Lc),Frmt)).
  handleInterpolation(.evaluate(Lc,Toks)) where (A,_) .= astParse(Toks) => A.
  
  checkToken:(tk,cons[token]) => (option[locn],cons[token]).
  checkToken(Tk,[.tok(Lc,Tk),..Toks]) => (.some(Lc),Toks).
  checkToken(Tk,[.tok(Lc,T),..Toks]) => valof{
    reportError("missing $(Tk)",.some(Lc));
    valis (.some(Lc),[.tok(Lc,T),..Toks])
  }
  checkToken(Tk,[.endTok(Lc),..Toks]) => valof{
    reportError("missing $(Tk) - end of input",.some(Lc));    
    valis (.some(Lc),[endTok(Lc),..Toks])
  }.

  checkTerminator:(cons[token],needsTerm) => cons[token].
  checkTerminator([.tok(_,.idTok(". ")),..Toks],_) => Toks.
  checkTerminator([.endTok(Lc)],_) => [].
  checkTerminator([],_) => [].
  checkTerminator(Toks,.noNeed) => Toks.
  checkTerminator([.tok(Lc,.rgtTok("{}")),..Toks],.needOne) => [.tok(Lc,.rgtTok("{}")),..Toks].
  checkTerminator([.tok(Lc,.rgtTok("{..}")),..Toks],.needOne) => [.tok(Lc,.rgtTok("{..}")),..Toks].
  checkTerminator([.tok(Lc,T),..Toks],.needOne) default => valof{
    reportError("missing terminator",.some(Lc));
    valis [.tok(Lc,T),..Toks]
  }

  implementation display[needsTerm] => {
    disp(.needOne) => "needs terminator".
    disp(.noNeed) => "terminator optional".
  }
}
