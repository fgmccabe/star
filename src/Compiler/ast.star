star.compiler.ast{
  import star.
  import star.compiler.location.
  import star.compiler.operators.
  import star.compiler.keywords.

  public ast ::=
      nme(locn,string)
    | lit(locn,literal)
    | tpl(locn,string,list[ast])
    | app(locn,ast,ast).

  public literal ::=
      intgr(integer)
    | flot(float)
    | strg(string).

  public implementation equality[ast] => {.
    nme(_,I1) == nme(_,I2) => I1==I2.
    lit(_,L1) == lit(_,L2) => L1==L2.
    tpl(_,K1,E1) == tpl(_,K2,E2) => K1==K2 && E1==E2.
    app(_,O1,A1) == app(_,O2,A2) => O1==O2 && A1==A2.
    _ == _ default => false.
  .}

  public implementation equality[literal] => {.
    intgr(I1) == intgr(I2) => I1==I2.
    flot(F1) == flot(F2) => F1==F2.
    strg(S1) == strg(S2) => S1==S2.
    _ == _ default => false.
  .}

  public implementation display[ast] => {.
    disp(A) => dispAst(A,2000).
  .}

  public implementation display[literal] => {.
    disp(intgr(Ix)) => disp(Ix).
    disp(flot(Dx)) => disp(Dx).
    disp(strg(Sx)) => disp(Sx).
  .}

  public dispAst:(ast,integer) => ss.
  dispAst(lit(_,Lt),_) => disp(Lt).
  dispAst(nme(_,Id),_) => dispId(Id).
  dispAst(tpl(_,"{}",Els),_) =>
    ssSeq([ss("{"),ssSeq(interleave(Els//((E)=>dispAst(E,2000)),ss(". "))),ss("}")]).
  dispAst(tpl(_,Bk,Els),_) where bkt(Lft,_,Rgt,Inn)^=isBracket(Bk) =>
    ssSeq([ss(Lft),ssSeq(interleave(Els//((E)=>dispAst(E,Inn)),ss(","))),ss(Rgt)]).
  dispAst(app(_,nme(_,Op),tpl(_,"()",[L,R])),Pr) where (Lf,P,Rg)^=isInfixOp(Op) =>
    ssSeq([leftPar(P,Pr),dispAst(L,Lf),ss(Op),dispAst(R,Rg),rightPar(P,Pr)]).
  dispAst(app(_,nme(_,Op),tpl(_,"()",[R])),Pr) where (P,Rg)^=isPrefixOp(Op) =>
    ssSeq([leftPar(P,Pr),ss(Op),dispAst(R,Rg),rightPar(P,Pr)]).
  dispAst(app(_,nme(_,Op),tpl(_,"()",[R])),Pr) where (P,Rg)^=isPostfixOp(Op) =>
    ssSeq([leftPar(P,Pr),dispAst(R,Rg),ss(Op),rightPar(P,Pr)]).
  dispAst(app(_,Op,A),_) => ssSeq([dispAst(Op,0),dispAst(A,0)]).

  dispId:(string) => ss.
  dispId(S) where isOperator(S) => ssSeq([ss("("),ss(S),ss(")")]).
  dispId(S) => ss(S). -- temporary until we can fix better

  leftPar:(integer,integer) => ss.
  leftPar(P,Pr) where P>Pr => ss("(").
  leftPar(_,_) default => ss("").

  rightPar:(integer,integer) => ss.
  rightPar(P,Pr) where P>Pr => ss(")").
  rightPar(_,_) default => ss("").

  public implementation hasLoc[ast] => {.
    locOf(nme(Lc,_)) => Lc.
    locOf(lit(Lc,_)) => Lc.
    locOf(tpl(Lc,_,_)) => Lc.
    locOf(app(Lc,_,_)) => Lc.
  .}

  public isName:(ast) => option[(locn,string)].
  isName(nme(Lc,Id)) where \+ keyword(Id) => some((Lc,Id)).
  isName(tpl(_,"()",[nme(Lc,Id)])) => some((Lc,Id)).
  isName(_) default => none.

  public isKeyword:(ast) => option[(locn,string)].
  isKeyword(nme(Lc,Id)) where keyword(Id) => some((Lc,Id)).
  isKeyword(_) default => none.
  
  public isInt:(ast) => option[(locn,integer)].
  isInt(lit(Lc,intgr(Ix))) => some((Lc,Ix)).
  isInt(_) default => none.

  public unary:(locn,string,ast) => ast.
  unary(Lc,Op,Arg) => app(Lc,nme(Lc,Op),tpl(locOf(Arg),"()",[Arg])).

  public isUnary:(ast,string) => option[(locn,ast)].
  isUnary(app(Lc,nme(_,Op),tpl(_,"()",[A])),Op) => some((Lc,A)).
  isUnary(_,_) default => none.

  public binary:(locn,string,ast,ast) => ast.
  binary(Lc,Op,L,R) where Lc.=mergeLoc(locOf(L),locOf(R)) =>
    app(Lc,nme(Lc,Op),tpl(Lc,"()",[L,R])).

  public isBinary:(ast,string) => option[(locn,ast,ast)].
  isBinary(app(Lc,nme(_,Op),tpl(_,"()",[L,R])),Op) => some((Lc,L,R)).
  isBinary(_,_) default => none.

  public isSquareTerm:(ast) => option[(locn,ast,list[ast])].
  isSquareTerm(app(Lc,Op,tpl(_,"[]",A))) => some((Lc,Op,A)).
  isSquareTerm(_) default => none.

  public isSquareApply:(ast) => option[(locn,string,list[ast])].
  isSquareApply(app(Lc,Op,tpl(_,"[]",A))) where
      (_,Id) ^= isName(Op) => some((Lc,Id,A)).
  isSquareApply(_) default => none.
  
  public isTuple:(ast) => option[(locn,list[ast])].
  isTuple(tpl(Lc,"()",A)) => some((Lc,A)).
  isTuple(_) => none.

  public rndTuple:(locn,list[ast]) => ast.
  rndTuple(Lc,Els) => tpl(Lc,"()",Els).

  public isSqTuple:(ast) => option[(locn,list[ast])].
  isSqTuple(tpl(Lc,"[]",A)) => some((Lc,A)).
  isSqTuple(_) => none.

  public sqTuple:(locn,list[ast]) => ast.
  sqTuple(Lc,Els) => tpl(Lc,"[]",Els).

  public isBrTuple:(ast) => option[(locn,list[ast])].
  isBrTuple(tpl(Lc,"{}",A)) => some((Lc,A)).
  isBrTuple(_) => none.

  public brTuple:(locn,list[ast]) => ast.
  brTuple(Lc,Els) => tpl(Lc,"{}",Els).

  public isQBrTuple:(ast) => option[(locn,list[ast])].
  isQBrTuple(tpl(Lc,"{..}",A)) => some((Lc,A)).
  isQBrTuple(_) => none.

  public isBrTerm:(ast) => option[(locn,ast,list[ast])].
  isBrTerm(app(Lc,Op,tpl(_,"{}",A))) => some((Lc,Op,A)).
  isBrTerm(_) default => none.

  public isBrApply:(ast) => option[(locn,string,list[ast])].
  isBrApply(app(Lc,Op,tpl(_,"{}",A))) where
      (_,Id) ^= isName(Op) => some((Lc,Id,A)).
  isBrApply(_) default => none.

  public isRoundTerm:(ast) => option[(locn,ast,list[ast])].
  isRoundTerm(app(Lc,Op,tpl(_,"()",A))) where
      (_,Id) ^= isName(Op) =>
    (keyword(Id) ? none || some((Lc,Op,A))).
  isRoundTerm(_) default => none.

}
