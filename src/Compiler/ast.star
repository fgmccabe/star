star.compiler.ast{
  import star.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.operators.

  public ast ::=
    nme(locn,string)
      | qnm(locn,string)
      | int(locn,integer)
      | num(locn,float)
      | str(locn,string)
      | tpl(locn,string,cons[ast])
      | app(locn,ast,ast).

  public implementation equality[ast] => {.
    nme(_,I1) == nme(_,I2) => I1==I2.
    qnm(_,I1) == qnm(_,I2) => I1==I2.
    int(_,L1) == int(_,L2) => L1==L2.
    num(_,L1) == num(_,L2) => L1==L2.
    str(_,L1) == str(_,L2) => L1==L2.
    tpl(_,K1,E1) == tpl(_,K2,E2) => K1==K2 && E1==E2.
    app(_,O1,A1) == app(_,O2,A2) => O1==O2 && A1==A2.
    _ == _ default => .false.
  .}

  public implementation display[ast] => {.
    disp(A) => dispAst(A,2000,"").
  .}

  public implementation coercion[ast,string] => {.
    _coerce(A) => "$(A)".
  .}

  public dispAst:(ast,integer,string) => ss.
  dispAst(int(_,Ix),_,_) => disp(Ix).
  dispAst(num(_,Dx),_,_) => disp(Dx).
  dispAst(str(_,Sx),_,_) => disp(Sx).
  dispAst(nme(_,Id),_,_) => dispId(Id).
  dispAst(qnm(_,Id),_,_) => ssSeq([ss("'"),ss(Id),ss("'")]).
  dispAst(tpl(_,"{}",Els),_,Sp) =>
    ssSeq([ss("{\n"),ss(Sp),ssSeq(interleave(Els//((E)=>dispAst(E,2000,Sp++"  ")),ss(".\n"++Sp))),ss("\n"++Sp),ss("}")]).
  dispAst(tpl(_,"()",[nme(_,Id)]),_,_) => ssSeq([ss("("),ss(Id),ss(")")]).
  dispAst(tpl(_,Bk,Els),_,Sp) where bkt(Lft,_,Rgt,Inn)^=isBracket(Bk) =>
    ssSeq([ss(Lft),ssSeq(interleave(Els//((E)=>dispAst(E,Inn,Sp)),ss(","))),ss(Rgt)]).
  dispAst(app(_,nme(_,Op),tpl(_,"()",[L,R])),Pr,Sp) where (Lf,P,Rg)^=isInfixOp(Op)=>
    ssSeq([leftPar(P,Pr),dispAst(L,Lf,Sp),ss(" "),ss(Op),ss(" "),dispAst(R,Rg,Sp),rightPar(P,Pr)]).
  dispAst(app(_,nme(_,Op),tpl(_,"()",[R])),Pr,Sp) where (P,Rg)^=isPrefixOp(Op) =>
    ssSeq([leftPar(P,Pr),ss(Op),ss(" "),dispAst(R,Rg,Sp),rightPar(P,Pr)]).
  dispAst(app(_,nme(_,Op),tpl(_,"()",[R])),Pr,Sp) where (P,Rg)^=isPostfixOp(Op) =>
    ssSeq([leftPar(P,Pr),dispAst(R,Rg,Sp),ss(" "),ss(Op),rightPar(P,Pr)]).
  dispAst(app(_,Op,A),_,Sp) => ssSeq([dispAst(Op,0,Sp),dispAst(A,0,Sp++"  ")]).

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
    locOf(int(Lc,_)) => Lc.
    locOf(num(Lc,_)) => Lc.
    locOf(str(Lc,_)) => Lc.
    locOf(tpl(Lc,_,_)) => Lc.
    locOf(app(Lc,_,_)) => Lc.
  .}

  public isName:(ast) => option[(locn,string)].
  isName(nme(Lc,Id)) where ! keyword(Id) => some((Lc,Id)).
  isName(tpl(_,"()",[nme(Lc,Id)])) => some((Lc,Id)).
  isName(_) default => .none.

  public isKeyword:(ast) => option[(locn,string)].
  isKeyword(nme(Lc,Id)) where keyword(Id) => some((Lc,Id)).
  isKeyword(_) default => .none.

  public genName:(locn,string) => ast.
  genName(Lc,Pr) => nme(Lc,genSym(Pr)).
  
  public isInt:(ast) => option[(locn,integer)].
  isInt(int(Lc,Ix)) => some((Lc,Ix)).
  isInt(_) default => .none.

  public isFlt:(ast) => option[(locn,float)].
  isFlt(num(Lc,Dx)) => some((Lc,Dx)).
  isFlt(_) default => .none.

  public isStr:(ast) => option[(locn,string)].
  isStr(str(Lc,Sx)) => some((Lc,Sx)).
  isStr(_) default => .none.

  public zeroary:(locn,string)=>ast.
  zeroary(Lc,Op) => app(Lc,nme(Lc,Op),tpl(Lc,"()",[])).

  public unary:(locn,string,ast) => ast.
  unary(Lc,Op,Arg) => app(Lc,nme(Lc,Op),tpl(locOf(Arg),"()",[Arg])).

  public isUnary:(ast,string) => option[(locn,ast)].
  isUnary(app(Lc,nme(_,Op),tpl(_,"()",[A])),Op) => some((Lc,A)).
  isUnary(_,_) default => .none.

  public binary:(locn,string,ast,ast) => ast.
  binary(Lc,Op,L,R) => app(Lc,nme(Lc,Op),tpl(Lc,"()",[L,R])).

  public isBinary:(ast,string) => option[(locn,ast,ast)].
  isBinary(app(Lc,nme(_,Op),tpl(_,"()",[L,R])),Op) => some((Lc,L,R)).
  isBinary(_,_) default => .none.

  public ternary:(locn,string,ast,ast,ast) => ast.
  ternary(Lc,Op,L,M,R) =>
    app(Lc,nme(Lc,Op),tpl(Lc,"()",[L,M,R])).

  public isTernary:(ast,string) => option[(locn,ast,ast,ast)].
  isTernary(app(Lc,nme(_,Op),tpl(_,"()",[L,M,R])),Op) => some((Lc,L,M,R)).
  isTernary(_,_) default => .none.

  public isSquareTerm:(ast) => option[(locn,ast,cons[ast])].
  isSquareTerm(app(Lc,Op,tpl(_,"[]",A))) => some((Lc,Op,A)).
  isSquareTerm(_) default => .none.

  public isSquareApply:(ast) => option[(locn,string,cons[ast])].
  isSquareApply(app(Lc,Op,tpl(_,"[]",A))) where
      (_,Id) ^= isName(Op) => some((Lc,Id,A)).
  isSquareApply(_) default => .none.

  public squareTerm:(locn,ast,cons[ast])=>ast.
  squareTerm(Lc,Op,Args) => app(Lc,Op,tpl(Lc,"[]",Args)).
  
  public isTuple:(ast) => option[(locn,cons[ast])].
  isTuple(tpl(Lc,"()",A)) => some((Lc,A)).
  isTuple(_) => .none.

  public rndTuple:(locn,cons[ast]) => ast.
  rndTuple(Lc,Els) => tpl(Lc,"()",Els).

  public isSqTuple:(ast) => option[(locn,cons[ast])].
  isSqTuple(tpl(Lc,"[]",A)) => some((Lc,A)).
  isSqTuple(_) => .none.

  public sqTuple:(locn,cons[ast]) => ast.
  sqTuple(Lc,Els) => tpl(Lc,"[]",Els).

  public isBrTuple:(ast) => option[(locn,cons[ast])].
  isBrTuple(tpl(Lc,"{}",A)) => some((Lc,A)).
  isBrTuple(_) => .none.

  public brTuple:(locn,cons[ast]) => ast.
  brTuple(Lc,Els) => tpl(Lc,"{}",Els).

  public isQBrTuple:(ast) => option[(locn,cons[ast])].
  isQBrTuple(tpl(Lc,"{..}",A)) => some((Lc,A)).
  isQBrTuple(_) => .none.

  public isBrTerm:(ast) => option[(locn,ast,cons[ast])].
  isBrTerm(app(Lc,Op,tpl(_,"{}",A))) => some((Lc,Op,A)).
  isBrTerm(_) default => .none.

  public isBrApply:(ast) => option[(locn,string,cons[ast])].
  isBrApply(app(Lc,Op,tpl(_,"{}",A))) where
      (_,Id) ^= isName(Op) => some((Lc,Id,A)).
  isBrApply(_) default => .none.

  public isQBrTerm:(ast) => option[(locn,ast,cons[ast])].
  isQBrTerm(app(Lc,Op,tpl(_,"{..}",A))) => some((Lc,Op,A)).
  isQBrTerm(_) default => .none.

  public isQBrApply:(ast) => option[(locn,string,cons[ast])].
  isQBrApply(app(Lc,Op,tpl(_,"{..}",A))) where
      (_,Id) ^= isName(Op) => some((Lc,Id,A)).
  isQBrApply(_) default => .none.

  public isRoundTerm:(ast) => option[(locn,ast,cons[ast])].
  isRoundTerm(app(Lc,Op,tpl(_,"()",A))) where !_^=isKeyword(Op) => some((Lc,Op,A)).
  isRoundTerm(_) default => .none.

  public roundTerm:(locn,ast,cons[ast]) => ast.
  roundTerm(Lc,Op,Els) => app(Lc,Op,tpl(Lc,"()",Els)).

  public braceTerm:(locn,ast,cons[ast]) => ast.
  braceTerm(Lc,Op,Els) => app(Lc,Op,tpl(Lc,"{}",Els)).

  public implementation coercion[locn,ast]=>{
    _coerce(Lc where locn(Pkg,Line,Col,Off,Ln).=Lc)=>
      roundTerm(Lc,nme(Lc,"locn"),[str(Lc,Pkg),
	  int(Lc,Line), int(Lc,Col), int(Lc,Off), int(Lc,Ln)]).
  }

}
