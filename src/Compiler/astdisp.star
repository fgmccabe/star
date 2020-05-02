star.compiler.ast.display{
  import star.
  import star.compiler.ast.
  import star.compiler.location.
  import star.compiler.operators.

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
  dispAst(app(_,nme(_,Op),tpl(_,"()",[L,R])),Pr,Sp) where (Lf,P,Rg,_)^=isInfixOp(Op)=>
    ssSeq([leftPar(P,Pr),dispAst(L,Lf,Sp),ss(" "),ss(Op),ss(" "),dispAst(R,Rg,Sp),rightPar(P,Pr)]).
  dispAst(app(_,nme(_,Op),tpl(_,"()",[R])),Pr,Sp) where (P,Rg,_)^=isPrefixOp(Op) =>
    ssSeq([leftPar(P,Pr),ss(Op),ss(" "),dispAst(R,Rg,Sp),rightPar(P,Pr)]).
  dispAst(app(_,nme(_,Op),tpl(_,"()",[R])),Pr,Sp) where (P,Rg,_)^=isPostfixOp(Op) =>
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
}
