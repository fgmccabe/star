star.compiler.ast{
  import star.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.operators.

  public ast ::= .nme(option[locn],string)
    | .qnm(option[locn],string)
    | .int(option[locn],integer)
    | .big(option[locn],bigint)
    | .num(option[locn],float)
    | .chr(option[locn],char)
    | .str(option[locn],string)
    | .tpl(option[locn],string,cons[ast])
    | .app(option[locn],ast,ast).

  public implementation equality[ast] => let{.
    eq(A1,A2) => case A1 in {
      .nme(_,I1) => .nme(_,I2).=A2 && I1==I2.
      .qnm(_,I1) => .qnm(_,I2).=A2 && I1==I2.
      .int(_,L1) => .int(_,L2).=A2 && L1==L2.
      .big(_,I1) => .big(_,I2).=A2 && I1==I2.
      .num(_,L1) => .num(_,L2).=A2 && L1==L2.
      .str(_,L1) => .str(_,L2).=A2 && L1==L2.
      .chr(_,L1) => .chr(_,L2).=A2 && L1==L2.
      .tpl(_,K1,E1) => .tpl(_,K2,E2).=A2 && K1==K2 && eqList(E1,E2).
      .app(_,O1,As1) => .app(_,O2,As2).=A2 && eq(O1,O2) && eq(As1,As2).
    }

    eqList([],[]) => .true.
    eqList([E1,..L1],[E2,..L2]) => eq(E1,E2) && eqList(L1,L2).
    eqList(_,_) default => .false.
  .} in {
    X == Y => eq(X,Y)
  }

  public implementation hasLoc[ast] => {
    locOf(A) => case A in {
      .nme(Lc,_) => Lc.
      .qnm(Lc,_) => Lc.
      .int(Lc,_) => Lc.
      .big(Lc,_) => Lc.
      .num(Lc,_) => Lc.
      .chr(Lc,_) => Lc.
      .str(Lc,_) => Lc.
      .tpl(Lc,_,_) => Lc.
      .app(Lc,_,_) => Lc.
    }
  }

  public implementation display[ast] => {
    disp(A) => dispAst(A,2000,"").
  }

  public dispAst:(ast,integer,string) => string.
  dispAst(As,Pr,Sp) => case As in {
    .int(_,Ix) => disp(Ix).
    .big(_,Ix) => disp(Ix).
    .num(_,Dx) => disp(Dx).
    .chr(_,Ch) => disp(Ch).
    .str(_,Sx) => disp(Sx).
    .nme(_,Id) => dispId(Id).
    .qnm(_,Id) => "'#(stringQuote(Id))'".
    .tpl(_,"{}",Els) =>
      "{#(interleave(Els//((E)=>dispAst(E,2000,Sp++"  ")),".\n"++Sp)*)}".
    .tpl(_,"{..}",Els) =>
      "{.#(interleave(Els//((E)=>dispAst(E,2000,Sp++"  ")),".\n"++Sp)*).}".
    .tpl(_,Bk,Els) where .bkt(Lft,_,Rgt,Sep,Inn)?=isBracket(Bk) =>
      "#(Lft)#(interleave(Els//((E)=>dispAst(E,Inn,Sp++"  ")),Sep)*)#(Rgt)".
    .app(_,.nme(_,Op),.tpl(_,"()",[L,R])) where (Lf,P,Rg)?=isInfixOp(Op)=>
      "#(leftPar(P,Pr))#(dispAst(L,Lf,Sp)) #(Op) #(dispAst(R,Rg,Sp))#(rightPar(P,Pr))".
    .app(_,.nme(_,Op),.tpl(_,"()",[R])) where (P,Rg)?=isPrefixOp(Op) =>
      "#(leftPar(P,Pr))#(Op) #(dispAst(R,Rg,Sp))#(rightPar(P,Pr))".
    .app(_,.nme(_,Op),.tpl(_,"()",[L])) where (P,Rg)?=isPostfixOp(Op) =>
      "#(leftPar(P,Pr))#(dispAst(L,Rg,Sp)) #(Op)#(rightPar(P,Pr))".
    T where isInterpolated(T) => "\"#(deInterpolate(T))\"".
    .app(_,.nme(_,Op),.tpl(_,"()",A)) where
	.bkt(LB,Op,RB,Sep,Pr) ?= isBracket(Op) =>
      "#(LB) #(interleave(A//((E)=>dispAst(E,2000,Sp++"  ")),Sep)*) #(RB)".
    .app(_,Op,A) => "$(Op)#(dispAst(A,0,Sp++"  "))".
  }

  dispId:(string) => string.
  dispId(S) where isOperator(S) => "(#(S))".
  dispId(S) => S. -- temporary until we can fix better

  leftPar:(integer,integer) => string.
  leftPar(P,Pr) where P>Pr => "(".
  leftPar(_,_) default => "".

  rightPar:(integer,integer) => string.
  rightPar(P,Pr) where P>Pr => ")".
  rightPar(_,_) default => "".

  isInterpolated(A) where (_,I) ?= isUnary(A,"_str_multicat") => isDispCons(I).
  isInterpolated(A) where (_,I) ?= isUnary(A,"disp") => isDisp(I).
  isInterpolated(A) where (_,I,_) ?= isBinary(A,"frmt") => isDisp(I).
  isInterpolated(A) default => .false.

  deInterpolate:(ast) => string.
  deInterpolate(A) where (_,S) ?= isUnary(A,"_str_multicat") =>
    _str_multicat(deConsPolate(S)).
  deInterpolate(A) where (_,S) ?= isUnary(A,"disp") => dePolate(A).
  deInterpolate(A) where (_,I,F) ?= isBinary(A,"frmt") => dePolate(A).

  deConsPolate(A) where (_,I) ?= isUnary(A,".") && (_,"nil")?=isNme(I) => .nil.
  deConsPolate(A) where (_,L,R) ?= isBinary(A,"cons") =>
    cons(dePolate(L),deConsPolate(R)).

  dePolate(A) where (_,D) ?= isUnary(A,"disp") => "$"++dispAst(D,0,"").
  dePolate(A) where (_,D,.str(_,F)) ?= isBinary(A,"frmt") => "$"++dispAst(D,0,"")++":"++F++";".
  dePolate(A) where (_,S) ?= isStr(A) => S.
  dePolate(A) default => "#"++dispAst(A,0,"").

  isDispCons(A) where (_,I) ?= isUnary(A,".") && (_,"nil")?=isNme(I) => .true.
  isDispCons(A) where (_,L,R) ?= isBinary(A,"cons") => isDisp(L) && isDispCons(R).
  isDispCons(_) default => .false.

  isDisp(A) where (_,S) ?= isUnary(A,"_str_multicat") => isDispCons(S).
  isDisp(A) where (_,S) ?= isUnary(A,"disp") => .true.
  isDisp(A) where (_,I,F) ?= isBinary(A,"frmt") => .true.
  isDisp(_) default => .false.

  public implementation coercion[ast,string] => {
    _coerce(A) => .some("$(A)").
  }

  generated:ref map[string,integer].
  generated = ref {}.

  public genName:(option[locn],string) => ast.
  genName(Lc,Pr) => valof{
    if Last?=generated![Pr] then{
      Nxt = Last+1;
      generated[Pr] := Nxt;
      valis nme(Lc,"#(Pr)*$(Nxt)")
    } else{
      generated[Pr] := 0;
      valis nme(Lc,"#(Pr)*0")
    }
  }
  
  public genId:(string) => string.
  genId(Pr) => valof{
    if Last?=generated![Pr] then{
      Nxt = Last+1;
      generated[Pr] := Nxt;
      valis "#(Pr)*$(Nxt)"
    } else{
      generated[Pr] := 0;
      valis "#(Pr)*0"
    }
  }

  public mkAnon(Lc) => nme(Lc,"_").

  public isAnon(.nme(Lc,"_")) => some(Lc).
  isAnon(_) default => .none.

  public isNme:(ast) => option[(option[locn],string)].
  isNme(.nme(Lc,Nm)) => .some((Lc,Nm)).
  isNme(.qnm(Lc,Nm)) => .some((Lc,Nm)).
  isNme(_) default => .none.

  public isInt:(ast) => option[(option[locn],integer)].
  isInt(.int(Lc,Ix)) => .some((Lc,Ix)).
  isInt(_) default => .none.

  public isBig:(ast) => option[(option[locn],bigint)].
  isBig(.big(Lc,Ix)) => .some((Lc,Ix)).
  isBig(_) default => .none.

  public isFlt:(ast) => option[(option[locn],float)].
  isFlt(.num(Lc,Dx)) => .some((Lc,Dx)).
  isFlt(_) default => .none.

  public isChr:(ast) => option[(option[locn],char)].
  isChr(.chr(Lc,Cx)) => .some((Lc,Cx)).
  isChr(_) default => .none.

  public isStr:(ast) => option[(option[locn],string)].
  isStr(.str(Lc,Sx)) => .some((Lc,Sx)).
  isStr(_) default => .none.

  public isZeroary:(ast,string) => option[locn].
  isZeroary(.app(Lc,.nme(_,Op),.tpl(_,"()",[])),Op) => Lc.
  isZeroary(_,_) default => .none.

  public zeroary:(option[locn],string)=>ast.
  zeroary(Lc,Op) => .app(Lc,.nme(Lc,Op),.tpl(Lc,"()",[])).

  public unary:(option[locn],string,ast) => ast.
  unary(Lc,Op,Arg) => .app(Lc,.nme(Lc,Op),.tpl(locOf(Arg),"()",[Arg])).

  public sqUnary:(option[locn],string,ast) => ast.
  sqUnary(Lc,Op,Arg) => .app(Lc,.nme(Lc,Op),.tpl(locOf(Arg),"[]",[Arg])).

  public isUnary:(ast,string) => option[(option[locn],ast)].
  isUnary(.app(Lc,.nme(_,Op),.tpl(_,"()",[A])),Op) => some((Lc,A)).
  isUnary(_,_) default => .none.

  public binary:(option[locn],string,ast,ast) => ast.
  binary(Lc,Op,L,R) => .app(Lc,.nme(Lc,Op),.tpl(Lc,"()",[L,R])).

  public sqBinary:(option[locn],string,ast,ast) => ast.
  sqBinary(Lc,Op,L,R) => .app(Lc,.nme(Lc,Op),.tpl(Lc,"[]",[L,R])).

  public isBinary:(ast,string) => option[(option[locn],ast,ast)].
  isBinary(.app(Lc,.nme(_,Op),.tpl(_,"()",[L,R])),Op) => .some((Lc,L,R)).
  isBinary(_,_) default => .none.

  public ternary:(option[locn],string,ast,ast,ast) => ast.
  ternary(Lc,Op,L,M,R) =>
    .app(Lc,.nme(Lc,Op),.tpl(Lc,"()",[L,M,R])).

  public isTernary:(ast,string) => option[(option[locn],ast,ast,ast)].
  isTernary(.app(Lc,.nme(_,Op),.tpl(_,"()",[L,M,R])),Op) => .some((Lc,L,M,R)).
  isTernary(_,_) default => .none.

  public isTuple:(ast) => option[(option[locn],cons[ast])].
  isTuple(.tpl(Lc,"()",A)) => .some((Lc,A)).
  isTuple(_) => .none.

  public rndTuple:(option[locn],cons[ast]) => ast.
  rndTuple(Lc,Els) => .tpl(Lc,"()",Els).

  public unit(Lc) => rndTuple(Lc,[]).

  public isSqTuple:(ast) => option[(option[locn],cons[ast])].
  isSqTuple(.tpl(Lc,"[]",A)) => .some((Lc,A)).
  isSqTuple(_) => .none.

  public sqTuple:(option[locn],cons[ast]) => ast.
  sqTuple(Lc,Els) => .tpl(Lc,"[]",Els).

  public isBrTuple:(ast) => option[(option[locn],cons[ast])].
  isBrTuple(.tpl(Lc,"{}",A)) => .some((Lc,A)).
  isBrTuple(_) => .none.

  public brTuple:(option[locn],cons[ast]) => ast.
  brTuple(Lc,Els) => .tpl(Lc,"{}",Els).

  public isQBrTuple:(ast) => option[(option[locn],cons[ast])].
  isQBrTuple(.tpl(Lc,"{..}",A)) => .some((Lc,A)).
  isQBrTuple(_) => .none.

  public qbrTuple(Lc,Els) => .tpl(Lc,"{..}",Els).

  public isQuoted(.tpl(Lc,"<||>",[E])) => .some((Lc,E)).
  isQuoted(E) => isUnary(E,"<||>").
}
