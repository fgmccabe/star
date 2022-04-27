star.compiler.ast{
  import star.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.operators.

  public ast ::=
    nme(option[locn],string)
      | qnm(option[locn],string)
      | int(option[locn],integer)
      | big(option[locn],string)
      | num(option[locn],float)
      | chr(option[locn],char)
      | str(option[locn],string)
      | tpl(option[locn],string,cons[ast])
      | app(option[locn],ast,ast).

  public implementation equality[ast] => let{.
    eq(nme(_,I1),nme(_,I2)) => I1==I2.
    eq(qnm(_,I1),qnm(_,I2)) => I1==I2.
    eq(int(_,L1),int(_,L2)) => L1==L2.
    eq(big(_,I1),big(_,I2)) => I1==I2.
    eq(num(_,L1),num(_,L2)) => L1==L2.
    eq(str(_,L1),str(_,L2)) => L1==L2.
    eq(chr(_,L1),chr(_,L2)) => L1==L2.
    eq(tpl(_,K1,E1),tpl(_,K2,E2)) => K1==K2 && eqList(E1,E2).
    eq(app(_,O1,A1),app(_,O2,A2)) => eq(O1,O2) && eq(A1,A2).
    eq(_,_) default => .false.

    eqList([],[]) => .true.
    eqList([E1,..L1],[E2,..L2]) => eq(E1,E2) && eqList(L1,L2).
    eqList(_,_) default => .false.
  .} in {
    X == Y => eq(X,Y)
  }

  public implementation hasLoc[ast] => {
    locOf(nme(Lc,_)) => Lc.
    locOf(qnm(Lc,_)) => Lc.
    locOf(int(Lc,_)) => Lc.
    locOf(big(Lc,_)) => Lc.
    locOf(num(Lc,_)) => Lc.
    locOf(chr(Lc,_)) => Lc.
    locOf(str(Lc,_)) => Lc.
    locOf(tpl(Lc,_,_)) => Lc.
    locOf(app(Lc,_,_)) => Lc.
  }

  public implementation display[ast] => {
    disp(A) => dispAst(A,2000,"").
  }

  public dispAst:(ast,integer,string) => string.
  dispAst(int(_,Ix),_,_) => disp(Ix).
  dispAst(big(_,Ix),_,_) => disp(Ix).
  dispAst(num(_,Dx),_,_) => disp(Dx).
  dispAst(chr(_,Ch),_,_) => disp(Ch).
  dispAst(str(_,Sx),_,_) => disp(Sx).
  dispAst(nme(_,Id),_,_) => dispId(Id).
  dispAst(qnm(_,Id),_,_) => "'#(stringQuote(Id))'".
  dispAst(tpl(_,"{}",Els),_,Sp) =>
    "{#(interleave(Els//((E)=>dispAst(E,2000,Sp++"  ")),".\n"++Sp)*)}".
  dispAst(tpl(_,"{..}",Els),_,Sp) =>
    "{.#(interleave(Els//((E)=>dispAst(E,2000,Sp++"  ")),".\n"++Sp)*).}".
  dispAst(tpl(_,Bk,Els),_,Sp) where bkt(Lft,_,Rgt,Sep,Inn)^=isBracket(Bk) =>
    "#(Lft)#(interleave(Els//((E)=>dispAst(E,Inn,Sp++"  ")),Sep)*)#(Rgt)".
  dispAst(app(_,nme(_,Op),tpl(_,"()",[L,R])),Pr,Sp) where (Lf,P,Rg)^=isInfixOp(Op)=>
    "#(leftPar(P,Pr))#(dispAst(L,Lf,Sp)) #(Op) #(dispAst(R,Rg,Sp))#(rightPar(P,Pr))".
  dispAst(app(_,nme(_,Op),tpl(_,"()",[R])),Pr,Sp) where (P,Rg)^=isPrefixOp(Op) =>
    "#(leftPar(P,Pr))#(Op) #(dispAst(R,Rg,Sp))#(rightPar(P,Pr))".
  dispAst(app(_,nme(_,Op),tpl(_,"()",[L])),Pr,Sp) where (P,Rg)^=isPostfixOp(Op) =>
    "#(leftPar(P,Pr))#(dispAst(L,Rg,Sp)) #(Op)#(rightPar(P,Pr))".
  dispAst(T,_,_) where isInterpolated(T) => "\"#(deInterpolate(T))\"".
  dispAst(app(_,nme(_,Op),tpl(_,"()",A)),_,Sp) where
      bkt(LB,Op,RB,Sep,Pr) ^= isBracket(Op) =>
    "#(LB) #(interleave(A//((E)=>dispAst(E,2000,Sp++"  ")),Sep)*) #(RB)".
  dispAst(app(_,Op,A),_,Sp) => "$(Op)#(dispAst(A,0,Sp++"  "))".

  dispId:(string) => string.
  dispId(S) where isOperator(S) => "(#(S))".
  dispId(S) => S. -- temporary until we can fix better

  leftPar:(integer,integer) => string.
  leftPar(P,Pr) where P>Pr => "(".
  leftPar(_,_) default => "".

  rightPar:(integer,integer) => string.
  rightPar(P,Pr) where P>Pr => ")".
  rightPar(_,_) default => "".

  isInterpolated(A) where _ ^= isUnary(A,"_str_multicat") => .true.
  isInterpolated(A) where _ ^= isUnary(A,"disp") => .true.
  isInterpolated(A) where _ ^= isBinary(A,"frmt") => .true.
  isInterpolated(A) default => .false.

  deInterpolate:(ast) => string.
  deInterpolate(A) where (_,S) ^= isUnary(A,"_str_multicat") => _str_multicat(deConsPolate(S)).
  deInterpolate(A) where (_,S) ^= isUnary(A,"disp") => dePolate(A).
  deInterpolate(A) where (_,I,F) ^= isBinary(A,"frmt") => dePolate(A).

  deConsPolate(A) where (_,I) ^= isUnary(A,".") && (_,"nil")^=isNme(I) => .nil.
  deConsPolate(A) where (_,L,R) ^= isBinary(A,"cons") =>
    cons(dePolate(L),deConsPolate(R)).

  dePolate(A) where (_,D) ^= isUnary(A,"disp") => "$"++dispAst(D,0,"").
  dePolate(A) where (_,D,str(_,F)) ^= isBinary(A,"frmt") => "$"++dispAst(D,0,"")++":"++F++";".
  dePolate(A) where (_,S) ^= isStr(A) => S.
  dePolate(A) default => "#"++dispAst(A,0,"").
  
  public implementation coercion[ast,string] => {
    _coerce(A) => some("$(A)").
  }

  generated:ref map[string,integer].
  generated = ref {}.

  public genName:(option[locn],string) => ast.
  genName(Lc,Pr) => valof{
    if Last^=generated![Pr] then{
      Nxt .= Last+1;
      generated[Pr] := Nxt;
      valis nme(Lc,"#(Pr)*$(Nxt)")
    } else{
      generated[Pr] := 0;
      valis nme(Lc,"#(Pr)*0")
    }
  }

  public anon(Lc) => nme(Lc,"_").

  public isNme:(ast) => option[(option[locn],string)].
  isNme(nme(Lc,Nm)) => some((Lc,Nm)).
  isNme(qnm(Lc,Nm)) => some((Lc,Nm)).
  isNme(_) default => .none.

  public isInt:(ast) => option[(option[locn],integer)].
  isInt(int(Lc,Ix)) => some((Lc,Ix)).
  isInt(_) default => .none.

  public isFlt:(ast) => option[(option[locn],float)].
  isFlt(num(Lc,Dx)) => some((Lc,Dx)).
  isFlt(_) default => .none.

  public isStr:(ast) => option[(option[locn],string)].
  isStr(str(Lc,Sx)) => some((Lc,Sx)).
  isStr(_) default => .none.

  public isZeroary:(ast,string) => option[locn].
  isZeroary(app(Lc,nme(_,Op),tpl(_,"()",[])),Op) => Lc.
  isZeroary(_,_) default => .none.

  public zeroary:(option[locn],string)=>ast.
  zeroary(Lc,Op) => app(Lc,nme(Lc,Op),tpl(Lc,"()",[])).

  public unary:(option[locn],string,ast) => ast.
  unary(Lc,Op,Arg) => app(Lc,nme(Lc,Op),tpl(locOf(Arg),"()",[Arg])).

  public sqUnary:(option[locn],string,ast) => ast.
  sqUnary(Lc,Op,Arg) => app(Lc,nme(Lc,Op),tpl(locOf(Arg),"[]",[Arg])).

  public isUnary:(ast,string) => option[(option[locn],ast)].
  isUnary(app(Lc,nme(_,Op),tpl(_,"()",[A])),Op) => some((Lc,A)).
  isUnary(_,_) default => .none.

  public binary:(option[locn],string,ast,ast) => ast.
  binary(Lc,Op,L,R) => app(Lc,nme(Lc,Op),tpl(Lc,"()",[L,R])).

  public sqBinary:(option[locn],string,ast,ast) => ast.
  sqBinary(Lc,Op,L,R) => app(Lc,nme(Lc,Op),tpl(Lc,"[]",[L,R])).

  public isBinary:(ast,string) => option[(option[locn],ast,ast)].
  isBinary(app(Lc,nme(_,Op),tpl(_,"()",[L,R])),Op) => some((Lc,L,R)).
  isBinary(_,_) default => .none.

  public ternary:(option[locn],string,ast,ast,ast) => ast.
  ternary(Lc,Op,L,M,R) =>
    app(Lc,nme(Lc,Op),tpl(Lc,"()",[L,M,R])).

  public isTernary:(ast,string) => option[(option[locn],ast,ast,ast)].
  isTernary(app(Lc,nme(_,Op),tpl(_,"()",[L,M,R])),Op) => some((Lc,L,M,R)).
  isTernary(_,_) default => .none.

  public isTuple:(ast) => option[(option[locn],cons[ast])].
  isTuple(tpl(Lc,"()",A)) => some((Lc,A)).
  isTuple(_) => .none.

  public rndTuple:(option[locn],cons[ast]) => ast.
  rndTuple(Lc,Els) => tpl(Lc,"()",Els).

  public unit(Lc) => rndTuple(Lc,[]).

  public isSqTuple:(ast) => option[(option[locn],cons[ast])].
  isSqTuple(tpl(Lc,"[]",A)) => some((Lc,A)).
  isSqTuple(_) => .none.

  public sqTuple:(option[locn],cons[ast]) => ast.
  sqTuple(Lc,Els) => tpl(Lc,"[]",Els).

  public isBrTuple:(ast) => option[(option[locn],cons[ast])].
  isBrTuple(tpl(Lc,"{}",A)) => some((Lc,A)).
  isBrTuple(_) => .none.

  public brTuple:(option[locn],cons[ast]) => ast.
  brTuple(Lc,Els) => tpl(Lc,"{}",Els).

  public isQBrTuple:(ast) => option[(option[locn],cons[ast])].
  isQBrTuple(tpl(Lc,"{..}",A)) => some((Lc,A)).
  isQBrTuple(_) => .none.

  public qbrTuple(Lc,Els) => tpl(Lc,"{..}",Els).

  public isQuoted(tpl(Lc,"<||>",[E])) => some((Lc,E)).
  isQuoted(E) => isUnary(E,"<||>").
}
