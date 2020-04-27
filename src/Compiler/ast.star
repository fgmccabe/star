star.compiler.ast{
  import star.
  import star.compiler.location.
  import star.compiler.misc.

  public ast ::=
    nme(locn,string)
      | qnm(locn,string)
      | int(locn,integer)
      | num(locn,float)
      | str(locn,string)
      | tpl(locn,string,cons[ast])
      | app(locn,ast,ast)
      | astImplies(locn,ast,ast).

  public implementation equality[ast] => let{
    eq(nme(_,I1),nme(_,I2)) => I1==I2.
    eq(qnm(_,I1),qnm(_,I2)) => I1==I2.
    eq(int(_,L1),int(_,L2)) => L1==L2.
    eq(num(_,L1),num(_,L2)) => L1==L2.
    eq(str(_,L1),str(_,L2)) => L1==L2.
    eq(tpl(_,K1,E1),tpl(_,K2,E2)) => K1==K2 && eqList(E1,E2).
    eq(app(_,O1,A1),app(_,O2,A2)) => eq(O1,O2) && eq(A1,A2).
    eq(astImplies(_,O1,A1),astImplies(_,O2,A2)) => eq(O1,O2) && eq(A1,A2).
    eq(_,_) default => .false.

    eqList([],[]) => .true.
    eqList([E1,..L1],[E2,..L2]) => eq(E1,E2) && eqList(L1,L2).
    eqList(_,_) default => .false.
  } in {.
    X == Y => eq(X,Y)
  .}

  public implementation hasLoc[ast] => {.
    locOf(nme(Lc,_)) => Lc.
    locOf(qnm(Lc,_)) => Lc.
    locOf(int(Lc,_)) => Lc.
    locOf(num(Lc,_)) => Lc.
    locOf(str(Lc,_)) => Lc.
    locOf(tpl(Lc,_,_)) => Lc.
    locOf(app(Lc,_,_)) => Lc.
  .}

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

}
