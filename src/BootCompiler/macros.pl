:- module(macros,[macroPkg/2]).

:- use_module(abstract).
:- use_module(grammar).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(astdisp).
:- use_module(macroRules).

macroAst(A,Cx,Examine,Rp) :-
  applyRls(A,Cx,Rs),!,
  testReslt(Rs,A,Cx,Examine,Rp).

testReslt(active(A),_,Cx,Examine,Rep) :-!,
  macroAst(A,Cx,Examine,Rep).
testReslt(inactive,A,_,Examine,Rep) :-
  call(Examine,A,Rep).

applyRls(A,Cx,active(Rs)) :-
  one_of(macros:macroKey(A,Ky)),
  macroRl(Ky,Cx,Rl),
  call(Rl,A,Cx,Rs),!.
applyRls(_,_,inactive).

macroPkg(P,Rp) :- macroAst(P,package,macros:examinePkg,Rp).

examinePkg(A,Rp) :-
  isBraceTerm(A,Lc,Nm,Els),!,
  build_main(Els,Els0),
  makeGrammar(Els0,Sts),
  map(Sts,macros:macroStmt,Elx),
  braceTerm(Lc,Nm,Elx,Rp).

macroStmt(S,Rp) :- macroAst(S,statement,macros:examineStmt,Rp).

examineStmt(S,Rp) :-
  isTypeAnnotation(S,Lc,V,T),!,
  macroType(T,Tx),
  typeAnnotation(Lc,V,Tx,Rp).
examineStmt(S,Rp) :-
  isPublic(S,Lc,I),!,
  macroStmt(I,II),
  mkPublic(Lc,II,Rp).
examineStmt(S,Rp) :-
  isPrivate(S,Lc,I),!,
  macroStmt(I,II),
  mkPrivate(Lc,II,Rp).
examineStmt(S,S) :-
  isImport(S,_Lc,_I),!.
examineStmt(S,Sx) :-
  isOpen(S,Lc,E),!,
  macroTerm(E,Ex),
  mkOpen(Lc,Ex,Sx).
examineStmt(S,Rp) :-
  isDefn(S,Lc,P,V),!,
  macroPtn(P,PP),
  macroTerm(V,VV),
  mkDefn(Lc,PP,VV,Rp).
examineStmt(S,Rp) :-
  isEquation(S,Lc,P,G,V),!,
  macroHead(P,PP),
  macroOpt(G,macros:macroTerm,GG),
  macroTerm(V,VV),
  mkEquation(Lc,PP,GG,VV,Rp).
examineStmt(S,Rp) :-
  isTypeFunStmt(S,Lc,Q,C,L,R),!,
  macroType(L,Lx),
  macroType(R,Rx),
  map(C,macros:macroType,Cx),
  typeFunStmt(Lc,Q,Cx,Lx,Rx,Rp).
examineStmt(S,Rp) :-
  isContractStmt(S,Lc,Q,C,T,B),!,
  macroConstraint(T,Tx),
  map(B,macros:macroStmt,Bx),
  map(C,macros:macroConstraint,Cx),
  contractStmt(Lc,Q,Cx,Tx,Bx,Rp).
examineStmt(S,Rp) :-
  isImplementationStmt(S,Lc,Q,C,T,B),!,
  macroConstraint(T,Tx),
  map(C,macros:macroConstraint,Cx),
  macroTerm(B,Bx),
  implementationStmt(Lc,Q,Cx,Tx,Bx,Rp).
examineStmt(S,Sx) :-
  isAlgebraicTypeStmt(S,Lc,Q,C,H,B),!,
  macroType(H,Hx),
  map(C,macros:macroType,Cx),
  macroAlgebraic(B,Bx),
  mkAlgebraicTypeStmt(Lc,Q,Cx,Hx,Bx,Sx).
examineStmt(S,Sx) :-
  isStructTypeStmt(S,Lc,Q,X,C,H,Nm,Els),!,
  macroType(H,Hx),
  map(C,macros:macroType,Cx),
  map(Els,macros:braceTypeMacro,Elx),
  mkStructTypeStmt(Lc,Q,X,Cx,Hx,Nm,Elx,Sx).
%  dispAst(Sx).
examineStmt(S,S) :-
  isAnnotation(S,_,_,_),!.
examineStmt(S,S) :-
  locOfAst(S,Lc),
  reportError("cannot figure out statement %s",[ast(S)],Lc).

macroAlgebraic(B,Bx) :-
  isBinary(B,Lc,"|",L,R),!,
  macroAlgebraic(L,Lx),
  macroAlgebraic(R,Rx),
  binary(Lc,"|",Lx,Rx,Bx).
macroAlgebraic(B,Bx) :-
  isUnary(B,Lc,"|",R),!,
  macroAlgebraic(R,Rx),
  unary(Lc,"|",Rx,Bx).
macroAlgebraic(B,B) :-
  isEnum(B,_,N),isIden(N,_,_),!.
macroAlgebraic(B,Bx) :-
  isConApply(B,Lc,O,E),!,
  map(E,macros:macroType,Ex),
  mkConApply(Lc,O,Ex,Bx).
macroAlgebraic(B,Bx) :-
  isBraceTerm(B,Lc,O,E),!,
  map(E,macros:braceTypeMacro,Ex),
  braceTerm(Lc,O,Ex,Bx).
macroAlgebraic(B,Bx) :-
  isXQuantified(B,Q,I),
  macroAlgebraic(I,Ix),
  reXQuant(Q,Ix,Bx).
macroAlgebraic(B,B) :-
  locOfAst(B,Lc),
  reportError("%s is an invalid constructor in type definition",[ast(B)],Lc).

macroType(T,Tx) :-
  macroAst(T,type,macros:examineType,Tx).

examineType(T,T) :- isIden(T,_,_),!.
examineType(T,Tx) :- isSquareTerm(T,Lc,Op,Args),!,
  map(Args,macros:macroType,Argx),
  macroType(Op,Opx),
  squareTerm(Lc,Opx,Argx,Tx).
examineType(T,Tx) :- isConstructorType(T,Lc,U,X,L,R),!,
  map(U,macros:macroType,Ux),
  map(X,macros:macroType,Xx),
  macroType(L,Lx),
  macroType(R,Rx),
  constructorType(Lc,Ux,Xx,Lx,Rx,Tx).
examineType(T,Tx) :- isFuncType(T,Lc,L,R),!,
  macroType(L,Lx),
  macroType(R,Rx),
  funcType(Lc,Lx,Rx,Tx).
examineType(T,Tx) :- isTaskType(T,Lc,L),!,
  macroType(L,Lx),
  mkTaskType(Lc,Lx,Tx).
examineType(T,Tx) :- isResult(T,Lc,L,R),!,
  macroType(L,Lx),
  macroType(R,Rx),
  mkResult(Lc,Lx,Rx,Tx).
examineType(T,Tx) :- isRoundTuple(T,Lc,Els),!,
  map(Els,macros:macroType,Elx),
  roundTuple(Lc,Elx,Tx).
examineType(T,Tx) :- isGeneratorType(T,Lc,Y),!,
  macroType(Y,Yx),
  mkGeneratorType(Lc,Yx,Tx).
examineType(T,Tx) :-
  isQuantified(T,V,I),!,
  map(V,macros:macroTypeVar,Vx),
  macroType(I,Ix),
  reUQuant(Vx,Ix,Tx).
examineType(T,Tx) :-
  isXQuantified(T,V,I),!,
  map(V,macros:macroTypeVar,Vx),
  macroType(I,Ix),
  reXQuant(Vx,Ix,Tx).
examineType(T,Tx) :-
  isConstrained(T,I,C),!,
  map(C,macros:macroConstraint,Cx),
  macroType(I,Ix),
  reConstrain(Cx,Ix,Tx).
examineType(T,Tx) :-
  isTypeExists(T,Lc,L,R),!,
  macroType(L,Lx),
  macroType(R,Rx),
  typeExists(Lc,Lx,Rx,Tx).
examineType(T,Tx) :-
  isTypeLambda(T,Lc,L,R),!,
  macroType(L,Lx),
  macroType(R,Rx),
  typeLambda(Lc,Lx,Rx,Tx).
examineType(T,Tx) :-
  isComma(T,Lc,L,R),!,
  macroType(L,Lx),
  macroType(R,Rx),
  comma(Lc,Lx,Rx,Tx).
examineType(T,Tx) :-
  isRef(T,Lc,L),!,
  macroType(L,Lx),
  mkRef(Lc,Lx,Tx).
examineType(T,Tx) :-
  isRoundTuple(T,Lc,Els),!,
  map(Els,macros:macroType,Elx),
  roundTuple(Lc,Elx,Tx).
examineType(T,Tx) :-
  isBraceTuple(T,Lc,Els),!,
  map(Els,macros:braceTypeMacro,Elx),
  braceTuple(Lc,Elx,Tx).
examineType(T,Tx) :-
  isFieldAcc(T,Lc,L,F),!,
  macroTerm(L,Lx),
  fieldAcc(Lc,Lx,name(Lc,F),Tx).
examineType(T,T) :-
  locOfAst(T,Lc),
  reportError("cannot figure out type %s",[ast(T)],Lc).

macroTypeVar(T,Tx) :-
  macroAst(T,type,macros:examineTypeVar,Tx).

examineTypeVar(T,T) :-
  isIden(T,_,_),!.
examineTypeVar(T,T) :-
  isBinary(T,_,"/",_,_),!.
examineTypeVar(T,T) :-
  locOfAst(T,Lc),
  reportError("cannot figure out type variable %s",[ast(T)],Lc).

braceTypeMacro(S,Sx) :-
  isTypeAnnotation(S,Lc,L,R),!,
  macroType(R,Rx),
  typeAnnotation(Lc,L,Rx,Sx).
braceTypeMacro(S,Sx) :-
  isTypeExists(S,Lc,L,R),
  macroType(R,Rx),
  mkTypeExists(Lc,L,Rx,Sx).

macroConstraint(T,Tx) :-
  macroAst(T,constraint,macros:examineConstraint,Tx).

examineConstraint(T,Tx) :- isSquareTerm(T,Lc,Op,Args),!,
  map(Args,macros:macroConstraintArg,Argx),
  macroType(Op,Opx),
  squareTerm(Lc,Opx,Argx,Tx).
examineConstraint(T,Tx) :-
  isQuantified(T,V,I),!,
  map(V,macros:macroConstraint,Vx),
  macroType(I,Ix),
  reUQuant(Vx,Ix,Tx).
examineConstraint(T,Tx) :-
  isXQuantified(T,V,I),!,
  map(V,macros:macroConstraint,Vx),
  macroType(I,Ix),
  reXQuant(Vx,Ix,Tx).
examineConstraint(T,Tx) :-
  isDynamic(T,Lc,Nm,Tp),
  macroType(Tp,TTp),
  mkDynamic(Lc,Nm,TTp,Tx).
examineConstraint(T,Tx) :-
  isRaises(T,Lc,E),
  macroType(E,Ex),
  mkRaises(Lc,Ex,Tx).
examineConstraint(T,Tx) :-
  isTypeExists(T,Lc,L,R),!,
  macroType(L,Lx),
  macroType(R,Rx),
  typeExists(Lc,Lx,Rx,Tx).
examineConstraint(T,Tx) :-
  isRoundTuple(T,Lc,[El]),!,
  macroConstraint(El,Elx),
  roundTuple(Lc,[Elx],Tx).
examineConstraint(T,T) :-
  locOfAst(T,Lc),
  reportError("cannot figure out constraint %s",[ast(T)],Lc).

macroConstraintArg(A,Ax) :-
  macroAst(A,constraint,macros:examineConstraintArg,Ax).

examineConstraintArg(T,Tx) :- isBinary(T,Lc,"->>",L,R),!,
  macroType(L,Lx),
  macroType(R,Rx),
  binary(Lc,"->>",Lx,Rx,Tx).
examineConstraintArg(T,Tx) :-
  macroType(T,Tx).
  
macroTerm(T,Tx) :-
  macroAst(T,expression,macros:examineTerm,Tx).

examineTerm(T,T) :-
  isIden(T,_,_),!.
examineTerm(T,T) :-
  isEnum(T,_,N),isIden(N,_,_),!.
examineTerm(T,T) :-
  isInteger(T,_,_),!.
examineTerm(T,T) :-
  isBigInt(T,_,_),!.
examineTerm(T,T) :-
  isFloat(T,_,_),!.
examineTerm(T,T) :-
  isChar(T,_,_),!.
examineTerm(T,T) :-
  isString(T,_,_),!.
examineTerm(T,Tx) :-
  isTypeAnnotation(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroType(R,Rx),
  typeAnnotation(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isCoerce(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroType(R,Rx),
  coerce(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isCellRef(T,Lc,L),!,
  macroTerm(L,Lx),
  cellRef(Lc,Lx,Tx).
examineTerm(T,Tx) :-
  isRef(T,Lc,R),!,
  macroTerm(R,Rx),
  mkRef(Lc,Rx,Tx).
examineTerm(T,Tx) :-
  isLetDef(T,Lc,D,B),!,
  map(D,macros:macroStmt,Dx),
  macroTerm(B,Bx),
  mkLetDef(Lc,Dx,Bx,Tx).
examineTerm(T,Tx) :-
  isLetRec(T,Lc,D,B),!,
  map(D,macros:macroStmt,Dx),
  macroTerm(B,Bx),
  mkLetRec(Lc,Dx,Bx,Tx).
examineTerm(T,Tx) :-
  isConApply(T,Lc,O,As),!,
  map(As,macros:macroTerm,Ax),
  macroTerm(O,Ox),
  mkConApply(Lc,Ox,Ax,Tx).
examineTerm(T,Tx) :-
  isRoundTerm(T,Lc,O,D),!,
  map(D,macros:macroTerm,Dx),
  macroTerm(O,Ox),
  roundTerm(Lc,Ox,Dx,Tx).
examineTerm(T,Tx) :-
  isComprehension(T,Lc,Bnd,Bdy),!,
  macroTerm(Bnd,Bndx),
  macroTerm(Bdy,Bdyx),
  mkComprehension(Lc,Bndx,Bdyx,Tx).
examineTerm(T,Tx) :-
  isIotaComprehension(T,Lc,Bnd,Bdy),!,
  macroTerm(Bnd,Bndx),
  macroTerm(Bdy,Bdyx),
  mkIotaComprehension(Lc,Bndx,Bdyx,Tx).
examineTerm(T,Tx) :-
  isTestComprehension(T,Lc,Bdy),!,
  macroTerm(Bdy,Bdyx),
  mkTestComprehension(Lc,Bdyx,Tx).
examineTerm(T,Tx) :-
  isTotalizerComprehension(T,Lc,Fun,El,Zr,Bdy),!,
  macroTerm(Fun,Fnx),
  macroTerm(El,Elx),
  macroTerm(Zr,Zx),
  macroTerm(Bdy,Bdyx),
  mkTotalizerComprehension(Lc,Fnx,Elx,Zx,Bdyx,Tx).
examineTerm(T,Tx) :-
  isMapLiteral(T,Lc,D),!,
  map(D,macros:macroPair(macros:examineTerm),expression,Dx),
  mkMapLiteral(Lc,Dx,Tx).
examineTerm(T,Tx) :-
  isSquareTerm(T,Lc,O,D),!,
  map(D,macros:macroTerm,Dx),
  macroTerm(O,Ox),
  squareTerm(Lc,Ox,Dx,Tx).
examineTerm(T,Tx) :-
  isRoundTuple(T,Lc,D),!,
  map(D,macros:macroTerm,Dx),
  roundTuple(Lc,Dx,Tx).
examineTerm(T,Tx) :-
  isSquareTuple(T,Lc,D),!,
  map(D,macros:macroTerm,Dx),
  squareTuple(Lc,Dx,Tx).
examineTerm(T,Tx) :-
  isCons(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  mkCons(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isComma(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  comma(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isPair(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  pair(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isSequence(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  mkSequence(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isWhere(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  mkWhere(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isMatch(T,Lc,L,R),!,
  macroPtn(L,Lx),
  macroTerm(R,Rx),
  match(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isSearch(T,Lc,L,R),!,
  macroPtn(L,Lx),
  macroTerm(R,Rx),
  search(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isConjunct(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  conjunct(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isDisjunct(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  disjunct(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isNegation(T,Lc,R),!,
  macroTerm(R,Rx),
  negation(Lc,Rx,Tx).
examineTerm(T,Tx) :-
  isConditional(T,Lc,Ts,L,R),!,
  macroTerm(Ts,Tsx),
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  conditional(Lc,Tsx,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isForall(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  mkForall(Lc,Lx,Rx,Tx).
examineTerm(T,Tx) :-
  isEquation(T,Lc,L,G,R),!,
  macroHead(L,Lx),
  macroTerm(R,Rx),
  macroOpt(G,macros:macroTerm,Gx),
  mkEquation(Lc,Lx,Gx,Rx,Tx).
examineTerm(T,Tx) :-
  isValof(T,Lc,A),
  isBraceTuple(A,_,[As]),!,
  macroAction(As,A1),
  braceTuple(Lc,[A1],Ax),
  mkValof(Lc,Ax,Tx).
examineTerm(T,Tx) :-
  isFieldAcc(T,Lc,L,F),!,
  macroTerm(L,Lx),
  fieldAcc(Lc,Lx,name(Lc,F),Tx).
examineTerm(T,Tx) :-
  isTupleAcc(T,Lc,L,F),!,
  macroTerm(L,Lx),
  tupleAcc(Lc,Lx,F,Tx).
examineTerm(T,Tx) :-
  isOpen(T,Lc,E),!,
  macroTerm(E,Ex),
  mkOpen(Lc,Ex,Tx).
examineTerm(T,Tx) :-
  isSuppress(T,Lc,I),
  macroTerm(I,Ix),
  mkSuppress(Lc,Ix,Tx).
examineTerm(T,Tx) :-
  isRecordUpdate(T,Lc,R,F,V),!,
  macroTerm(R,Rx),
  macroTerm(V,Vx),
  recordUpdate(Lc,Rx,F,Vx,Tx).
examineTerm(T,Tx) :-
  isCaseExp(T,Lc,E,C),!,
  macroTerm(E,Ex),
  map(C,macros:macroLambda,Cx),
  caseExp(Lc,Ex,Cx,Tx).
examineTerm(T,Tx) :-
  isTask(T,Lc,B),!,
  macroAction(B,Bx),
  mkTask(Lc,Bx,Tx).
examineTerm(T,Tx) :-
  isGenerator(T,Lc,B),!,
  macroAction(B,Bx),
  mkGenerator(Lc,Bx,Tx),
  reportMsg("we have generator %s",[ast(Tx)]).
examineTerm(T,Tx) :-
  isBraceTuple(T,Lc,D),!,
  map(D,macros:macroStmt,Dx),
  braceTuple(Lc,Dx,Tx).
examineTerm(T,Tx) :-
  isQBraceTuple(T,Lc,D),!,
  map(D,macros:macroStmt,Dx),
  qbraceTuple(Lc,Dx,Tx).
examineTerm(T,Tx) :-
  isBraceTerm(T,Lc,O,D),!,
  map(D,macros:macroStmt,Dx),
  macroTerm(O,Ox),
  braceTerm(Lc,Ox,Dx,Tx).
examineTerm(T,Tx) :-
  isQBraceTerm(T,Lc,O,D),!,
  map(D,macros:macroStmt,Dx),
  macroTerm(O,Ox),
  qbraceTerm(Lc,Ox,Dx,Tx).
examineTerm(A,Ax) :-
  isTryCatch(A,Lc,B,E,C),!,
  macroTerm(B,Bx),
  macroType(E,Ex),
  map(C,macros:macroLambda,Cs),
  mkTryCatch(Lc,Bx,Ex,Cs,Ax).
examineTerm(A,Ax) :-
  isTry(A,Lc,B,C),!,
  macroTerm(B,Bx),
  map(C,macros:macroLambda,Cs),
  mkTry(Lc,Bx,Cs,Ax).
examineTerm(A,Ax) :-
  isRaise(A,Lc,V),!,
  macroTerm(V,Vx),
  mkRaise(Lc,Vx,Ax).
examineTerm(A,Ax) :-
  isThrow(A,Lc,V),!,
  macroTerm(V,Vx),
  mkThrow(Lc,Vx,Ax).
examineTerm(A,Ax) :-
  isResume(A,Lc,T,M),!,
  macroTerm(T,Tx),
  macroTerm(M,Mx),
  mkResume(Lc,Tx,Mx,Ax).
examineTerm(A,Ax) :-
  isSuspend(A,Lc,T,M),!,
  macroTerm(T,Tx),
  macroTerm(M,Mx),
  mkSuspend(Lc,Tx,Mx,Ax).
examineTerm(A,Ax) :-
  isRetire(A,Lc,T,M),!,
  macroTerm(T,Tx),
  macroTerm(M,Mx),
  mkRetire(Lc,Tx,Mx,Ax).
examineTerm(A,Ax) :-
  isThunk(A,Lc,V),!,
  macroTerm(V,Vx),
  mkThunk(Lc,Vx,Ax).
examineTerm(A,Ax) :-
  isThunkRef(A,Lc,V),!,
  macroTerm(V,Vx),
  mkThunkRef(Lc,Vx,Ax).
examineTerm(T,Tx) :-
  isParseCall(T,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  mkParseCall(Lc,Lx,Rx,Tx).
examineTerm(T,T) :-
  locOfAst(T,Lc),
  reportError("cannot figure out expression %s",[ast(T)],Lc).

macroOpt(none,_,none) :-!.
macroOpt(some(A),C,some(Ax)) :-
  call(C,A,Ax).

macroPair(Ex,Cxt,(A,B),(Ax,Bx)) :-
  macroAst(A,Cxt,Ex,Ax),
  macroAst(B,Cxt,Ex,Bx).

macroLambda(A,Ax) :-
  macroAst(A,rule,macros:examineLambda,Ax).

examineLambda(S,Rp) :-
  isEquation(S,Lc,P,G,V),!,
  macroHead(P,PP),
  macroOpt(G,macros:macroTerm,GG),
  macroTerm(V,VV),
  mkEquation(Lc,PP,GG,VV,Rp).
examineLambda(S,S) :-
  locOfAst(S,Lc),!,
  reportError("cannot figure out equation %s",[ast(S)],Lc).

macroHead(H,Hx) :-
  isDefault(H,Lc,P),!,
  macroPtn(P,Px),
  mkDefault(Lc,Px,Hx).
macroHead(H,Hx) :-
  macroPtn(H,Hx).
  
macroPtn(T,Tx) :-
  macroAst(T,pattern,macros:examinePtn,Tx).

examinePtn(T,T) :-
  isIden(T,_,_),!.
examinePtn(T,T) :-
  isEnum(T,_,N),isIden(N,_,_),!.
examinePtn(T,T) :-
  isInteger(T,_,_),!.
examinePtn(T,T) :-
  isBigInt(T,_,_),!.
examinePtn(T,T) :-
  isFloat(T,_,_),!.
examinePtn(T,T) :-
  isString(T,_,_),!.
examinePtn(T,T) :-
  isChar(T,_,_),!.
examinePtn(T,Tx) :-
  isTypeAnnotation(T,Lc,L,R),!,
  macroPtn(L,Lx),
  macroType(R,Rx),
  typeAnnotation(Lc,Lx,Rx,Tx).
examinePtn(T,Tx) :-
  isBraceTuple(T,Lc,D),!,
  map(D,macros:macroField,Dx),
  braceTuple(Lc,Dx,Tx).
examinePtn(T,Tx) :-
  isBraceTerm(T,Lc,O,D),!,
  map(D,macros:macroField,Dx),
  macroTerm(O,Ox),
  braceTerm(Lc,Ox,Dx,Tx).
examinePtn(T,Tx) :-
  isConApply(T,Lc,O,D),!,
  map(D,macros:macroPtn,Dx),
  macroTerm(O,Ox),
  mkConApply(Lc,Ox,Dx,Tx).
examinePtn(T,Tx) :-
  isRoundTerm(T,Lc,O,D),!,
  map(D,macros:macroPtn,Dx),
  macroTerm(O,Ox),
  roundTerm(Lc,Ox,Dx,Tx).
examinePtn(T,Tx) :-
  isSquareTerm(T,Lc,O,D),!,
  map(D,macros:macroPtn,Dx),
  macroTerm(O,Ox),
  squareTerm(Lc,Ox,Dx,Tx).
examinePtn(T,Tx) :-
  isRoundTuple(T,Lc,D),!,
  map(D,macros:macroPtn,Dx),
  roundTuple(Lc,Dx,Tx).
examinePtn(T,Tx) :-
  isSquareTuple(T,Lc,D),!,
  map(D,macros:macroPtn,Dx),
  squareTuple(Lc,Dx,Tx).
examinePtn(T,Tx) :-
  isCons(T,Lc,L,R),!,
  macroPtn(L,Lx),
  macroPtn(R,Rx),
  mkCons(Lc,Lx,Rx,Tx).
examinePtn(T,Tx) :-
  isComma(T,Lc,L,R),!,
  macroPtn(L,Lx),
  macroPtn(R,Rx),
  comma(Lc,Lx,Rx,Tx).
examinePtn(T,Tx) :-
  isPair(T,Lc,L,R),!,
  macroPtn(L,Lx),
  macroPtn(R,Rx),
  pair(Lc,Lx,Rx,Tx).
examinePtn(T,Tx) :-
  isWhere(T,Lc,L,R),!,
  macroPtn(L,Lx),
  macroTerm(R,Rx),
  mkWhere(Lc,Lx,Rx,Tx).
examinePtn(T,T) :-
  locOfAst(T,Lc),
  reportError("cannot figure out pattern %s",[ast(T)],Lc).

macroField(F,Fx) :-
  macroAst(F,field,macros:examineField,Fx).

examineField(F,Fx) :-
  isDefn(F,Lc,L,R),!,
  examinePtn(R,Rx),
  mkDefn(Lc,L,Rx,Fx).

macroAction(T,Tx) :-
  macroAst(T,action,macros:examineAction,Tx).

examineAction(A,Ax) :-
  isActionSeq(A,Lc,L,R),!,
  macroAction(L,Lx),
  macroAction(R,Rx),
  mkActionSeq(Lc,Lx,Rx,Ax).
examineAction(A,Ax) :-
  isActionSeq(A,_,L),!,
  macroAction(L,Ax).
examineAction(A,Ax) :-
  isBraceTuple(A,Lc,[S]),!,
  macroAction(S,Sx),
  braceTuple(Lc,[Sx],Ax).
examineAction(A,A) :-
  isBraceTuple(A,_,[]),!.
examineAction(A,Ax) :-
  isLbldAction(A,Lc,Lb,B),!,
  macroAction(B,Bx),
  mkLbldAction(Lc,Lb,Bx,Ax).
examineAction(A,A) :-
  isBreak(A,_,_),!.
examineAction(A,Ax) :-
  isDefn(A,Lc,L,R),!,
  macroPtn(L,Lx),
  macroTerm(R,Rx),
  mkDefn(Lc,Lx,Rx,Ax).
examineAction(A,Ax) :-
  isMatch(A,Lc,L,R),!,
  macroPtn(L,Lx),
  macroTerm(R,Rx),
  match(Lc,Lx,Rx,Ax).
examineAction(A,Ax) :-
  isOptionMatch(A,Lc,L,R),!,
  macroPtn(L,Lx),
  macroTerm(R,Rx),
  unary(Lc,"some",Lx,Lh),
  match(Lc,Lh,Rx,Ax).
examineAction(A,Ax) :-
  isAssignment(A,Lc,L,R),!,
  macroTerm(L,Lx),
  macroTerm(R,Rx),
  assignment(Lc,Lx,Rx,Ax).
examineAction(A,Ax) :-
  isIfThenElse(A,Lc,T,L,R),!,
  macroTerm(T,Tx),
  macroAction(L,Lx),
  macroAction(R,Rx),
  mkIfThenElse(Lc,Tx,Lx,Rx,Ax).
examineAction(A,Ax) :-
  isIfThen(A,Lc,T,L),!,
  macroTerm(T,Tx),
  macroAction(L,Lx),
  mkIfThen(Lc,Tx,Lx,Ax).
examineAction(A,Ax) :-
  isTryCatch(A,Lc,B,E,C),!,
  macroAction(B,Bx),
  macroType(E,Ex),
  map(C,macros:actionCase,Cs),
  mkTryCatch(Lc,Bx,Ex,Cs,Ax).
examineAction(A,Ax) :-
  isTry(A,Lc,B,C),!,
  macroAction(B,Bx),
  map(C,macros:actionCase,Cs),
  mkTry(Lc,Bx,Cs,Ax).
examineAction(A,Ax) :-
  isThrow(A,Lc,V),!,
  macroTerm(V,Vx),
  mkThrow(Lc,Vx,Ax).
examineAction(A,Ax) :-
  isWhileDo(A,Lc,T,B),!,
  macroTerm(T,Tx),
  macroAction(B,Bx),
  mkWhileDo(Lc,Tx,Bx,Ax).
examineAction(A,Ax) :-
  isForDo(A,Lc,E,T,B),!,
  macroTerm(E,Ex),
  macroTerm(T,Tx),
  macroAction(B,Bx),
  mkForDo(Lc,Ex,Tx,Bx,Ax).
examineAction(A,Ax) :-
  isValis(A,Lc,V),!,
  macroTerm(V,Vx),
  mkValis(Lc,Vx,Ax).
examineAction(A,Ax) :-
  isRaise(A,Lc,V),!,
  macroTerm(V,Vx),
  mkRaise(Lc,Vx,Ax).
examineAction(A,Ax) :-
  isLetDef(A,Lc,D,B),!,
  map(D,macros:macroStmt,Dx),
  macroAction(B,Bx),
  mkLetDef(Lc,Dx,Bx,Ax).
examineAction(A,Ax) :-
  isLetRec(A,Lc,D,B),!,
  map(D,macros:macroStmt,Dx),
  macroAction(B,Bx),
  mkLetRec(Lc,Dx,Bx,Ax).
examineAction(T,Tx) :-
  isCaseExp(T,Lc,E,C),!,
  macroTerm(E,Ex),
  map(C,macros:actionCase,Cx),
  caseExp(Lc,Ex,Cx,Tx).
examineAction(A,Ax) :-
  isResume(A,Lc,T,M),!,
  macroTerm(T,Tx),
  macroTerm(M,Mx),
  mkResume(Lc,Tx,Mx,Ax).
examineAction(A,Ax) :-
  isSuspend(A,Lc,T,M),!,
  macroTerm(T,Tx),
  macroTerm(M,Mx),
  mkSuspend(Lc,Tx,Mx,Ax).
examineAction(A,Ax) :-
  isRetire(A,Lc,T,M),!,
  macroTerm(T,Tx),
  macroTerm(M,Mx),
  mkRetire(Lc,Tx,Mx,Ax).
examineAction(A,Ax) :-
  isRoundTerm(A,_,_,_),!,
  macroTerm(A,Ax).
examineAction(T,T) :-
  locOfAst(T,Lc),
  reportError("cannot figure out action %s",[ast(T)],Lc).

actionCase(A,Ax) :-
  macroAst(A,actionRule,macros:examineActionCase,Ax).

examineActionCase(A,Ax) :-
  isEquation(A,Lc,P,G,V),!,
  macroHead(P,PP),
  macroOpt(G,macros:macroTerm,GG),
  macroAction(V,VV),
  mkEquation(Lc,PP,GG,VV,Ax).
examineActionCase(T,T) :-
  locOfAst(T,Lc),
  reportError("'%s' should be an action case",[ast(T)],Lc).

macroKey(name(_,Nm),Nm).
macroKey(qnm(_,Nm),Nm).
macroKey(integer(_,_),"$integer").
macroKey(bigint(_,_),"$bigint").
macroKey(float(_,_),"$float").
macroKey(string(_,_),"$string").
macroKey(char(_,_),"$char").
macroKey(tuple(_,Op,_),Op).
macroKey(app(_,Op,tuple(_,"()",_)),Ky) :-
  macroKey(Op,Ky).
macroKey(app(_,_,tuple(_,"[]",_)),"$[]").
macroKey(app(_,Op,tuple(_,"{}",_)),Ky) :-
  macroKey(Op,Ky0),
  string_concat(Ky0,"{}",Ky).
