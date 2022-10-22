test.ex{
  import star.

  import star.compiler.ast.
  import star.compiler.wff.
  import star.compiler.errors.


  examineType(A,_) where _ ?= isName(A) => either(A).
  examineType(A,Rp) where (Lc,Op,Els) ?= isSquareTerm(A) => do{
    NEls <- seqmap((E)=>macroType(E,Rp),Els);
    valis squareTerm(Lc,Op,NEls)
  }
  examineType(A,Rp) where (Lc,L,R) ?= isConstructorType(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkConstructorType(Lc,NL,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ?= isFunctionType(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkFunctionType(Lc,NL,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ?= isBinary(A,"->>") => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis binary(Lc,"->>",NL,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ?= isComma(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis binary(Lc,",",NL,NR)
  }
  examineType(A,Rp) where (Lc,R) ?= isRef(A) => do{
    NR <- macroType(R,Rp);
    valis mkRef(Lc,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ?= isTypeLambda(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkTypeLambda(Lc,NL,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ?= isTypeExists(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkTypeExists(Lc,NL,NR)
  }
  examineType(A,Rp) where (Lc,Q,T) ?= isQuantified(A) => do{
    NT <- macroType(T,Rp);
    valis reUQuant(Lc,Q,NT)
  }
  examineType(A,Rp) where (Lc,Q,T) ?= isXQuantified(A) => do{
    NT <- macroType(T,Rp);
    valis reXQuant(Lc,Q,NT)
  }
  examineType(A,Rp) where (Lc,C,T) ?= isConstrained(A) => do{
    NC <- seqmap((X)=>macroConstraint(X,Rp),C);
    NT <- macroType(T,Rp);
    valis reConstrain(NC,NT)
  }
  examineType(A,Rp) where (Lc,Els) ?= isTuple(A) => do{
    NC <- seqmap((X)=>macroType(X,Rp),Els);
    valis rndTuple(Lc,NC)
  }
  examineType(A,Rp) where (Lc,Els) ?= isBrTuple(A) => do{
    NC <- macroStmts(Els,Rp);
    valis brTuple(Lc,NC)
  }
  examineType(A,Rp) where (Lc,R,F) ?= isFieldAcc(A) => do{
    RR <- macroTerm(R,Rp);
    valis mkFieldAcc(Lc,RR,F)
  }

  examineType(A,Rp) default =>
    other(reportError(Rp,"cannot figure out type expression $(A), key=$(macroKey(A))",locOf(A))).

  macroConstraint(A,Rp) => macroAst(A,.constraint,examineConstraint,Rp).

  examineConstraint(A,Rp) where (Lc,Op,Els) ?= isSquareTerm(A) => do{
    NEls <- seqmap((E)=>macroType(E,Rp),Els);
    valis squareTerm(Lc,Op,NEls)
  }
  examineType(A,Rp) where (Lc,L,R) ?= isTypeExists(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkTypeExists(Lc,NL,NR)
  }

  macroType(X,_) => either(X).
}
