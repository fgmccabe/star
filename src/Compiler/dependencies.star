star.compiler.dependencies{
  import star.
  import star.topsort.
  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.operators.
  import star.compiler.meta.
  import star.compiler.wff.
  import star.compiler.misc.

  public visMap ~> cons[(defnSp,visibility)].

  public dependencies:(cons[ast]) =>
    (visMap,cons[ast],map[string,ast],cons[cons[defnSpec]]).
  dependencies(Dfs) => valof{
    if traceDependencies! then
      showMsg("look for dependencies in $(Dfs)");
    (Defs,Pb,As,Opn) = collectDefinitions(Dfs);
    AllRefs = foldLeft((D,M)=>collectRef(D,M),[],Defs);
    InitDefs = collectThetaRefs(Defs,AllRefs,As,[]);
    Groups = (topsort(InitDefs) // (Gp)=>(Gp//((.definition(Sp,Lc,_,Els))=>.defnSpec(Sp,Lc,Els))));
    if traceDependencies! then
      showMsg("groups $(Groups)");
    valis (Pb,Opn,As,Groups)
  }

  private collectRef:(defnSpec,map[defnSp,defnSp])=>map[defnSp,defnSp].
  collectRef(.defnSpec(.conSp(Nm),_,[St]),M) where (_,_,Els) ?= isContractStmt(St) =>
    foldLeft((El,MM) => ((_,N,_) ?= isTypeAnnotation(El) && (_,Id)?=isName(N) ??
	  MM[.varSp(Id)->.conSp(Nm)] || MM),
      M[.conSp(Nm)->.conSp(Nm)],Els).
  collectRef(.defnSpec(N,_,_),M) => M[N->N].

  definitionSpec ::= .definition(defnSp,option[locn],cons[defnSp],cons[ast]).

  implementation depends[definitionSpec->>defnSp] => {
    references(.definition(_,_,Refs,_)) => Refs.
    defined(.definition(Sp,_,_,_),Rf) => Sp==Rf.
  }

  implementation display[definitionSpec] => {
    disp(.definition(Sp,Lc,Refs,_)) => "$(Sp)->$(Refs)".
  }

  collectDefinitions:(cons[ast]) => (cons[defnSpec],visMap,map[string,ast],cons[ast]).
  collectDefinitions(Stmts) => collectDefs(Stmts,[],[],[],[]).

  collectDefs:(cons[ast],cons[defnSpec],visMap,map[string,ast],cons[ast]) => (cons[defnSpec],visMap,map[string,ast],cons[ast]).
  
  collectDefs([],Defs,Pb,As,Opn) => (Defs,Pb,As,Opn).
  collectDefs([A,..Ss],Defs,Pb,As,Opn) where _ ?= isAnnotation(A) =>
    collectDefs(Ss,Defs,Pb,As,Opn).
  collectDefs([A,..Ss],Defs,Pb,As,Opn) => valof{
    (SS1,Dfs1,Pb1,As1,Opn1) = collectDefinition(A,Ss,Defs,Pb,As,Opn,.deFault);
    valis collectDefs(SS1,Dfs1,Pb1,As1,Opn1)
  }
    
  collectDefinition:(ast, cons[ast], cons[defnSpec], visMap,
    map[string,ast], cons[ast], visibility) =>
    (cons[ast],cons[defnSpec], visMap,map[string,ast],cons[ast]).

  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_) where
      (_,Ai) ?= isPublic(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Opn,.pUblic).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_) where
      (_,Ai) ?= isPrivate(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Opn,.priVate).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz) where (Lc,V,T) ?= isTypeAnnotation(A) => valof{
    if(ILc,Id) ?= isName(V) then{
      valis (Stmts,Defs,[(.varSp(Id),Vz),..Pb],As[Id->T],Opn)
    }
    else{
      reportError("expecting an identifier, not $(V)",locOf(V));
      valis (Stmts,Defs,Pb,As,Opn)
    }
  }
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz) where (Lc,Lhs,Els) ?= isContractStmt(A) &&
      (_,Q,C,Nm,Ts,Ds) ?= isContractSpec(Lhs)  =>
    (Stmts,[.defnSpec(.conSp(Nm),Lc,[A]),..Defs],[(.conSp(Nm),Vz),..Pb],
      generateAnnotations(Els,Q,C,As),Opn).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz) where
      (Lc,_,_,Cn,_) ?= isImplementationStmt(A) &&
      Sp .= .implSp(implementedContractName(Cn)) =>
    (Stmts,[.defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz) where
      (Lc,_,_,L,R) ?= isTypeFunStmt(A) && Sp .= .tpSp(typeName(L)) =>
    (Stmts,[.defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz) where
      (Lc,_,Cx,Hd,R) ?= isAlgebraicTypeStmt(A) => valof{
	Id = surfaceName(Hd);
	(Dfs,Pbs) = collectConstructors(R,Hd,Defs,Pb,Vz);
	valis (Stmts,[.defnSpec(.tpSp(Id),Lc,[A]),..Dfs],[(.tpSp(Id),Vz),..Pbs],As,Opn)
      }.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz) where
      (Lc,_,Cx,Hd,R) ?= isStructTypeStmt(A) => valof{
	Id = surfaceName(Hd);
	(Dfs,Pbs) = collectStruct(R,Hd,Defs,Pb,Vz);
	valis (Stmts,[.defnSpec(.tpSp(Id),Lc,[A]),..Dfs],[(.tpSp(Id),Vz),..Pbs],As,Opn)
      }.
  collectDefinition(A,Ss,Defs,Pb,As,Opn,Vz) where
      (Lc,Nm,Rhs) ?= isDefn(A) && (_,Id) ?= isName(Nm) => valof{
	Sp = .varSp(Id);
	valis (Ss,[.defnSpec(Sp,Lc,[A]),..Defs],publishName(Sp,Vz,Pb),As,Opn)
      }.
  collectDefinition(A,Ss,Defs,Pb,As,Opn,Vz) where
      (Lc,Nm,Rhs) ?= isAssignment(A) && (_,Id) ?= isName(Nm) => valof{
	Sp = .varSp(Id);
	valis (Ss,[.defnSpec(Sp,Lc,[A]),..Defs],publishName(Sp,Vz,Pb),As,Opn)
      }.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz) where
      (Lc,Nm) ?= ruleName(A) => valof{
	if DLc ?= isDefined(.varSp(Nm),Defs) then{
	  reportError("$(Nm) already defined at $(DLc)",Lc);
	  valis (Stmts,Defs,Pb,As,Opn)
	}
	else{
	  (Ss,Dfs) = collectDefines(Stmts,Nm,[A]);
	  Sp = .varSp(Nm);
	  valis (Ss,[.defnSpec(Sp,Lc,reverse(Dfs)),..Defs],publishName(Sp,Vz,Pb),As,Opn)
	}
      }.
  collectDefinition(A,Ss,Defs,Pb,As,Opn,_Vz) => valof{
    reportError("Cannot understand definition $(A)",locOf(A));
    valis (Ss,Defs,Pb,As,Opn)
  }.

  collectConstructors(T,H,Defs,Pb,Vz) where (_,L,R) ?= isBinary(T,"|") => valof{
    (D1,Pb1) = collectConstructors(L,H,Defs,Pb,Vz);
    valis collectConstructors(R,H,D1,Pb1,Vz)
  }
  collectConstructors(T,H,Defs,Pb,Vz) where (_,R) ?= isUnary(T,"|") => 
    collectConstructors(R,H,Defs,Pb,Vz).
  collectConstructors(T,H,Defs,Pb,Vz) where (Lc,Id) ?= isEnumSymb(T) => valof{
    Sp = .cnsSp(Id);
    valis ([.defnSpec(Sp,Lc,[H]),..Defs],publishName(Sp,Vz,Pb))
  }
  collectConstructors(T,H,Defs,Pb,Vz) where (Lc,Op,Els) ?= isEnumCon(T) && (_,Id) ?= isName(Op) => valof{
    Sp = .cnsSp(Id);
    valis ([.defnSpec(Sp,Lc,[mkConstructorType(Lc,rndTuple(Lc,Els),H)]),..Defs],publishName(Sp,Vz,Pb))
  }
  collectConstructors(T,H,Defs,Pb,Vz) where (_Lc,_Q,I) ?= isQuantified(T) =>
    collectConstructors(I,H,Defs,Pb,Vz).
  collectConstructors(T,H,Defs,Pb,Vz) where (_Lc,_Q,I) ?= isXQuantified(T) =>
    collectConstructors(I,H,Defs,Pb,Vz).
  collectConstructors(T,_,Defs,Pb,_Vz) default => (Defs,Pb).

  collectStruct(T,H,Defs,Pb,Vz) where
      (Lc,Q,X,Id,Els) ?= isBraceCon(T) => valof{
	Sp = .cnsSp(Id);
    valis ([.defnSpec(Sp,Lc,[reUQuant(Lc,Q,reXQuant(Lc,X,mkConstructorType(Lc,brTuple(Lc,Els),H)))]),..Defs],publishName(Sp,Vz,Pb))
      }.

  isDefined:(defnSp,cons[defnSpec])=>option[locn].
  isDefined(_,[]) => .none.
  isDefined(Sp,[.defnSpec(Sp,Lc,Sts),..Defs]) => Lc.
  isDefined(Sp,[_,..Defs]) => isDefined(Sp,Defs).

  publishName:(defnSp,visibility,visMap)=>visMap.
  publishName(Nm,_,Pb) where {? (Nm,_) in Pb ?} => Pb.
  publishName(Nm,Vz,Pb) => [(Nm,Vz),..Pb].

  collectDefines:(cons[ast],string,cons[ast]) => (cons[ast],cons[ast]).
  collectDefines([St,..Ss],Nm,Dfs) where
      (_,Nm) ?= ruleName(St) => collectDefines(Ss,Nm,[St,..Dfs]).
  collectDefines(Ss,Nm,Dfs) default => (Ss,Dfs).
	
  generateAnnotations:(cons[ast],cons[ast],cons[ast],map[string,ast]) =>
    map[string,ast].
  generateAnnotations([],_,_,As) => As.
  generateAnnotations([A,..Ss],Qs,Cs,As) where
      (Lc,V,T) ?= isTypeAnnotation(A) && (_,Id) ?= isName(V) =>
    generateAnnotations(Ss,Qs,Cs,As[Id->reUQuant(Lc,Qs,reConstrain(Cs,T))]).
  generateAnnotations([A,..Ss],Qs,Cs,As) =>
    generateAnnotations(Ss,Qs,Cs,As).

  collectThetaRefs:(cons[defnSpec],map[defnSp,defnSp],map[string,ast],cons[definitionSpec]) => cons[definitionSpec].
  collectThetaRefs([],_,_,DSpecs) => DSpecs.
  collectThetaRefs([.defnSpec(.cnsSp(Nm),Lc,[Def]),..Defs],AllRefs,Annots,S) => valof{
    Refs = collectTypeRefs(Def,AllRefs,[]);
    valis collectThetaRefs(Defs,AllRefs,Annots,[.definition(.cnsSp(Nm),Lc,Refs,[Def]),..S])
  }
  collectThetaRefs([.defnSpec(Defines,Lc,Stmts),..Defs],AllRefs,Annots,S) => valof{
    Refs = collectStmtsRefs(Stmts,AllRefs,Annots,[]);
    valis collectThetaRefs(Defs,AllRefs,Annots,[.definition(Defines,Lc,Refs,Stmts),..S])
  }.

  collectEnvRefs:(cons[ast],map[defnSp,defnSp],map[string,ast],cons[defnSp]) =>
    cons[defnSp].
  collectEnvRefs(Defs,All,Annots,Rf) =>
    collectStmtsRefs(Defs,locallyDefined(Defs,All),Annots,Rf).

  locallyDefined:(cons[ast],map[defnSp,defnSp]) => map[defnSp,defnSp].
  locallyDefined([],All) => All.
  locallyDefined([St,..Stmts],All) =>
    locallyDefined(Stmts,removeLocalDef(St,All)).

  removeLocalDef(St,All) where (_,Id) ?= ruleName(St) =>
    All[~.varSp(Id)].

  collectStmtsRefs([],_,_,Rf) => Rf.
  collectStmtsRefs([St,..Sts],All,Annots,Rf) => valof{
    Rf0 = collectStmtRefs(St,All,Annots,Rf);
    valis collectStmtsRefs(Sts,All,Annots,Rf0)
  }

  collectStmtRefs:(ast,map[defnSp,defnSp],map[string,ast],cons[defnSp]) => cons[defnSp].
  collectStmtRefs(A,All,Annots,Rf) where (_,_,Tp) ?= isTypeAnnotation(A) =>
    collectTypeRefs(Tp,All,Rf).
  collectStmtRefs(A,All,Annots,Rf) where (_,I) ?= isPublic(A) =>
    collectStmtRefs(I,All,Annots,Rf).
  collectStmtRefs(A,All,Annots,Rf) where (_,I) ?= isPrivate(A) =>
    collectStmtRefs(I,All,Annots,Rf).
  collectStmtRefs(A,All,Annots,Rf) where (_,V,D) ?= isDefn(A) =>
    collectTermRefs(D,All,collectAnnotRefs(V,All,Annots,Rf)).
  collectStmtRefs(A,All,Annots,Rf) where (_,V,D) ?= isAssignment(A) =>
    collectTermRefs(D,All,collectAnnotRefs(V,All,Annots,Rf)).
  collectStmtRefs(A,All,Annots,Rf) where (_,.some(Nm),_,H,C,R) ?= isEquation(A) => valof{
    Rf0 = collectAnnotRefs(Nm,All,Annots,Rf);
    Rf1 = collectHeadRefs(H,C,All,Rf0);
    valis collectTermRefs(R,All,Rf1)
  }
  collectStmtRefs(A,All,Annots,Rf) where (Lc,Q,Cx,L,R) ?= isAlgebraicTypeStmt(A) => valof{
    A0 = filterOut(All,Q);
    Rf0 = collectConstraintRefs(Cx,A0,Rf);
    valis collectConstructorRefs(R,A0,Rf0);
  }
  collectStmtRefs(A,All,Annots,Rf) where (Lc,Q,Cx,L,R) ?= isStructTypeStmt(A) => valof{
    A0 = filterOut(All,Q);
    Rf0 = collectConstraintRefs(Cx,A0,Rf);
    valis collectStructRefs(R,A0,Rf0);
  }
  collectStmtRefs(A,All,Annots,Rf) where
      (_,Q,Cx,L,R) ?= isTypeFunStmt(A) => valof{
	A0 = filterOut(All,Q);
	Rf0 = collectConstraintRefs(Cx,A0,Rf);
	Rf1 = collectTypeRefs(L,A0,Rf0);
	valis collectTypeRefs(R,A0,Rf1)
      }.
  collectStmtRefs(A,All,Annots,Rf) where
      (Lc,Lhs,Els) ?= isContractStmt(A) && (_,Q,C,_,As,Ds) ?= isContractSpec(Lhs)  => valof{
	A0 = filterOut(All,Q);
	Rf0 = collectConstraintRefs(C,A0,Rf);
	valis collectFaceTypes(Els,A0,Rf0)
      }.
  collectStmtRefs(A,All,Annots,Rf) where
      (_,Q,Cx,Tp,Exp) ?= isImplementationStmt(A) => valof{
	A0 = filterOut(All,Q);
	Rf1 = collectConstraintRefs([Tp,..Cx],A0,Rf);
	valis collectTermRefs(Exp,A0,Rf1)
      }.
  collectStmtRefs(A,All,Annots,Rf) => valof{
    reportError("cannot fathom definition $(A)",locOf(A));
    valis Rf
  }.

  collectHeadRefs(H,.some(C),All,Rf) => 
    collectCondRefs(C,All,collectTermRefs(H,All,Rf)).
  collectHeadRefs(H,.none,All,Rf) =>
    collectTermRefs(H,All,Rf).
    
  collectAnnotRefs(H,All,Annots,Rf) where Id?=headName(H) =>
    (Tp ?= Annots[Id] ?? collectTypeRefs(Tp,All,Rf) || Rf).
  collectAnnotRefs(H,_,_,Rf) => valof{
    reportError("not a head: $(H)",locOf(H));
    valis Rf
  }.

  collectCondRefs:(ast,map[defnSp,defnSp],cons[defnSp]) => cons[defnSp].
  collectCondRefs(A,All,Rf) where (_,L,R) ?= isConjunct(A) =>
    collectCondRefs(R,All,collectCondRefs(L,All,Rf)).
  collectCondRefs(A,All,Rf) where (_,L,R) ?= isDisjunct(A) =>
    collectCondRefs(R,All,collectCondRefs(L,All,Rf)).
  collectCondRefs(A,All,Rf) where (_,R) ?= isNegation(A) => 
    collectCondRefs(R,All,Rf).
  collectCondRefs(A,All,Rf) where (_,T,L,R) ?= isConditional(A) => valof{
    Rf0 = collectCondRefs(T,All,Rf);
    Rf1 = collectCondRefs(L,All,Rf0);
    valis collectCondRefs(R,All,Rf1)
  }
  collectCondRefs(A,All,Rf) where (_,[C]) ?= isTuple(A) => 
    collectCondRefs(C,All,Rf).
  collectCondRefs(E,All,Rf) => collectTermRefs(E,All,Rf).
    
  collectTermRefs:(ast,map[defnSp,defnSp],cons[defnSp]) => cons[defnSp].
  collectTermRefs(V,All,Rf) where _ ?= isAnon(V) => Rf.
  collectTermRefs(V,All,Rf) where (_,Id) ?= isName(V) => 
    collectName(.cnsSp(Id),All,collectName(.varSp(Id),All,Rf)).
  collectTermRefs(T,All,Rf) where (_,Id) ?= isEnumSymb(T) =>
    collectName(.cnsSp(Id),All,Rf).
  collectTermRefs(T,All,Rf) where (_,Op,Args) ?= isEnumCon(T) =>
    collectTermListRefs(Args,All,collectTermRefs(Op,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,Lhs,Rhs) ?= isTypeAnnotation(T) =>
    collectTypeRefs(Rhs,All,collectTermRefs(Lhs,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,Env,Bnd) ?= isLetDef(T) =>
    collectStmtsRefs(Env,All,[],collectTermRefs(Bnd,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,Env,Bnd) ?= isLetRecDef(T) =>
    collectStmtsRefs(Env,All,[],collectTermRefs(Bnd,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,I) ?= isCellRef(T) =>
    collectTermRefs(I,All,Rf).
  collectTermRefs(T,All,Rf) where (_,I) ?= isRef(T) =>
    collectTermRefs(I,All,Rf).
  collectTermRefs(T,All,Rf) where (_,I) ?= isThunk(T) =>
    collectTermRefs(I,All,Rf).
  collectTermRefs(T,All,Rf) where (_,I) ?= isThunkRef(T) =>
    collectTermRefs(I,All,Rf).
  collectTermRefs(T,All,Rf) where (_,I) ?= isSuppress(T) =>
    collectTermRefs(I,All,Rf).
  collectTermRefs(T,All,Rf) where (_,Op,Args) ?= isRoundTerm(T) =>
    collectTermListRefs(Args,All,collectTermRefs(Op,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,Args) ?= isTuple(T) => 
    collectTermListRefs(Args,All,Rf).
  collectTermRefs(T,All,Rf) where (_,L,R) ?= isCons(T) =>
    collectTermRefs(R,All,collectTermRefs(L,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,Args) ?= isSqTuple(T) => 
    collectTermListRefs(Args,All,Rf).
  collectTermRefs(T,All,Rf) where (_,E,C) ?= isCase(T) =>
    collectCasesRefs(C,collectTermRefs,All,collectTermRefs(E,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,Sts) ?= isTheta(T) =>
    collectStmtsRefs(Sts,All,[],Rf).
  collectTermRefs(T,All,Rf) where (_,Sts) ?= isQTheta(T) =>
    collectStmtsRefs(Sts,All,[],Rf).
  collectTermRefs(T,All,Rf) where (_,L,R) ?= isMatch(T) =>
    collectTermRefs(R,All,collectTermRefs(L,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,L,R) ?= isOptionMatch(T) => 
    collectTermRefs(R,All,collectTermRefs(L,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,L,R) ?= isSearch(T) =>
    collectTermRefs(R,All,collectTermRefs(L,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,L,M,R) ?= isSlice(T) =>
    collectTermRefs(R,All,collectTermRefs(L,All,collectTermRefs(L,All,Rf))).
  collectTermRefs(T,All,Rf) where (_,L,R) ?= isCoerce(T) =>
    collectTypeRefs(R,All,collectTermRefs(L,All,Rf)).
  collectTermRefs(A,All,Rf) where (_,L,R) ?= isConjunct(A) =>
    collectCondRefs(R,All,collectCondRefs(L,All,Rf)).
  collectTermRefs(A,All,Rf) where (_,L,R) ?= isDisjunct(A) =>
    collectCondRefs(R,All,collectCondRefs(L,All,Rf)).
  collectTermRefs(A,All,Rf) where (_,R) ?= isNegation(A) => 
    collectCondRefs(R,All,Rf).
  collectTermRefs(A,All,Rf) where (_,T,L,R) ?= isConditional(A) =>
    collectTermRefs(R,All,collectTermRefs(L,All,collectCondRefs(T,All,Rf))).
  collectTermRefs(T,All,Rf) where (_,_,L,C,R) ?= isLambda(T) =>
    collectTermRefs(R,All,collectHeadRefs(L,C,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,L,R) ?= isWhere(T) =>
    collectCondRefs(R,All,collectTermRefs(L,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,L) ?= isValof(T) =>
    ((_,[As]) ?= isBrTuple(L) ??
      collectDoRefs(As,All,Rf) ||
	collectTermRefs(L,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,As) ?= isTaskExp(T) =>
    collectDoRefs(As,All,Rf).
  collectTermRefs(A,All,Rf) where (_,R) ?= isRaise(A) => 
    collectTermRefs(R,All,Rf).
  collectTermRefs(A,All,Rf) where (_,R) ?= isThrow(A) =>
    collectTermRefs(R,All,Rf).
  collectTermRefs(A,All,Rf) where (_,L,E,H) ?= isTryCatch(A) =>
    collectCasesRefs(H,collectTermRefs,All,collectTypeRefs(E,All,collectTermRefs(L,All,Rf))).
  collectTermRefs(A,All,Rf) where (_,L,R) ?= isResume(A) =>
    collectTermRefs(L,All,collectTermRefs(R,All,Rf)).
  collectTermRefs(A,All,Rf) where (_,L,R) ?= isSuspend(A) =>
    collectTermRefs(L,All,collectTermRefs(R,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,L,R) ?= isComprehension(T) =>
    collectCondRefs(R,All,collectTermRefs(L,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,R,_,V) ?= isRecordUpdate(T) => 
    collectTermRefs(V,All,collectTermRefs(R,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,L,R) ?= isComma(T) =>
    collectTermRefs(R,All,collectTermRefs(L,All,Rf)).
  collectTermRefs(T,All,Rf) where (_,L,_) ?= isFieldAcc(T) => 
    collectTermRefs(L,All,Rf).
  collectTermRefs(T,All,Rf) where (_,L,_) ?= isTupleAcc(T) => 
    collectTermRefs(L,All,Rf).
  collectTermRefs(T,All,Rf) where (_,L,Stmts) ?= isLabeledTheta(T) => valof{
    Rf1 = collectStmtsRefs(Stmts,All,[],Rf);
    valis collectTermRefs(dollarName(L),All,collectTermRefs(L,All,Rf1))
  }
  collectTermRefs(T,All,Rf) where (_,L,Stmts) ?= isBrTerm(T) => valof{
    Rf1 = collectStmtsRefs(Stmts,All,[],Rf);
    valis collectTermRefs(dollarName(L),All,collectTermRefs(L,All,Rf1))
  }
  collectTermRefs(.int(_,_),_,Rf) => Rf.
  collectTermRefs(.big(_,_),_,Rf) => Rf.
  collectTermRefs(.chr(_,_),_,Rf) => Rf.
  collectTermRefs(.str(_,_),_,Rf) => Rf.
  collectTermRefs(.num(_,_),_,Rf) => Rf.
  collectTermRefs(T,_,Rf) => valof{
    reportError("cant parse $(T) for references",locOf(T));
    valis Rf
  }.

  collectDoRefs:(ast,map[defnSp,defnSp],cons[defnSp]) => cons[defnSp].
  collectDoRefs(A,All,Rf) where (_,L,R) ?= isActionSeq(A) =>
    collectDoRefs(R,All,collectDoRefs(L,All,Rf)).
  collectDoRefs(A,All,Rf) where (_,_,In) ?= isLbldAction(A) =>
    collectDoRefs(In,All,Rf).
  collectDoRefs(A,_All,Rf) where _ ?= isBreak(A) => Rf.
  collectDoRefs(A,All,Rf) where (_,L,R) ?= isDefn(A) => 
    collectTermRefs(R,All,collectTermRefs(L,All,Rf)).
  collectDoRefs(A,All,Rf) where (_,L,R) ?= isMatch(A) =>
    collectTermRefs(R,All,collectTermRefs(L,All,Rf)).
  collectDoRefs(A,All,Rf) where (_,L,R) ?= isAssignment(A) =>
    collectTermRefs(R,All,collectTermRefs(L,All,Rf)).
  collectDoRefs(A,All,Rf) where (_,R) ?= isValis(A) => 
    collectTermRefs(R,All,Rf).
  collectDoRefs(A,All,Rf) where (_,R) ?= isRaise(A) => 
    collectTermRefs(R,All,Rf).
  collectDoRefs(A,All,Rf) where (_,R) ?= isThrow(A) =>
    collectTermRefs(R,All,Rf).
  collectDoRefs(A,All,Rf) where (_,L,E,H) ?= isTryCatch(A) =>
    collectCasesRefs(H,collectDoRefs,All,collectTypeRefs(E,All,collectDoRefs(L,All,Rf))).
  collectDoRefs(A,All,Rf) where (_,L,H) ?= isCase(A) =>
    collectCasesRefs(H,collectDoRefs,All,collectTermRefs(L,All,Rf)).
  collectDoRefs(T,All,Rf) where (_,Env,Bnd) ?= isLetDef(T) =>
    collectStmtsRefs(Env,All,[],collectDoRefs(Bnd,All,Rf)).
  collectDoRefs(T,All,Rf) where (_,Env,Bnd) ?= isLetRecDef(T) =>
    collectStmtsRefs(Env,All,[],collectDoRefs(Bnd,All,Rf)).
  collectDoRefs(A,All,Rf) where (_,T,L,R) ?= isIfThenElse(A) => valof{
    Rf0 = collectTermRefs(T,All,Rf);
    Rf1 = collectDoRefs(L,All,Rf0);
    valis collectDoRefs(R,All,Rf1)
  }
  collectDoRefs(A,All,Rf) where (_,T,L) ?= isIfThen(A) => valof{
    Rf0 = collectCondRefs(T,All,Rf);
    valis collectDoRefs(L,All,Rf0)
  }
  collectDoRefs(A,All,Rf) where (_,T,L) ?= isWhileDo(A) => valof{
    Rf0 = collectCondRefs(T,All,Rf);
    valis collectDoRefs(L,All,Rf0)
  }
  collectDoRefs(A,All,Rf) where (_,[S]) ?= isBrTuple(A) =>
    collectDoRefs(S,All,Rf).
  collectDoRefs(A,All,Rf) => collectTermRefs(A,All,Rf).

  collectCasesRefs([],_,_,Rf) => Rf.
  collectCasesRefs([St,..Sts],F,All,Rf) =>
    collectCasesRefs(Sts,F,All,collectCaseRefs(St,F,All,Rf)).

  collectCaseRefs(Cse,F,All,Rf) where (_,_,A,C,Rhs) ?= isLambda(Cse) =>
    F(Rhs,All,collectHeadRefs(A,C,All,Rf)).
  collectCaseRefs(Cse,_,_,Rf) => valof{
    reportError("invalid case in case expression $(Cse)",locOf(Cse));
    valis Rf
  }.
    
  collectTermListRefs:(cons[ast],map[defnSp,defnSp],cons[defnSp]) => cons[defnSp].
  collectTermListRefs([],_,Rf) => Rf.
  collectTermListRefs([T,..Ts],All,Rf) =>
    collectTermListRefs(Ts,All,collectTermRefs(T,All,Rf)).

  collectTypeRefs:(ast,map[defnSp,defnSp],cons[defnSp]) => cons[defnSp].
  collectTypeRefs(V,All,SoFar) where (_,Id) ?= isName(V) =>
    collectName(.tpSp(Id),All,SoFar).
  collectTypeRefs(T,All,SoFar) where (_,Op,Els) ?= isSquareTerm(T) => 
    collectTypeList(Els,All,collectTypeRefs(Op,All,SoFar)).
  collectTypeRefs(T,All,SoFar) where (_,L,R) ?= isConstructorType(T) =>
    collectTypeRefs(R,All,collectTypeRefs(L,All,SoFar)).
  collectTypeRefs(T,All,SoFar) where (_,L,R) ?= isFunctionType(T) =>
    collectTypeRefs(R,All,collectTypeRefs(L,All,SoFar)).
  collectTypeRefs(T,All,SoFar) where (_,R) ?= isGeneratorType(T) =>
    collectTypeRefs(R,All,SoFar).
  collectTypeRefs(T,All,SoFar) where (_,L,R) ?= isBinary(T,"->>") =>
    collectTypeRefs(R,All,collectTypeRefs(L,All,SoFar)).
  collectTypeRefs(T,All,SoFar) where (_,L,R) ?= isComma(T) =>
    collectTypeRefs(R,All,collectTypeRefs(L,All,SoFar)).
  collectTypeRefs(T,All,SoFar) where (_,R) ?= isUnary(T,"ref") => 
    collectTypeRefs(R,All,SoFar).
  collectTypeRefs(T,All,SoFar) where (_,L,R) ?= isBinary(T,"~>") =>
    collectTypeRefs(R,All,collectTypeRefs(L,All,SoFar)).
  collectTypeRefs(T,All,SoFar) where (_,L,R) ?= isBinary(T,"<~") =>
    collectTypeRefs(R,All,collectTypeRefs(L,All,SoFar)).
  collectTypeRefs(T,All,SoFar) where (_,Q,I) ?= isQuantified(T) =>
    collectTypeRefs(I,filterOut(All,Q),SoFar).
  collectTypeRefs(T,All,SoFar) where (_,Q,I) ?= isXQuantified(T) =>
    collectTypeRefs(I,filterOut(All,Q),SoFar).
  collectTypeRefs(T,All,Rf) where (_,Cx,Tp) ?= isConstrained(T) =>
    collectConstraintRefs(Cx,All,collectTypeRefs(Tp,All,Rf)).
  collectTypeRefs(T,All,SoFar) where (_,Els) ?= isTuple(T) =>
    collectTypeList(Els,All,SoFar).
  collectTypeRefs(T,All,SoFar) where (_,Els) ?= isBrTuple(T) =>
    collectFaceTypes(Els,All,SoFar).
  collectTypeRefs(T,All,SoFar) where (_,Op,Els) ?= isBrTerm(T) =>
    collectFaceTypes(Els,All,collectTermRefs(Op,All,SoFar)).
  collectTypeRefs(T,All,SoFar) where (_,Rc,_) ?= isFieldAcc(T) => 
    collectTermRefs(Rc,All,SoFar).
  collectTypeRefs(T,All,SoFar) where (_,Rc,_) ?= isTupleAcc(T) => 
    collectTermRefs(Rc,All,SoFar).
  collectTypeRefs(T,All,SoFar) where (_,Op,Els) ?= isRoundTerm(T) =>
    collectTypeList(Els,All,collectTypeRefs(Op,All,SoFar)).
  collectTypeRefs(T,_,Rf) default => valof{
    reportError("cannot fathom type `$(T)`",locOf(T));
    valis Rf
  }.
  
  collectTypeList:(cons[ast],map[defnSp,defnSp],cons[defnSp]) => cons[defnSp].
  collectTypeList([],_,Rf) => Rf.
  collectTypeList([T,..Ts],All,Rf) =>
    collectTypeList(Ts,All,collectTypeRefs(T,All,Rf)).

  collectConstraintRefs:(cons[ast],map[defnSp,defnSp],cons[defnSp]) => cons[defnSp].
  collectConstraintRefs([],_,Rf) => Rf.
  collectConstraintRefs([C,..Cs],All,Rf) => 
    collectConstraintRefs(Cs,All,collectConstraintRef(C,All,Rf)).
  
  collectConstraintRef(T,All,Rf) where _ ?= isSquareTerm(T) => 
    collectContractRefs(T,All,Rf).
  collectConstraintRef(T,All,Rf) where (_,_,Tp) ?= isImplicit(T) =>
    collectTypeRefs(Tp,All,Rf).
  collectConstraintRef(T,All,Rf) where (_,Tp) ?= isRaises(T) =>
    collectTypeRefs(Tp,All,Rf).
  collectConstraintRef(T,All,Rf) => collectTypeRefs(T,All,Rf).

  collectContractRefs:(ast,map[defnSp,defnSp],cons[defnSp]) => cons[defnSp].
  collectContractRefs(T,All,Rf) where
      (_,Op,Args) ?= isSquareTerm(T) && (_,Id)?=isName(Op) => valof {
    R0 = collectName(.conSp(Id),All,Rf);
    valis collectTypeList(Args,All,R0)
  }
  collectContractRefs(T,All,Rf) where (_,Q,I) ?= isQuantified(T) =>
    collectContractRefs(I,filterOut(All,Q),Rf).
  collectContractRefs(T,All,Rf) where (_,Q,I) ?= isXQuantified(T) =>
    collectContractRefs(I,filterOut(All,Q),Rf).

  collectConstructorRefs(T,All,SoFar) where (_,L,R) ?= isBinary(T,"|") =>
    collectConstructorRefs(R,All,collectConstructorRefs(L,All,SoFar)).
  collectConstructorRefs(T,All,SoFar) where (_,R) ?= isUnary(T,"|") =>
    collectConstructorRefs(R,All,SoFar).
  collectConstructorRefs(T,All,SoFar) where _ ?= isEnumSymb(T) => SoFar.
  collectConstructorRefs(T,All,SoFar) where (_,_,Els) ?= isEnumCon(T) =>
    collectTypeList(Els,All,SoFar).
  collectConstructorRefs(T,All,SoFar) where (_Lc,_Q,I) ?= isQuantified(T) =>
    collectConstructorRefs(I,All,SoFar).
  collectConstructorRefs(T,All,SoFar) where (_Lc,_Q,I) ?= isXQuantified(T) =>
    collectConstructorRefs(I,All,SoFar).
  collectConstructorRefs(T,_,SoFar) default => valof{
    reportError("cannot fathom constructor form $(T)",locOf(T));
    valis SoFar
  }

  collectStructRefs(T,All,SoFar) where (_,_,_,_,Els) ?= isBraceCon(T) =>
    collectFaceTypes(Els,All,SoFar).

  collectFaceTypes([],_,Rf) => Rf.
  collectFaceTypes([D,..Ds],All,Rf) => valof{
    R1 = collectFaceType(D,All,Rf);
    valis collectFaceTypes(Ds,All,R1)
  }

  collectFaceType(P,All,R) where (_,I) ?= isUnary(P,"type") =>
    collectFaceType(I,All,R).
  collectFaceType(P,All,R) where (_,L,T) ?= isTypeAnnotation(P) =>
    collectTypeRefs(T,All,R).
  collectFaceType(P,All,R) where (_,L,T) ?= isTypeLambda(P) =>
    collectTypeRefs(T,All,collectTypeRefs(L,All,R)).
  collectFaceType(P,All,R) where (_,L,T) ?= isTypeExists(P) =>
    collectTypeRefs(T,All,collectTypeRefs(L,All,R)).
  collectFaceType(_,All,R) => R.

  collectName:(defnSp,map[defnSp,defnSp],cons[defnSp])=>cons[defnSp].
  collectName(Sp,All,SoFar) where Rf?=All[Sp] => SoFar\+Rf.
  collectName(_,_,SoFar) default => SoFar.

  filterOut:(map[defnSp,defnSp],cons[ast]) => map[defnSp,defnSp].
  filterOut(M,Q) => let{.
    qName(V) where (_,Id) ?= isName(V) => .some(Id).
    qName(V) where (_,L,_) ?= isBinary(V,"/") => qName(L).
    qName(_) default => .none.
  .} in foldLeft((V,MM) where Id?=qName(V) => MM[~.varSp(Id)],M,Q).
}
