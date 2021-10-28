star.compiler.macro.rules{
  import star.
  import star.sort.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.macro.infra.
  import star.compiler.wff.

  public applyRules:(ast,macroContext,macroState,reports) => result[reports,macroState].
  applyRules(A,Cxt,St,Rp) where Rules^=macros[macroKey(A)] =>
    applyRls(A,Cxt,St,Rules,Rp).
  applyRules(A,_,_,_) default => do{ valis .inactive}.

  applyRls:(ast,macroContext,macroState,cons[(macroContext,macroRule)],reports) =>
    result[reports,macroState].
  applyRls(A,_,St,[],_) => do{ valis St}.
  applyRls(A,Cxt,St,[(Cxt,R),..Rls],Rp) => do{
    Rslt <- R(A,Cxt,Rp);
    if .inactive.=Rslt then
      applyRls(A,Cxt,St,Rls,Rp)
    else
    valis Rslt
  }
  applyRls(A,Cxt,St,[_,..Rls],Rp) => 
    applyRls(A,Cxt,St,Rls,Rp).
  
  macros:map[string,cons[(macroContext,macroRule)]].
  macros = [ "::=" -> [(.statement,algebraicMacro)],
    ":=" -> [(.act,spliceAssignMacro),
      (.actn,indexAssignMacro),
      (.actn,assignMacro)]
    "[]" -> [(.pattern,squarePtnMacro),
      (.expression,macroListComprehension),
      (.expression,sequenceMacro)],
    "$[]" -> [(.expression,indexMacro),(.expression,sliceMacro)],
    "<||>" -> [(.expression,macroQuote)],
    "[||]" -> [(.expression,macroTpl)],
    "(||)" -> [(.expression,macroTpl)],
    "::" -> [(.expression,coercionMacro)],
    ":?" -> [(.expression,coercionMacro)],
    "*" -> [(.expression,multicatMacro)],
    "{}" -> [(.expression,macroComprehension)],
    "{!!}" -> [(.expression,macroIotaComprehension)],
    "{??}" -> [(.expression,macroIterableGoal)],
    "do" -> [(.expression,macroDo),(.actn,forLoopMacro)],
    "action" -> [(.expression,actMacro)],
    "task" -> [(.expression,taskAction)],
    "valof" -> [(.expression,valofMacro)],
    "assert" -> [(.actn,assertMacro)],
    "show" -> [(.actn,showMacro)],
    "try" -> [(.actn,tryCatchMacro)],
    "__pkg__" -> [(.expression,pkgNameMacro)],
    "__loc__" -> [(.expression,locationMacro)],
    "-" -> [(.expression, uMinusMacro),(.pattern, uMinusMacro)],
    "^=" -> [(.expression, optionMatchMacro)],
    "^" -> [(.pattern, optionPtnMacro),(.expression,unwrapMacro)],
    "!" -> [(.expression,binRefMacro)]
  ].

  algebraicMacro(St,.statement,Rp) where (Lc,Vz,Q,Cx,H,R) ^= isAlgebraicTypeStmt(St) => do{
    Rslt <- makeAlgebraic(Lc,Vz,Q,Cx,H,R,Rp);
    valis active(brTuple(Lc,Rslt))
  }
  algebraicMacro(_,_,_) default => do{ valis .inactive}.

  /*
  * We generate auto implementations of fields declared for an algebraic type
  * declaration
  *
  * RT ::= rc{F:FT..}
  *
  * becomes
  *
  * RT <~ {F:FT .. }
  * auto_contract '$F'[RT->>FT] => {.
  *  '$F'(rc(_,..,F,...,_)) => F
  *.}
  *
  * where auto_contract is an implementation statement that automatically 
  * induces a contract -- with the empty prefix
  */

  makeAlgebraic:(locn,visibility,cons[ast],cons[ast],ast,ast,reports) =>
    result[reports,cons[ast]].
  makeAlgebraic(Lc,Vz,Q,Cx,H,R,Rp) => do{
    Nm .= typeName(H);
    (Qs,Xs,Face) <- algebraicFace(R,Q,[],Rp);
    TpExSt .= reveal(reUQuant(Lc,Qs,reConstrain(Cx,binary(Lc,"<~",H,reXQuant(Lc,Xs,brTuple(Lc,sort(Face,compEls)))))),Vz);
    Cons <- buildConstructors(R,Q,Cx,H,Vz,Rp);
    valis [TpExSt,..Cons]
  }

  algebraicFace:(ast,cons[ast],cons[ast],reports) =>
    either[reports,(cons[ast],cons[ast],cons[ast])].
  algebraicFace(A,Qs,Xs,Rp) where (_,L,R) ^= isBinary(A,"|") => do{
    (Q1,X1,Lhs) <- algebraicFace(L,Qs,Xs,Rp);
    (Qx,Xx,Rhs) <- algebraicFace(R,Q1,X1,Rp);
    Fs <- combineFaces(Lhs,Rhs,Rp);
    valis (Qx,Xx,Fs)
  }
  algebraicFace(A,Qs,Xs,Rp) where (Lc,_,_) ^= isRoundTerm(A) =>
    either((Qs,Xs,[])).
  algebraicFace(A,Qs,Xs,Rp) where (Lc,_) ^= isEnum(A) => either((Qs,Xs,[])).
  algebraicFace(A,Qs,Xs,Rp) where (Lc,_,Els) ^= isBrTerm(A) =>
    either((Qs,Xs,Els)).
  algebraicFace(A,Qs,Xs,Rp) where (_,I) ^= isPrivate(A) =>
    algebraicFace(I,Qs,Xs,Rp).
  algebraicFace(A,Qs,Xs,Rp) where (_,I) ^= isPublic(A) =>
    algebraicFace(I,Qs,Xs,Rp).
  algebraicFace(A,Qs,Xs,Rp) where (_,X0,I) ^= isXQuantified(A) =>
    algebraicFace(I,Qs,Xs\/X0,Rp).
  algebraicFace(A,Qs,Xs,Rp) where (_,A0,I) ^= isQuantified(A) =>
    algebraicFace(I,Qs\/A0,Xs,Rp).
  algebraicFace(A,_,_,Rp) default =>
    other(reportError(Rp,"invalid case in algebraic type",locOf(A))).

  combineFaces([],F2,Rp) => either(F2).
  combineFaces(F1,[],Rp) => either(F1).
  combineFaces([F,..Fs],Gs,Rp) where (Lc,Id,_,Tp) ^= isTypeAnnot(F) => do{
    G1 <- mergeField(Lc,Id,Tp,Gs,Rp);
    Fs1 <- combineFaces(Fs,G1,Rp);
    valis [F,..Fs1]
  }

  mergeField(_,_,_,[],_) => either([]).
  mergeField(Lc,Id,Tp,[A,..As],Rp) => do{
    if (Lc2,Id,_,Tp2) ^= isTypeAnnot(A) then {
      if Tp==Tp2 then
	valis As
      else
      raise reportError(Rp,"type associated with $(Id) at $(Lc) incompatible with $(Tp2)",Lc2)
    } else{
      A1 <- mergeField(Lc,Id,Tp,As,Rp);
      valis [A,..A1]
    }
  }

  conArities:(ast,map[string,integer]) => map[string,integer].
  conArities(A,Axx) where (Lc,L,R) ^= isBinary(A,"|") =>
    conArities(L,conArities(R,Axx)).
  conArities(A,Axx) where (_,Nm,_,_,Els) ^= isBraceCon(A) =>
    Axx[Nm->size(Els)].
  conArities(_,Axx) default => Axx.

  buildConIndices:(ast,map[string,map[string,integer]]) => map[string,map[string,integer]].
  buildConIndices(A,Ixx) where (Lc,L,R) ^= isBinary(A,"|") =>
    buildConIndices(L,buildConIndices(R,Ixx)).
  buildConIndices(A,Ixx) where (Lc,Nm,XQs,XCx,Els) ^= isBraceCon(A) =>
    buildConIx(Els,Nm,Ixx).
  buildConIndices(_,Ixx) default => Ixx.

  buildConIx:(cons[ast],string,map[string,map[string,integer]])=>map[string,map[string,integer]].
  buildConIx(Els,Nm,Ixx) =>
    fst(foldLeft((El,(Mp,Ix)) where
	    (_,FNm,_,_) ^= isTypeAnnot(El) =>
	  (Mp[FNm->recordIx(Mp[FNm],Nm,Ix)],Ix+1),(Ixx,0),sort(Els,compEls))).

  recordIx:(option[map[string,integer]],string,integer) => map[string,integer].
  recordIx(some(NIx),Rc,Off) => NIx[Rc->Off].
  recordIx(.none,Rc,Off) => [Rc->Off].

  buildConstructors:(ast,cons[ast],cons[ast],ast,visibility,reports)=>either[reports,cons[ast]].
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,L,R) ^= isBinary(A,"|") => do{
	Dfs1 <- buildConstructors(L,Qs,Cx,Tp,Vz,Rp);
	Dfs2 <- buildConstructors(R,Qs,Cx,Tp,Vz,Rp);
	valis Dfs1++Dfs2
      }.
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,Nm,XQs,XCx,Els) ^= isBraceCon(A) => let{
	Con = typeAnnotation(Lc,nme(Lc,Nm),reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",reXQuant(Lc,XQs,
		  reConstrain(XCx,brTuple(Lc,sort(Els,compEls)))),Tp)))).
      } in either([reveal(Con,Vz)]).
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,Nm,XQs,XCx,Els) ^= isRoundCon(A) => let{
	Con = typeAnnotation(Lc,nme(Lc,Nm),reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",rndTuple(Lc,Els),Tp)))).
      } in either([reveal(Con,Vz)]).
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,Nm) ^= isEnumSymb(A) => let{
	Con = typeAnnotation(Lc,nme(Lc,Nm),reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",rndTuple(Lc,[]),Tp)))).
      } in either([reveal(Con,Vz)]).
  buildConstructors(A,Qs,Cx,Tp,_,Rp) where
      (_,I) ^= isPrivate(A) => 
    buildConstructors(I,Qs,Cx,Tp,.priVate,Rp).
  buildConstructors(A,Qs,Cx,Tp,_,Rp) where
      (_,I) ^= isPublic(A) => 
    buildConstructors(I,Qs,Cx,Tp,.pUblic,Rp).
  buildConstructors(A,_,_,_,_,Rp) =>
    other(reportError(Rp,"cannot fathom constructor $(A)",locOf(A))).

  compEls:(ast,ast)=>boolean.
  compEls(A,B) where
      (_,N1,_,_) ^= isTypeAnnot(A) &&
      (_,N2,_,_) ^= isTypeAnnot(B) => N1<N2.
  compEls(_,_) default => .false.

  reveal(A,.priVate) => unary(locOf(A),"private",A).
  reveal(A,.pUblic) => unary(locOf(A),"public",A).
  reveal(A,_) default => A.

  visibilityOf:(ast) => (ast,visibility).
  visibilityOf(A) => visib(A,.deFault).

  visib(A,_) where (_,I) ^= isPrivate(A) => visib(I,.priVate).
  visib(A,_) where (_,I) ^= isPublic(A) => visib(I,.pUblic).
  visib(A,Vz) default => (A,Vz).

  isBraceCon:(ast) => option[(locn,string,cons[ast],cons[ast],cons[ast])].
  isBraceCon(A) => isCon(A,isBrTerm).

  isRoundCon:(ast) => option[(locn,string,cons[ast],cons[ast],cons[ast])].
  isRoundCon(A) => isCon(A,isRoundTerm).

  isCon:(ast,(ast)=>option[(locn,ast,cons[ast])]) => option[(locn,string,cons[ast],cons[ast],cons[ast])].
  isCon(A,P) where
      (Lc,Nm,Els) ^= P(A) && (_,Id) ^= isName(Nm) => some((Lc,Id,[],[],Els)).
  isCon(A,P) where
      (Lc,Q,I) ^= isXQuantified(A) &&
      (_,Nm,_,Cx,Els) ^= isCon(I,P) =>
    some((Lc,Nm,Q,Cx,Els)).
  isCon(A,P) where
      (Lc,Cx,I) ^= isConstrained(A) &&
      (_,Nm,Q,_,Els) ^= isCon(I,P) =>
    some((Lc,Nm,Q,Cx,Els)).
  isCon(_,_) default => .none.
  
