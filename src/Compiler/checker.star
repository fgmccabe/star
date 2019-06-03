star.compiler.checker{
  import star.

  import star.pkg.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.dependencies.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.typeparse.
  import star.compiler.freshen.
  import star.compiler.unify.
  import star.compiler.wff.

  typeOfPtn:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,"_") ^= isName(A) =>
    either((vr(Lc,genSym("_"),Tp),Env)).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) && Spec ^= isVar(Id,Env) => do{
    if isConstructorType(Tp) then{
      Exp <- typeOfVar(Lc,Id,Tp,Spec,Env,Rp);
      valis (Exp,Env)
    }
      else{
	  WhExp <- typeOfExp(mkWhereEquality(A),Tp,Env,Path,Rp);
	  valis (WhExp,Env)
	}
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) => do{
    Ev = declareVar(Id,some(Lc),Tp,Env);
    valis (vr(Lc,Id,Tp),Ev)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where isLit(A) => do{
    Exp <- typeOfExp(A,Tp,Env,Path,Rp);
    valis (Exp,Env)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where
      (Lc,E,T) ^= isTypeAnnotation(A) => do{
	ETp <- parseType([],T,Env,Rp);
	checkType(E,Tp,ETp,Env,Rp);
	typeOfPtn(E,Tp,Env,Path,Rp)
      }.
  typeOfPtn(A,Tp,Env,Path,Rp) where 
      (Lc,E,C) ^= isWhere(A) => do{
	(Cond,Ev0) <- checkGoal(C,Env,Path,Rp);
	(Ptn,Ev1) <- typeOfPtn(E,Tp,Ev0,Path,Rp);
	valis (whr(Lc,Ptn,Cond),Ev1)
      }.
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isSqTuple(A) =>
    typeOfPtn(macroSquarePtn(Lc,Els),Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && \+ _ ^= isTuple(El) =>
    typeOfPtn(El,Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs = genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Pt,Ex) ^= isOptionPtn(A) =>
    typeOfPtn(mkWherePtn(Lc,Pt,Ex),Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Op,Els) ^= isRoundTerm(A) => do{
    At = newTypeVar("A");
    Fun <- typeOfExp(Op,consType(At,Tp),Env,Path,Rp);
    (Args,Ev) <- typeOfArgPtn(rndTuple(Lc,Els),At,Env,Path,Rp);
    valis (apply(Lc,Fun,Args,Tp),Ev)
  }
  typeOfPtn(A,Tp,_,_,Rp) => other(reportError(Rp,"illegal pattern: $(A), expecting a $(Tp)",locOf(A))).

  typeOfArgPtn:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfArgPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs = genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }

  typeOfPtns:(list[ast],list[tipe],list[canon],dict,string,reports) =>
    either[reports,(list[canon],dict)].
  typeOfPtns([],[],Els,Env,_,_) => either((Els,Env)).
  typeOfPtns([P,..Ps],[T,..Ts],Els,Env,Path,Rp) => do{
    (Pt,E0) <- typeOfPtn(P,T,Env,Path,Rp);
    typeOfPtns(Ps,Ts,[Els..,Pt],E0,Path,Rp)
  }
  
  typeOfExp:(ast,tipe,dict,string,reports) => either[reports,canon].
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) =>
    (Spec ^= isVar(Id,Env) ?
	typeOfVar(Lc,Id,Tp,Spec,Env,Rp) ||
	other(reportError(Rp,"variable $(Id) not defined. Expecting a $(Tp)",Lc))).
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Ix) ^= isInt(A) &&
      (_,IntTp,_) ^= findType(Env,"integer") => do{
	checkType(A,IntTp,Tp,Env,Rp);
	valis intLit(Lc,Ix)
      }
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Dx) ^= isFlt(A) &&
      (_,FltTp,_) ^= findType(Env,"float") => do{
	checkType(A,FltTp,Tp,Env,Rp);
	valis floatLit(Lc,Dx)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Sx) ^= isStr(A) &&
      (_,StrTp,_) ^= findType(Env,"string") => do{
	checkType(A,StrTp,Tp,Env,Rp);
	valis stringLit(Lc,Sx)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,T) ^= isTypeAnnotation(A) => do{
	ETp <- parseType([],T,Env,Rp);
	checkType(E,Tp,ETp,Env,Rp);
	typeOfExp(E,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,T) ^= isCoerce(A) => do{
	typeOfExp(binary(Lc,":",unary(Lc,"_coerce",E),T),Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,C) ^= isWhere(A) => do{
	(Cond,Ev0) <- checkGoal(C,Env,Path,Rp);
	Exp <- typeOfExp(E,Tp,Ev0,Path,Rp);
	valis whr(Lc,Exp,Cond)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isConjunct(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isDisjunct(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
    (Tst,E0) <- checkGoal(T,Env,Path,Rp);
    Thn <- typeOfExp(L,Tp,E0,Path,Rp);
    Els <- typeOfExp(R,Tp,Env,Path,Rp);
    valis cond(Lc,Tst,Thn,Els)
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isNegation(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,R,F) ^= isFieldAcc(A) && (_,Fld) ^= isName(F) => do{
	Rc <- typeOfExp(R,newTypeVar("_r"),Env,Path,Rp);
	typeOfField(Lc,Rc,Fld,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,C,Ix) ^= isIndex(A) =>
    ((_,K,V) ^= isBinary(Ix,"->") ?
	typeOfExp(ternary(Lc,"_put",C,K,V),Tp,Env,Path,Rp) ||
	(_,N) ^= isUnary(Ix,"\\+") ?
	  typeOfExp(binary(Lc,"_remove",C,N),Tp,Env,Path,Rp) ||
	  typeOfExp(binary(Lc,"_index",C,Ix),Tp,Env,Path,Rp)).
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Stmts) ^= isActionTerm(A) &&
      (_,ActionTp,_) ^= findType(Env,"action") => do{
	ErTp = newTypeVar("E");
	XTp = newTypeVar("X");
	checkType(A,tpExp(tpExp(ActionTp,ErTp),XTp),Tp,Env,Rp);
	checkDo(Stmts,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Stmts) ^= isTaskTerm(A) &&
      (_,ActionTp,_) ^= findType(Env,"task") => do{
	ErTp = newTypeVar("E");
	XTp = newTypeVar("X");
	checkType(A,tpExp(tpExp(ActionTp,ErTp),XTp),Tp,Env,Rp);
	checkDo(Stmts,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Stmts) ^= isDoTerm(A)  => 
    checkDo(Stmts,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Exp) ^= isValof(A) =>
    typeOfExp(unary(Lc,"_perform",Exp),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isSqTuple(A) =>
    ((isMapSequence(Els) || isMapType(Tp)) ?
	typeOfExp(macroMapExp(Lc,Els),Tp,Env,Path,Rp) ||
	typeOfExp(macroSquareExp(Lc,Els),Tp,Env,Path,Rp)).
  typeOfExp(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && \+ _ ^= isTuple(El) =>
    typeOfExp(El,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs = genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    Ptns <- typeOfExps(Els,Tvs,[],Env,Path,Rp);
    valis tple(Lc,Ptns)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,B,C) ^= isAbstraction(A) =>
    checkAbstraction(Lc,B,C,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where hasPromotion(A) =>
    typeOfExp(promoteOption(A),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Ar,C,R) ^= isEquation(A) =>
    typeOfLambda(Lc,Ar,C,R,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,I) ^= isUnary(A,"-") =>
    typeOfExp(binary(Lc,"-",lit(Lc,intgr(0)),I),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Op,Args) ^= isRoundTerm(A) =>
    typeOfRoundTerm(Lc,Op,Args,Tp,Env,Path,Rp).

  typeOfExps:(list[ast],list[tipe],list[canon],dict,string,reports) =>
    either[reports,list[canon]].
  typeOfExps([],[],Els,Env,_,_) => either(Els).
  typeOfExps([P,..Ps],[T,..Ts],Els,Env,Path,Rp) => do{
    Trm <- typeOfExp(P,T,Env,Path,Rp);
    typeOfExps(Ps,Ts,[Els..,Trm],Env,Path,Rp)
  }

  typeOfRoundTerm:(locn,ast,list[ast],tipe,dict,string,reports) => either[reports,canon].
  typeOfRoundTerm(Lc,Op,As,Tp,Env,Path,Rp) => do{
    FnTp = newTypeVar("F");
    Vrs = genTpVars(As);
    At = tupleType(Vrs);
    Fun <- typeOfExp(Op,FnTp,Env,Path,Rp);
    if sameType(FnTp,funType(At,Tp),Env) then{
      Args <- typeOfExps(As,Vrs,[],Env,Path,Rp);
      valis apply(Lc,Fun,tple(Lc,Args),Tp)
    } else if sameType(FnTp,consType(At,Tp),Env) then{
      Args <- typeOfExps(As,Vrs,[],Env,Path,Rp);
      valis apply(Lc,Fun,tple(Lc,Args),Tp)
    } else
      throw reportError(Rp,"type of $(Op)\:$(FnTp) not consistent with $(funType(At,Tp))",Lc)
  }

  typeOfArgExp:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfArgExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs = genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }

  checkGoal:(ast,dict,string,reports) => either[reports,(canon,dict)].
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isConjunct(A) => do{
    (Lhs,E0) <- checkGoal(L,Env,Path,Rp);
    (Rhs,E1) <- checkGoal(R,E0,Path,Rp);
    valis (conj(Lc,Lhs,Rhs),E1)
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isDisjunct(A) => do{
    (Lhs,E0) <- checkGoal(L,Env,Path,Rp);
    (Rhs,E1) <- checkGoal(R,Env,Path,Rp);
    valis (disj(Lc,Lhs,Rhs),mergeDict(E0,E1,Env))
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,R) ^= isNegation(A) => do{
    (Rhs,_) <- checkGoal(R,Env,Path,Rp);
    valis (neg(Lc,Rhs),Env)
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
    (Tst,E0) <- checkGoal(T,Env,Path,Rp);
    (Thn,E1) <- checkGoal(L,E0,Path,Rp);
    (Els,E2) <- checkGoal(R,Env,Path,Rp);
    valis (cond(Lc,Tst,Thn,Els),mergeDict(E1,E2,Env))
  }.
  checkGoal(A,Env,Path,Rp) where (_,BoolTp,_) ^= findType(Env,"boolean") => do{
    Exp <- typeOfExp(A,BoolTp,Env,Path,Rp);
    valis (Exp,Env)
  }

  checkDo:(ast,tipe,dict,string,reports) => either[reports,canon].
  checkDo(Stmts,Tp,Env,Path,Rp) => do{
    ElTp = newTypeVar("_e");
    Lc = locOf(Stmts);
    if conDfn(_,_,_,Con) ^= findContract(Env,"execution") then{
      (_,contractExists(typeConstraint(depType(tpExp(Op,StTp),[ErTp])),_)) = freshen(Con,[],Env);
      if sameType(tpExp(StTp,ElTp),Tp,Env) then {
	(Action,_) <- checkAction(Stmts,Env,StTp,ElTp,ErTp,Path,Rp);
	valis act(Lc,Action)
      } else
	throw reportError(Rp,"$(tpExp(StTp,ErTp)) not consistent with expected type $(Tp)",Lc)
    } else
      throw reportError(Rp,"cannot find execution contract",Lc)
  }

  checkAction:(ast,dict,tipe,tipe,tipe,string,reports) =>
    either[reports,(canonAction,dict)].
  checkAction(A,Env,_,_,_,_,_) where (Lc,"nothing") ^= isName(A) => either((noDo(Lc),Env)).
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isActionSeq(A) => do{
    E = newTypeVar("_e");
    (Fst,E0) <- checkAction(L,Env,StTp,E,ErTp,Path,Rp);
    (Snd,E1) <- checkAction(R,E0,StTp,ElTp,ErTp,Path,Rp);
    valis (seqnDo(Lc,Fst,Snd),E1)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isBind(A) => do{
    E = newTypeVar("P");
    HT = tpExp(StTp,E); -- bound terms must be in the same monad
    Exp <- typeOfExp(R,HT,Env,Path,Rp);
    (Ptn,Ev) <- typeOfPtn(L,E,Env,Path,Rp);
    valis (bindDo(Lc,Ptn,Exp,E,StTp,ErTp),Ev)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isDefn(A) => do{
    E = newTypeVar("P");
    (Ptn,Ev) <- typeOfPtn(L,E,Env,Path,Rp);
    Exp <- typeOfExp(R,E,Env,Path,Rp);

    valis (varDo(Lc,Ptn,Exp),Ev)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isAssignment(A) => do{
    Et = newTypeVar("P");
    if (LLc,Coll,[Arg]) ^= isSquareTerm(L) then {
      Lhs <- typeOfExp(Coll,refType(Et),Env,Path,Rp);
      Rhs <- typeOfExp(ternary(LLc,"_put",unary(LLc,"!",Coll),Arg,R),Et,Env,Path,Rp);
      valis (assignDo(Lc,Lhs,Rhs,StTp,ErTp),Env)
    } else{
      Lhs <- typeOfExp(L,refType(Et),Env,Path,Rp);
      Rhs <- typeOfExp(R,Et,Env,Path,Rp);
      valis (assignDo(Lc,Lhs,Rhs,StTp,ErTp),Env)
    }
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,L,R) ^= isIfThenElse(A) => do{
    (Cond,Ev0) <- checkGoal(T,Env,Path,Rp);
    (Th,E1) <- checkAction(L,Env,StTp,ElTp,ErTp,Path,Rp);
    (El,E2) <- checkAction(R,Env,StTp,ElTp,ErTp,Path,Rp);
    valis (ifThenDo(Lc,Cond,Th,El,StTp,ElTp),mergeDict(E1,E2,Env))
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,L) ^= isIfThen(A) => do{
    (Cond,Ev0) <- checkGoal(T,Env,Path,Rp);
    (Th,E1) <- checkAction(L,Env,StTp,ElTp,ErTp,Path,Rp);
    valis (ifThenDo(Lc,Cond,Th,noDo(Lc),StTp,ElTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,B) ^= isWhileDo(A) => do{
    (Cond,Ev0) <- checkGoal(T,Env,Path,Rp);
    (Body,_) <- checkAction(B,Env,StTp,tupleType([]),ErTp,Path,Rp);
    valis (whileDo(Lc,Cond,Body,StTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,B) ^= isForDo(A) => do{
    (Cond,Ev0) <- checkGoal(T,Env,Path,Rp);
    (Body,_) <- checkAction(B,Env,StTp,tupleType([]),ErTp,Path,Rp);
    valis (forDo(Lc,Cond,Body,StTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,B,C) ^= isTryCatch(A) => do{
    (Body,_) <- checkAction(B,Env,StTp,ElTp,ErTp,Path,Rp);
    Hndlr <- checkCatch(C,Env,StTp,ElTp,ErTp,Path,Rp);
    valis (tryCatchDo(Lc,Body,Hndlr,StTp,ElTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,E) ^= isThrow(A) => do{
    Err <- typeOfExp(E,ErTp,Env,Path,Rp);
    valis (throwDo(Lc,Err,StTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,E) ^= isValis(A) => do{
    Rtn <- typeOfExp(E,ElTp,Env,Path,Rp);
    valis (returnDo(Lc,Rtn,StTp,ElTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,[S]) ^= isBrTuple(A) => 
    checkAction(S,Env,StTp,ElTp,ErTp,Path,Rp).

  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) => do{
    Exp <- typeOfExp(A,tpExp(StTp,ElTp),Env,Path,Rp);
    valis (simpleDo(locOf(A),Exp,StTp,ErTp),Env)
  }

  checkCatch:(ast,dict,tipe,tipe,tipe,string,reports) => either[reports,canon].
  checkCatch(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,Stmts) ^= isBrTuple(A) => do{
    HT = funType(tupleType([ErTp]),tpExp(StTp,ElTp));
    (H,_) <- checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp);
    valis lambda(Lc,tple(Lc,[vr(Lc,genSym("_"),ErTp)]),
      enm(Lc,"true",tipe("star.core*boolean")),act(locOf(A),H),HT)
  }
  checkCatch(A,Env,StTp,ElTp,ErTp,Path,Rp) => do{
    HT = funType(tupleType([ErTp]),tpExp(StTp,ElTp));
    typeOfExp(A,HT,Env,Path,Rp)
  }
      
  typeOfVar:(locn,string,tipe,vrEntry,dict,reports) => either[reports,canon].
  typeOfVar(Lc,Nm,Tp,vrEntry(_,Mk,VTp),Env,Rp) => do{
    (_,VrTp) = freshen(VTp,[],Env);
    (MTp,Term) <- manageConstraints(VrTp,[],Lc,Mk(Lc,VrTp),Env,Rp);
    if sameType(Tp,MTp,Env) then {
      valis Term
    } else
      throw reportError(Rp,"$(Nm)\:$(VrTp) not consistent with expected type: $(Tp)",Lc)
  }

  typeOfField:(locn,canon,string,tipe,dict,string,reports) => either[reports,canon].
  typeOfField(Lc,Rc,Fld,Tp,Env,Path,Rp) => do{
    faceType(Flds,_) = faceOfType(typeOf(Rc),Env);
    if (Fld,FldTp) in Flds then{
      (_,VrTp) = freshen(FldTp,[],Env);
      (MTp,Term) <- manageConstraints(VrTp,[],Lc,dot(Lc,Rc,Fld,VrTp),Env,Rp);
      if sameType(Tp,VrTp,Env) then {
	valis Term
      } else
	throw reportError(Rp,"$(Fld)\:$(FldTp) not consistent with expected type: $(Tp)",Lc)
    } else
      throw reportError(Rp,"field $(Fld) is not present in $(Rc)",Lc)
  }

  typeOfLambda:(locn,ast,ast,ast,tipe,dict,string,reports) => either[reports,canon].
  typeOfLambda(Lc,A,C,R,Tp,Env,Path,Rp) => do{
    At = newTypeVar("_A");
    Rt = newTypeVar("_R");
    (As,E0) <- typeOfArgPtn(A,At,Env,Path,Rp);
    (Cond,E1) <- checkGoal(C,E0,Path,Rp);
    Rep <- typeOfExp(R,Rt,E1,Path,Rp);
    checkType(A,funType(At,Rt),Tp,Env,Rp);
    valis lambda(Lc,As,Cond,Rep,Tp)
  }

  manageConstraints:(tipe,list[constraint],locn,canon,dict,reports) =>
    either[reports,(tipe,canon)].
  manageConstraints(constrainedType(Tp,Con),Cons,Lc,Term,Env,Rp)
      where C0 .= applyConstraint(Con,Cons) =>
    manageConstraints(Tp,C0,Lc,Term,Env,Rp).
  manageConstraints(Tp,[],_,Term,Env,_) => either((Tp,Term)).

  applyConstraint(fieldConstraint(T,F),Cons) where
      _ ^= addConstraint(T,fieldConstraint(T,F)) => Cons.
  applyConstraint(typeConstraint(T),Cons) => valof action{
    _ = attachToArgs(T,typeConstraint(T));
    valis [Cons..,typeConstraint(T)]
  }

  attachToArgs(depType(Lhs,_),Con) => attach(Lhs,Con).
  attachToArgs(_,_) => ().
  attach(tpExp(Op,A),Con) where _ ^= addConstraint(A,Con) => attach(Op,Con).
  attach(_,_) => ().
  
  checkType:(ast,tipe,tipe,dict,reports) => either[reports,()].
  checkType(_,Actual,Expected,Env,_) where sameType(Actual,Expected,Env) => either(()).
  checkType(A,ATp,ETp,_,Rp) => other(reportError(
      Rp,"$(A)\:$(ATp) not consistent with expected type $(ETp)",locOf(A))).

  mergeDict:(dict,dict,dict) => dict.
  mergeDict(D1,D2,Env) => let{
    mergeScopes([scope(T1,V1,C1,I1),..Rst],
      [scope(_,V2,_,_),.._]) =>
      [scope(T1,mergeVDefs(V1,V2),C1,I1),..Rst].
    mergeVDefs(V1,V2) => {Nm->E1|Nm->E1 in V1 && Nm->E2 in V2 &&
	  sameDesc(E1,E2)}.
    sameDesc(vrEntry(_,_,T1),vrEntry(_,_,T2)) => sameType(T1,T2,Env)
  } in mergeScopes(D1,D2).

  genTpVars:(list[ast]) => list[tipe].
  genTpVars(Els) => Els//((_)=>newTypeVar("_v")).

  checkAbstraction:(locn,ast,ast,tipe,dict,string,reports) => either[reports,canon].
  checkAbstraction(Lc,B,C,Tp,Env,Path,Rp) where (_,K,V) ^= isBinary(B,"->") => do{
    (Cond,E0) <- checkGoal(C,Env,Path,Rp);
    (ExOp,StTp,KyTp,VlTp) <- pickupIxContract(Lc,Env,"indexed",Rp);
    checkType(B,Tp,StTp,Env,Rp);
    Key <- typeOfExp(K,KyTp,E0,Path,Rp);
    Val <- typeOfExp(V,VlTp,E0,Path,Rp);
    valis ixabstraction(Lc,Key,Val,Cond,Tp)
  }
  checkAbstraction(Lc,B,C,Tp,Env,Path,Rp) => do{
    (Cond,E0) <- checkGoal(C,Env,Path,Rp);
    (ExOp,StTp,BndTp) <- pickupContract(Lc,Env,"sequence",Rp);
    checkType(B,Tp,StTp,Env,Rp);
    Bnd <- typeOfExp(B,BndTp,E0,Path,Rp);
    valis abstraction(Lc,Bnd,Cond,Tp)
  }

  pickupContract:(locn,dict,string,reports) => either[reports,(tipe,tipe,tipe)].
  pickupContract(Lc,Env,Nm,Rp) => do{
    if conDfn(_,_,_,Con) ^= findContract(Env,Nm) then{
      (_,contractExists(typeConstraint(depType(tpExp(Op,StTp),[ErTp])),_)) =
	freshen(Con,[],Env);
      valis (Op,StTp,ErTp)
    } else
      throw reportError(Rp,"$(Nm) contract not defined",Lc)
  }

  pickupIxContract:(locn,dict,string,reports) => either[reports,(tipe,tipe,tipe,tipe)].
  pickupIxContract(Lc,Env,Nm,Rp) => do{
    if conDfn(_,_,_,Con) ^= findContract(Env,Nm) then{
      (_,contractExists(typeConstraint(depType(tpExp(Op,IxTp),[KyTp,VlTp])),_)) =
	freshen(Con,[],Env);
      valis (Op,IxTp,KyTp,VlTp)
    } else
      throw reportError(Rp,"$(Nm) contract not defined",Lc)
  }


}
