star.compiler.constraints{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.freshen.
  import star.compiler.unify.

  cnsCheck ::= check(locn,option[canon],dict,tipe).

  implementation display[cnsCheck] => {
    disp(check(Lc,.none,_,Tp)) =>
      "unresolved contraint: $(Tp) at $(Lc)".
    disp(check(Lc,some(T),_,Tp)) =>
      "resolved contraint: $(Tp)|:$(T)  at $(Lc)".
  }
  
  extractTermConstraints:(canon,dict,cons[cnsCheck])=>cons[cnsCheck].
  extractTermConstraints(vr(Lc,Nm,Tp),_,Cnx) => Cnx.
  extractTermConstraints(intr(Lc,Ix),_,Cnx) => Cnx.
  extractTermConstraints(flt(Lc,Dx),_,Cnx) => Cnx.
  extractTermConstraints(strng(_,_),_,Cnx) => Cnx.
  extractTermConstraints(enm(_,FullNm,Tp),_,Cnx) => Cnx.
  extractTermConstraints(dot(_,Rc,_,_),Dict,Cnx) =>
    extractTermConstraints(Rc,Dict,Cnx).
  extractTermConstraints(whr(_,T,C),Dict,Cnx) =>
    extractTermConstraints(T,Dict,extractTermConstraints(C,Dict,Cnx)).
  extractTermConstraints(mtd(Lc,Nm,Con,Tp),Dict,Cnx) => [check(Lc,.none,Dict,Con),..Cnx].
  extractTermConstraints(over(Lc,T,[Cn]),Dict,Cnx) =>
    extractConstraint(Cn,Lc,Dict,Cnx).
  extractTermConstraints(apply(lc,Op,Arg,_),Dict,Cnx) =>
    extractTermConstraints(Arg,Dict,extractTermConstraints(Op,Dict,Cnx)).
  extractTermConstraints(tple(Lc,Els),Dict,Cnx) =>
    foldLeft((Cn,Cx)=>extractTermConstraints(Cn,Dict,Cx),Cnx,Els).
  extractTermConstraints(match(Lc,Ptn,Src),Dict,Cnx) =>
    extractTermConstraints(Ptn,Dict,extractTermConstraints(Src,Dict,Cnx)).
  extractTermConstraints(conj(Lc,Lhs,Rhs),Dict,Cnx) => 
    extractTermConstraints(Rhs,Dict,extractTermConstraints(Lhs,Dict,Cnx)).
  extractTermConstraints(disj(Lc,Lhs,Rhs),Dict,Cnx) => 
    extractTermConstraints(Rhs,Dict,extractTermConstraints(Lhs,Dict,Cnx)).
  extractTermConstraints(implies(Lc,Lhs,Rhs),Dict,Cnx) =>
    extractTermConstraints(Rhs,Dict,extractTermConstraints(Lhs,Dict,Cnx)).
  extractTermConstraints(neg(Lc,Rhs),Dict,Cnx) =>
    extractTermConstraints(Rhs,Dict,Cnx).
  extractTermConstraints(cond(Lc,Tst,Lhs,Rhs),Dict,Cnx) =>
    extractTermConstraints(Tst,Dict,extractTermConstraints(Lhs,Dict,
	extractTermConstraints(Rhs,Dict,Cnx))).
  extractTermConstraints(lambda(_,Rls,_),Dict,Cnx) =>
    foldLeft((Eqn,Cs)=>extractEqnConstraints(Eqn,Dict,Cs),Cnx,Rls).
  extractTermConstraints(letExp(Lc,Gp,Rhs),Dict,Cnx) => valof action{
    DD .= declareImplementationsInGroup(Gp,Dict);
    valis foldLeft((Df,Cs)=>extractDefConstraints(Df,DD,Cs),
      extractTermConstraints(Rhs,DD,Cnx),Gp)
  }
  extractTermConstraints(letRec(Lc,Gp,Rhs),Dict,Cnx) => valof action{
    DD .= declareImplementationsInGroup(Gp,Dict);
    valis foldRight((Df,Cs)=>extractDefConstraints(Df,DD,Cs),
      extractTermConstraints(Rhs,DD,Cnx),Gp)
  }
  extractTermConstraints(csexp(Lc,Gov,Cases,Tp),Dict,Cnx) =>
    foldLeft((Eqn,Cs)=>extractEqnConstraints(Eqn,Dict,Cs),
      extractTermConstraints(Gov,Dict,Cnx),Cases).
  extractTermConstraints(record(Lc,Nm,Fields,Tp),Dict,Cnx) =>
    foldLeft(((_,T),Cs) => extractTermConstraints(T,Dict,Cs),
      Cnx,Fields).
  extractTermConstraints(update(_,Lhs,Rhs),Dict,Cnx) =>
    extractTermConstraints(Rhs,Dict,extractTermConstraints(Lhs,Dict,Cnx)).

  extractEqnConstraints:(equation,dict,cons[cnsCheck])=>cons[cnsCheck].
  extractEqnConstraints(eqn(_,A,.none,R),Dict,Cnx) =>
    extractTermConstraints(A,Dict,extractTermConstraints(R,Dict,Cnx)).
  extractEqnConstraints(eqn(_,A,some(C),R),Dict,Cnx) =>
    extractTermConstraints(A,Dict,
      extractTermConstraints(R,Dict,
	extractTermConstraints(C,Dict,Cnx))).

  extractFieldConstraints((_,T),Dict,Cnx) => extractTermConstraints(T,Dict,Cnx).

  extractDefConstraints(varDef(Lc,_,_,T,Cx,_),Dict,Cnx) => valof action{
    DD .= defineCVars(Lc,Cx,Dict);
    valis foldRight((Cn,Cons) => extractConstraint(Cn,Lc,DD,Cons),
      extractTermConstraints(T,DD,Cnx),Cx)
  }
  extractDefConstraints(implDef(Lc,_,_,T,Cx,_),Dict,Cnx) => valof action{
    DD .= defineCVars(Lc,Cx,Dict);
    valis foldRight((Cn,Cons) => extractConstraint(Cn,Lc,DD,Cons),
      extractTermConstraints(T,DD,Cnx),Cx)
  }
  extractDefConstraints(typeDef(_,_,_,_),_,Cnx) => Cnx.
  extractDefConstraints(conDef(_,_,_,_),_,Cnx) => Cnx.
  extractDefConstraints(cnsDef(_,_,_,_),_,Cnx) => Cnx.

  extractConstraint:(constraint,locn,dict,cons[cnsCheck]) => cons[cnsCheck].
  extractConstraint(conTract(C),Lc,Dict,Cnx) =>
    [check(Lc,.none,Dict,C),..Cnx].
  extractConstraint(_,_,_,Cnx) default => Cnx.

  declareImplementations([],Dict) => Dict.
  declareImplementations([Gp,..Gps],Dict) =>
    declareImplementations(Gps,declareImplementationsInGroup(Gp,Dict)).

  declareImplementationsInGroup:(cons[canonDef],dict) => dict.
  declareImplementationsInGroup([],Dict) => Dict.
  declareImplementationsInGroup([implDef(Lc,_,FullNm,_,_,Tp),..Gp],Dict) =>
    declareImplementationsInGroup(Gp,
      declareVar(FullNm,some(Lc),Tp,.none,
	declareImplementation(some(Lc),FullNm,FullNm,Tp,Dict))).
  declareImplementationsInGroup([typeDef(Lc,Nm,Tp,TpRl),..Gp],Dict) =>
    declareImplementationsInGroup(Gp,declareType(Nm,some(Lc),Tp,TpRl,Dict)).
  declareImplementationsInGroup([_,..Gp],Dict) => declareImplementationsInGroup(Gp,Dict).

  defineCVars:(locn,cons[constraint],dict) => dict.
  defineCVars(_,[],D) => D.
  defineCVars(Lc,[conTract(T),..Tps],D)
      where TpNm .= implementationName(T) =>
    defineCVars(Lc,Tps,declareVar(TpNm,some(Lc),T,.none,D)).
    
  resolveConstraint:(cnsCheck,reports) =>
    either[reports,(option[cnsCheck],cons[cnsCheck])].
  resolveConstraint(check(Lc,some(T),Dict,Tp),Rp) => do{
    valis (some(check(Lc,some(T),Dict,Tp)),[])
  }.
  resolveConstraint(check(Lc,.none,Dict,Tp),Rp) => do{
--    logMsg("trying to resolve constraint $(check(Lc,.none,Dict,Tp))");
    ImpNm .= implementationName(Tp);
--    logMsg("looking for implementation $(Tp) - $(ImpNm)");
    if Impl^=findVar(Lc,ImpNm,Dict) then {
--      logMsg("we have implementation $(Impl)\:$(typeOf(Impl))");
      if sameType(typeOf(Impl),Tp,Dict) then {
	valis (some(check(Lc,some(Impl),Dict,Tp)),extractTermConstraints(Impl,Dict,[])).
      } else{
	raise reportError(Rp,"implementation $(typeOf(Impl)) not consistent with $(Tp)",Lc)
      }
    }
    else
    valis (.none,[check(Lc,.none,Dict,Tp)])
  }

  solvePass:(cons[cnsCheck],cons[cnsCheck],cons[cnsCheck],reports) =>
    either[reports,(cons[cnsCheck],cons[cnsCheck])].
  solvePass([],So,Done,_) => either((So,Done)).
  solvePass([C,..Cnx],So,Done,Rp) => do{
    (Cn,More) <- resolveConstraint(C,Rp);
--    logMsg("result of looking at constraint $(C) is $(Cn)");
    if Solved^=Cn then
      solvePass(More++Cnx,So,[Solved,..Done],Rp)
    else{
      solvePass(Cnx,[C,..So],Done,Rp)
    }
  }

  mergeLocs:(cons[locn]) => locn.
  mergeLocs([L,..Ls]) => foldLeft(mergeLoc,L,Ls).

  multiPass:(cons[cnsCheck],
    cons[cnsCheck],integer,reports) => either[reports,cons[cnsCheck]].
  multiPass([],Done,_,_) => either(Done).
  multiPass(Cnx,_,0,Rp) =>
    other(reportError(Rp,"constraints too complex, remaining constraints $(Cnx)",
	mergeLocs(Cnx//(check(Lc,_,_,_))=>Lc))).
  multiPass(Cns,Done,Ix,Rp) => do{
    (More,DD) <- solvePass(Cns,[],Done,Rp);
--    logMsg("$(size(More)) remaining constraints");
    multiPass(More,DD,Ix-1,Rp)
  }

  public resolveConstraints:(cons[cons[canonDef]],dict,reports) =>
    either[reports,()].
  resolveConstraints(Defs,Dict,Rp) => do{
--    logMsg("get all constraints of $(Defs)");
    TDict .= declareImplementations(Defs,Dict);
    Cnx .= foldRight((Gp,C) => foldRight(
	(Df,C1)=>extractDefConstraints(Df,TDict,C1),C,Gp),[],Defs);
--    logMsg("all constraints $(Cnx)");

--    logMsg("$(size(Cnx)) constraints to solve");
    Done <- multiPass(Cnx,[],8,Rp);
--    logMsg("after 4 passes: Done=$(Done)");
    valis ()
  }
}
