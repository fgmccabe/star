star.compiler.constraints{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.types.
  import star.compiler.unify.

  public consRecord ::= conCon(canon,constraint,option[canon])
    | impliedCons(cons[canonDef],cons[consRecord]).

  public implementation display[consRecord] => let{
    dispCon(conCon(Src,Con,Witness)) =>
      ssSeq([disp(Con),ss("|:"),disp(Src),ss(" witness "),disp(Witness)]).
    dispCon(impliedCons(Dfs,Cons)) =>
      ssSeq([disp(Dfs),ss("=>"),ss("["),ssSeq(interleave(Cons//dispCon,ss(";"))),ss("]")])
  } in {.
    disp = dispCon
  .}

  public collectEnvConstraints:(cons[cons[canonDef]]) => cons[consRecord].
  collectEnvConstraints(Gps) =>
    foldLeft((Gp,Cx)=>foldLeft(collectDefCons,Cx,Gp),[],Gps).

  resolveContract:(locn,tipe,dict,reports) => either[reports,option[(canon,cons[consRecord])]].
  resolveContract(Lc,Tp,Dict,Rp) => do{
    ImpNm .= implementationName(Tp);
    logMsg("looking for implementation $(Tp) - $(ImpNm)");
    if Impl^=findVar(Lc,ImpNm,Dict) then {
      logMsg("we have implementation $(Impl)\:$(typeOf(Impl))");
      if sameType(typeOf(Impl),Tp,Dict) then {
	logMsg("we found implementation $(Impl)");
	valis some((Impl,collectConstraints(Impl,[])))
      } else{
	throw reportError(Rp,"implementation $(typeOf(Impl)) not consistent with $(Tp)",Lc)
      }
    } else{
      valis .none
    }
  }

  resolveConstraints:(cons[consRecord],dict,reports) => either[reports,(cons[consRecord],cons[consRecord])].
  resolveConstraints(Cons,Dict,Rp) =>
    resolveCons(Cons,Dict,[],[],Rp).
  
  resolveCons([],_,Rem,Done,_) => either((Rem,Done)).
  resolveCons([Con,..Cns],Dict,Rem,Done,Rp) => do{
    Rslt <- resolveConstraint(Con,Dict,Rp);
    if (Cn,Extra) ^= Rslt then
      resolveCons(Cns,Dict,Extra++Rem,[Con,..Done],Rp)
    else
    resolveCons(Cns,Dict,[Con,..Rem],Done,Rp)
  }
      
  resolveConstraint:(consRecord,dict,reports) =>
    either[reports,option[(consRecord,cons[consRecord])]].
  resolveConstraint(conCon(Term,Con,some(W)),_,_) =>
    either(some((conCon(Term,Con,some(W)),[]))).
  resolveConstraint(conCon(Term,contractConstraint(Tp),.none),Dict,Rp) => do{
    ImpNm .= implementationName(Tp);
    Lc .= locOf(Term);
    logMsg("looking for implementation $(Tp) - $(ImpNm)");
    if Impl^=findVar(Lc,ImpNm,Dict) then {
      logMsg("we have implementation $(Impl)\:$(typeOf(Impl))");
      if sameType(typeOf(Impl),Tp,Dict) then {
	logMsg("we found implementation $(Impl)");
	valis some((conCon(Term,contractConstraint(Tp),some(Impl)),collectConstraints(Impl,[])))
      } else{
	throw reportError(Rp,"implementation $(typeOf(Impl)) not consistent with $(Tp)",Lc)
      }
    } else{
      valis .none
    }
  }
  resolveConstraint(impliedCons(Dfs,Cns),Dict,Rp) => do{
    TDict .= declareImplementationsInGroup(Dfs,Dict);
    (Clft,Cxx) <- resolveConstraints(Cns,TDict,Rp);
    valis some((impliedCons(Dfs,Clft),Cxx))
  }

  declareImplementationsInGroup:(cons[canonDef],dict) => dict.
  declareImplementationsInGroup([],Dict) => Dict.
  declareImplementationsInGroup([implDef(Lc,_,FullNm,_,_,Tp),..Gp],Dict) =>
    declareImplementationsInGroup(Gp,
       declareVar(FullNm,some(Lc),Tp,
	declareImplementation(FullNm,Tp,Dict))).
  declareImplementationsInGroup([_,..Gp],Dict) => declareImplementationsInGroup(Gp,Dict).
  
  

  collectConstraints:(canon,cons[consRecord])=>cons[consRecord].
  collectConstraints(vr(_,_,_),Cons) => Cons.
  collectConstraints(intr(_,_),Cons) => Cons.
  collectConstraints(flt(_,_),Cons) => Cons.
  collectConstraints(strng(_,_),Cons) => Cons.
  collectConstraints(enm(_,_,_),Cons) => Cons.
  collectConstraints(mtd(Lc,Nm,Con,Tp),Cons) =>
    [conCon(mtd(Lc,Nm,Con,Tp),contractConstraint(Con),.none),..Cons].
  collectConstraints(over(Lc,T,Tp,Cns),Cons) =>
    foldLeft((C,Cx)=>[conCon(over(Lc,T,Tp,Cns),C,.none),..Cx],Cons,Cns).
  collectConstraints(whr(_,L,R),Cons) =>
    collectConstraints(L,collectConstraints(R,Cons)).
  collectConstraints(match(_,L,R),Cons) =>
    collectConstraints(L,collectConstraints(R,Cons)).
  collectConstraints(conj(_,L,R),Cons) =>
    collectConstraints(L,collectConstraints(R,Cons)).
  collectConstraints(disj(_,L,R),Cons) =>
    collectConstraints(L,collectConstraints(R,Cons)).
  collectConstraints(implies(_,L,R),Cons) =>
    collectConstraints(L,collectConstraints(R,Cons)).
  collectConstraints(neg(_,L),Cons) =>
    collectConstraints(L,Cons).
  collectConstraints(dot(_,L,_,_),Cons) =>
    collectConstraints(L,Cons).
  collectConstraints(serch(_,L,R,G),Cons) =>
    collectConstraints(G,collectConstraints(L,collectConstraints(R,Cons))).
  collectConstraints(cond(_,T,L,R),Cons) =>
    collectConstraints(R,collectConstraints(L,collectConstraints(T,Cons))).
  collectConstraints(update(_,L,R),Cons) =>
    collectConstraints(L,collectConstraints(R,Cons)).
  collectConstraints(tple(_,Els),Cons) =>
    foldLeft(collectConstraints,Cons,Els).
  collectConstraints(apply(_,L,R,_),Cons) =>
    collectConstraints(L,collectConstraints(R,Cons)).
  collectConstraints(csexp(_,E,Cs,_),Cons) =>
    foldLeft(collectEqnCons,collectConstraints(E,Cons),Cs).
  collectConstraints(lambda(Es,_),Cons) =>
    foldLeft(collectEqnCons,Cons,Es).
  collectConstraints(letExp(_,Ds,E),Cons) =>
    [impliedCons(Ds,foldLeft(collectDefCons,collectConstraints(E,[]),Ds)),..Cons].
  collectConstraints(letRec(_,Ds,E),Cons) =>
    [impliedCons(Ds,foldLeft(collectDefCons,collectConstraints(E,[]),Ds)),..Cons].
  collectConstraints(record(_,_,Els,_),Cons) =>
    foldLeft(((_,V),Cx) => collectConstraints(V,Cx),Cons,Els).

  collectEqnCons(eqn(_,Arg,.none,Val),Cons) =>
    collectConstraints(Arg,collectConstraints(Val,Cons)).
  collectEqnCons(eqn(_,Arg,some(C),Val),Cons) =>
    collectConstraints(C,collectConstraints(Arg,collectConstraints(Val,Cons))).

  collectDefCons(varDef(Lc,_,Nm,Val,Cns,Tp),Cons) =>
    collectConstraints(Val,foldLeft((C,Cx)=>[conCon(vr(Lc,Nm,Tp),C,.none),..Cx],Cons,Cns)).
  collectDefCons(implDef(Lc,Nm,FullNm,Val,Cns,Tp),Cons) =>
    collectDefCons(varDef(Lc,FullNm,FullNm,Val,Cns,Tp),Cons).
  collectDefCons(typeDef(_,_,_,_),Cons) => Cons.
  collectDefCons(conDef(_,_,_,_),Cons) => Cons.
  collectDefCons(cnsDef(_,_,_,_),Cons) => Cons.
}
  
