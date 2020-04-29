star.compiler.normalize{
  import star.
  import star.pkg.

  import star.compiler.canon.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.freevars.
  import star.compiler.intrinsics.
  import star.compiler.matcher.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  nameMapEntry ::= moduleFun(string,crExp,tipe)
    | moduleVar(crExp)
    | localFun(string,tipe,crExp)
    | localVar(crVar)
    | moduleCons(string,tipe)
    | labelArg(crVar,integer,tipe)
    | globalVar(string,tipe)
    | mapAccess((locn)=>crExp).

  mapLayer ::= lyr(string,map[string,nameMapEntry]).

  nameMap ~> cons[mapLayer].

  implementation display[mapLayer] => {.
    disp(lyr(Path,Entries)) =>
      ssSeq([disp(Path),ss(":"),disp(Entries),ss("\n")])
  .}

  implementation display[nameMapEntry] => {.
    disp(moduleFun(V,C,Tp)) => ssSeq([ss("module var "),disp(V),ss(", closure "),disp(C)]).
    disp(moduleVar(Vr)) => ssSeq([ss("module var"),disp(Vr)]).
    disp(moduleCons(Nm,Tp)) => ssSeq([ss("module cons "),ss(Nm)]).
    disp(localFun(Nm,Tp,V)) => ssSeq([ss("local fun "),ss(Nm)]).
    disp(localVar(Vr)) => ssSeq([ss("local var"),disp(Vr)]).
    disp(labelArg(Base,Ix,Tp)) => ssSeq([ss("label arg"),disp(Base),ss("["),disp(Ix),ss("]")]).
    disp(mapAccess(_)) => ss("map fun").
    disp(globalVar(Nm,Tp)) => ssSeq([ss("global "),ss(Nm)]).
  .}

  crFlow ~> (crExp,cons[crDefn]).

  public normalize:(pkgSpec,canonDef,reports)=>either[reports,cons[crDefn]].
  normalize(PkgSpec,Prg,Rp) => do{
    Map .= pkgMap(PkgSpec);
--    logMsg("package map $(Map)");
    (V,Defs) <- transformDef(Prg,Map,.none,[],[],Rp);
--    logMsg("raw transformed def: $(V)");
    Bound .= (V//((Vr,Vl))=>vrDef(locOf(Vl),Vr,Vl));
    valis Bound++Defs
  }

  pkgMap:(pkgSpec) => nameMap.
  pkgMap(pkgSpec(Pkg,Imports,Tp,Cons,Impls,PkgVrs)) =>
    [lyr(pkgName(Pkg),foldRight(((Nm,ITp),D)=>D[Nm->globalVar(Nm,ITp)],[],PkgVrs))].

  groupMap:(locn,cons[canonDef],string,nameMap,reports) => either[reports,(nameMap,option[crExp],crExp,cons[(crVar,crExp)])].
  groupMap(Lc,Grp,Prefix,Outer,Rp) => do{
--    logMsg("making map for group $(Grp)");
--    logMsg("outer map $(head(Outer))");
    rawGrpFree .= freeVarsInGroup(Grp)::cons[crVar];
--    logMsg("raw group free $(rawGrpFree)");
    freeVars .=
      foldRight((crId(Nm,Tp),So) => (_ ^= lookup(Outer,Nm,isModule) ? So || _addMem(crId(Nm,Tp),So)),[],
	rawGrpFree);
--    logMsg("group free vars $(freeVars)");

    if isEmpty(freeVars) then {
      M .= [lyr("",foldRight((D,LL)=>collectMtd(D,.none,LL),[],Grp)),..Outer];
--      logMsg("new group map $(head(M))");
      valis (M,.none,crTpl(Lc,[]),[])
    } else if [FrVr].=freeVars then {
      ThVr <- liftVarExp(Lc,crName(FrVr),typeOf(FrVr),Outer,Rp);
      L .= [crName(FrVr)->localVar(FrVr)]; -- protect against looking up more
      M .= [lyr("",foldRight((D,LL)=>collectMtd(D,some(crVar(Lc,FrVr)),LL),L,Grp)),..Outer];
--      logMsg("new group map $(head(M))\nfree var $(ThVr)");
      valis (M,some(crVar(Lc,FrVr)),ThVr,[])
    }
    else {
      ThV .= genVar("_ThVr",typeOf(freeVars));
--      logMsg("theta var is $(ThV)");
      ThVr .= some(crVar(Lc,ThV));
      L .= collectLabelVars(freeVars,ThV,0,[]);
--      logMsg("label vars $(L)");
    
      M .= [lyr(Prefix,foldRight((D,LL)=>collectMtd(D,ThVr,LL),L,Grp)),..Outer];
--      logMsg("new group map $(head(M))");

      freeArgs <- seqmap((crId(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer,Rp),freeVars);
--      logMsg("free term $(crTpl(Lc,freeArgs))");
      valis (M,ThVr,crTpl(Lc,freeArgs),[(ThV,crTpl(Lc,freeArgs))])
    }
  }

  lambdaMap:(canon,nameMap,reports) => either[reports,(nameMap,crExp,crExp)].
  lambdaMap(Lam,Outer,Rp) => do{
--    logMsg("making map for lambda $(Lam)");
    freeVars .= freeVarsInTerm(Lam,[],[])::cons[crVar];
--    logMsg("initial free $(freeVars)");

    Lc .= locOf(Lam);
    FullNm .= genSym(layerName(Outer)++"λ");
--    logMsg("free vars in lambda = $(freeVars), full name $(FullNm)");

    if [FrVr].=freeVars then{
      ThVr <- liftVarExp(Lc,crName(FrVr),typeOf(FrVr),Outer,Rp);
      L .= [crName(FrVr)->localVar(FrVr)]; -- protect against looking up more
      M .= [lyr(FullNm,L),..Outer];
--      logMsg("free var = $(ThVr)");
      valis (M,crVar(Lc,FrVr),ThVr)
    } else {
      ThV .= genVar("_ThVr",typeOf(freeVars));
--      logMsg("theta var is $(ThV)");
      freeArgs <- seqmap((crId(Nm,Tp))=>liftVarExp(Lc,Nm,Tp,Outer,Rp),freeVars);
      L .= collectLabelVars(freeVars,ThV,0,[]);
--      logMsg("lambde label vars $(L)");
--      logMsg("lambda freeterm = $(crTpl(Lc,freeArgs))");
      M .= [lyr(FullNm,L),..Outer];
      valis (M,crVar(Lc,ThV),crTpl(Lc,freeArgs))
    }
  }

  codeGroup([varDef(_,_,_,_,_,_),.._]) => .true.
  codeGroup([implDef(_,_,_,_,_,_),.._]) => .true.
  codeGroup([_,..G])=>codeGroup(G).
  codeGroup([])=>.false.
  
  liftLetExp:(locn,cons[canonDef],canon,nameMap,cons[crDefn],reports) => either[reports,crFlow].
  liftLetExp(Lc,Defs,Bnd,Map,Ex,Rp) where !codeGroup(Defs) => liftExp(Bnd,Map,Ex,Rp).
  liftLetExp(Lc,Defs,Bnd,Map,Ex,Rp) => do{
--    logMsg("let $(Lc)");
    (GMap,GrpVr,GrpFree,IVs) <- groupMap(Lc,Defs,layerName(Map),Map,Rp);
--    logMsg("group map $(head(GMap))");
--    logMsg("free term $(GrpVr) = $(GrpFree)");
    (Vs,Ex1,BMap) <- transformGroup(Defs,GMap,GrpVr,Ex,[],Rp);
--    logMsg("let Vs=$(Vs)");

--    logMsg("lifting bound exp $(Bnd)");
    (BndTrm,Ex2) <- liftExp(Bnd,BMap,Ex1,Rp);
--    logMsg("Bound exp s=$(BndTrm)");

    Bound .= foldRight(((Vr,Vl),X)=>
	((crVar(_,VV).=Vl && VV==Vr) ? X || crLtt(locOf(Vl),Vr,Vl,X)),BndTrm,IVs++Vs);

    if crVar(_,GrpThVr) ^= GrpVr && !(crVar(_,GrpThVr).=GrpFree) then
      valis (crLtt(Lc,GrpThVr,GrpFree,Bound),Ex2)
    else
    valis (Bound,Ex2)
  }

  liftLetRec:(locn,cons[canonDef],canon,nameMap,cons[crDefn],reports) => either[reports,crFlow].
  liftLetRec(Lc,Defs,Bnd,Map,Ex,Rp) where !codeGroup(Defs) => liftExp(Bnd,Map,Ex,Rp).
  liftLetRec(Lc,Defs,Bnd,Map,Ex,Rp) => do{
--    logMsg("lift let rec $(Defs) in $(Bnd)");
    (GMap,GrpVr,GrpFree,IVs) <- groupMap(Lc,Defs,layerName(Map),Map,Rp);
--    logMsg("group map $(head(GMap))");
--    logMsg("free term $(GrpVr) = $(GrpFree)");

    (Vs,Ex1,BMap) <- transformGroup(Defs,GMap,GrpVr,Ex,[],Rp);

--    logMsg("let rec Vs=$(Vs)");

    (BndTrm,Ex2) <- liftExp(Bnd,BMap,Ex1,Rp);

    Bound .= foldRight(((Vr,Vl),X)=>
	(crVar(_,VV).=Vl && VV==Vr ? X || crLtRec(locOf(Vl),Vr,Vl,X)),BndTrm,IVs++Vs);

--    logMsg("Bound let rec for $(Defs) = $(Bound)");
    valis (Bound,Ex2)
  }

  isFunctions:(cons[canonDef])=>boolean.
  isFunctions(Defs) => varDef(_,_,_,Vl,_,_) in Defs *> isFunDef(Vl).

  transformGroup:(cons[canonDef],nameMap,option[crExp],cons[crDefn],cons[(crVar,crExp)],reports) => either[reports,(cons[(crVar,crExp)],cons[crDefn],nameMap)].
  transformGroup([],Map,_,D,Vs,_) => either((Vs,D,Map)).
  transformGroup([D,..Ds],Map,Extra,Ex,V,Rp) => do {
    (V1,Ex1) <- transformDef(D,Map,Extra,[],V,Rp);
--    logMsg("transformed def is $(V1)\:$(Ex1)");
    transformGroup(Ds,Map,Extra,Ex++Ex1,V1,Rp)
  }

  transformDef:(canonDef,nameMap,option[crExp],cons[crDefn],cons[(crVar,crExp)],reports) =>
    either[reports,(cons[(crVar,crExp)],cons[crDefn])].
  transformDef(varDef(Lc,Nm,FullNm,lambda(Eqns,Tp),_,_),Map,Extra,Ex,Vs,Rp) => do{
--    logMsg("transform function $(Nm) - $(Eqns)");
--    logMsg("function map $(Map)");
--    logMsg("extra vars in function: $(Nm) are $(Extra)");
    ATp .= extendFunTp(deRef(Tp),Extra);
    (Eqs,Ex1) <- transformEquations(Eqns,Map,Extra,Ex,Rp);
--    logMsg("function equs $(Eqs)");
    Func .= functionMatcher(Lc,FullNm,ATp,Eqs);
--    logMsg("transformed function $(Func)");
--    logMsg("extra defs for $(FullNm)\: $(front(Ex1,size(Ex1)-size(Ex)))");

    ClosureNm .= closureNm(FullNm);
    ClVar .= (crVar(_,Exv)^=Extra ? Exv || crId("_",unitTp));
    ClVars .= makeFunVars(Tp);
    ClArgs .= [ClVar,..ClVars];

--    logMsg("ClArgs = $(ClArgs)");

    ClosTp .= extendFunTp(deRef(Tp),some(ClVar));

    if Exv^=Extra then {
--      logMsg("extra $(Exv)");
      ClosEntry .=
	fnDef(Lc,ClosureNm,ClosTp,ClArgs,
	  crCall(Lc,FullNm,ClArgs//(V)=>crVar(Lc,V),funTypeRes(Tp)));
--      logMsg("closure entry for $(FullNm) : $(ClosEntry)");
      valis ([(crId(Nm,Tp),crTerm(Lc,ClosureNm,[Exv],Tp)),..Vs],[Func,ClosEntry,..Ex1])
    } else {
      ClosEntry .=
	fnDef(Lc,ClosureNm,ClosTp,
	  ClArgs,crCall(Lc,FullNm,ClVars//(V)=>crVar(Lc,V),funTypeRes(Tp)));

--      logMsg("closure for $(FullNm) is $(ClosEntry)");

      valis (Vs,[Func,ClosEntry,..Ex1])
    }
  }

  transformDef(varDef(Lc,Nm,FullNm,Val,_,Tp),Map,_,Ex,Vs,Rp) => do{
    (Vl,Exx) <- liftExp(Val,Map,Ex,Rp);
    Vr .= crId(Nm,Tp);
--    logMsg("var $(Nm)\[$(FullNm)] lifts to $(Vl)");

    valis ([(Vr,Vl),..Vs],Exx)
  }
  transformDef(implDef(Lc,_,FullNm,Val,Cx,Tp),Map,Extra,Ex,Vs,Rp) => do{
--    logMsg("transform implementation $(FullNm) = $(Val)");
    transformDef(varDef(Lc,FullNm,FullNm,Val,Cx,Tp),Map,Extra,Ex,Vs,Rp)
  }.
  transformDef(_,_,_,Ex,Vs,_) => either((Vs,Ex)).

  transformEquations:(cons[equation],nameMap,option[crExp],cons[crDefn],reports) =>
    either[reports,(cons[(locn,cons[crExp],option[crExp],crExp)],cons[crDefn])].
  transformEquations([],_,_,Ex,_) => either(([],Ex)).
  transformEquations([Eqn,..Eqns],Map,Extra,Ex,Rp) => do{
    (Trple,Ex1) <- transformEquation(Eqn,Map,Extra,Ex,Rp);
    (Rest,Exx) <- transformEquations(Eqns,Map,Extra,Ex1,Rp);
    valis ([Trple,..Rest],Exx)
  }

  addExtra(.none,Args) => Args.
  addExtra(some(P),Args) => [P,..Args].

  transformEquation:(equation,nameMap,option[crExp],cons[crDefn],reports) =>
    either[reports,((locn,cons[crExp],option[crExp],crExp),cons[crDefn])].
  transformEquation(eqn(Lc,tple(_,As),.none,Val),Map,Extra,Ex,Rp) => do{
    (Ptns,Ex1) <- liftPtns(As,Map,Ex,Rp);
    (Rep,Exx) <- liftExp(Val,Map,Ex1,Rp);
    valis ((Lc,addExtra(Extra,Ptns),.none,Rep),Exx)
  }
  transformEquation(eqn(Lc,tple(_,As),some(Wh),Val),Map,Extra,Ex,Rp) => do{
    (Ptns,Ex1) <- liftPtns(As,Map,Ex,Rp);
    (Rep,Ex2) <- liftExp(Val,Map,Ex1,Rp);
    (Cond,Exx) <- liftGoal(Wh,Map,Ex2,Rp);
    valis ((Lc,addExtra(Extra,Ptns),some(Cond),Rep),Exx)
  }

  liftPtn:(canon,nameMap,cons[crDefn],reports) => either[reports,crFlow].
  liftPtn(vr(Lc,Nm,Tp),Map,Ex,Rp) => trVarPtn(Lc,Nm,Tp,Map,Ex,Rp).
  liftPtn(enm(Lc,FullNm,Tp),Map,Ex,Rp) => either((crLbl(Lc,FullNm,Tp),Ex)).
  liftPtn(intr(Lc,Ix),Map,Ex,Rp) => either((crInt(Lc,Ix),Ex)).
  liftPtn(flt(Lc,Dx),Map,Ex,Rp) => either((crFlot(Lc,Dx),Ex)).
  liftPtn(strng(Lc,Sx),Map,Ex,Rp) => either((crStrg(Lc,Sx),Ex)).
  liftPtn(whr(Lc,Ptn,Cond),Map,Ex,Rp) => do{
    (LPtn,Ex1) <- liftPtn(Ptn,Map,Ex,Rp);
    (LCond,Exx) <- liftGoal(Cond,Map,Ex,Rp);
    valis (crWhere(Lc,LPtn,LCond),Exx)
  }
  liftPtn(tple(Lc,Els),Map,Ex,Rp) => do{
    (LEls,Exx) <- liftPtns(Els,Map,Ex,Rp);
    valis (crTpl(Lc,LEls),Exx)
  }
  liftPtn(apply(Lc,vr(VLc,VNm,_),tple(_,Els),Tp),Map,Ex,Rp) => do{
    (LArgs,Ex1) <- liftPtns(Els,Map,Ex,Rp);
    liftPtnCallOp(Lc,VNm,LArgs,Tp,Map,Ex1,Rp)
  }
  liftPtn(apply(Lc,enm(VLc,FullNm,_),tple(_,Els),Tp),Map,Ex,Rp) => do{
    (LArgs,Ex1) <- liftPtns(Els,Map,Ex,Rp);

    valis (crTerm(Lc,FullNm,LArgs,Tp),Ex1)
  }
  
  liftPtns:(cons[canon],nameMap,cons[crDefn],reports) => either[reports,(cons[crExp],cons[crDefn])].
  liftPtns([],_,Ex,_) => either(([],Ex)).
  liftPtns([P,..Ps],Map,Ex,Rp) => do{
    (A,Ex1) <- liftPtn(P,Map,Ex,Rp);
    (As,Exx) <- liftPtns(Ps,Map,Ex1,Rp);
    valis ([A,..As],Exx)
  }

  liftPtnCallOp:(locn,string,cons[crExp],tipe,nameMap,cons[crDefn],reports) =>
    either[reports,(crExp,cons[crDefn])].
  liftPtnCallOp(Lc,Nm,Args,Tp,Map,Ex,Rp) where Entry^= lookupVarName(Map,Nm) =>
    implementPtnCall(Lc,Entry,Args,Tp,Map,Ex,Rp).

  implementPtnCall(Lc,moduleCons(Nm,CTp),Args,Tp,_,Ex,_) => either((crTerm(Lc,Nm,Args,Tp),Ex)).
  
  trVarPtn(Lc,Nm,Tp,Map,Ex,Rp) => implementVarPtn(Lc,Nm,lookupVarName(Map,Nm),Tp,Map,Ex,Rp).

  implementVarPtn(Lc,Nm,.none,Tp,_,Ex,_) => either((crVar(Lc,crId(Nm,Tp)),Ex)).
  implementVarPtn(Lc,Nm,some(moduleCons(Enum,CTp)),Tp,_,Ex,_) where ETp^=isEnumType(CTp) =>
    either((crLbl(Lc,Enum,ETp),Ex)).
  implementVarPtn(Lc,Nm,some(V),Tp,Map,_,Rp) => other(reportError(Rp,"not permitted to match against $(Nm)\:$(V)",Lc)).

  liftExp:(canon,nameMap,cons[crDefn],reports) => either[reports,crFlow].
  liftExp(vr(Lc,Nm,Tp),Map,Ex,Rp) => do{
    V <- liftVarExp(Lc,Nm,Tp,Map,Rp);
    valis (V,Ex)
  }
  liftExp(intr(Lc,Ix),Map,Ex,Rp) => either((crInt(Lc,Ix),Ex)).
  liftExp(flt(Lc,Dx),Map,Ex,Rp) => either((crFlot(Lc,Dx),Ex)).
  liftExp(strng(Lc,Sx),Map,Ex,Rp) => either((crStrg(Lc,Sx),Ex)).
  liftExp(enm(Lc,FullNm,Tp),Map,Ex,Rp) => either((crLbl(Lc,FullNm,Tp),Ex)).
  liftExp(tple(Lc,Els),Map,Ex,Rp) => do{
    (LEls,Exx) <- liftExps(Els,Map,Ex,Rp);
    valis (mkCrTpl(Lc,LEls),Exx)
  }
  liftExp(apply(Lc,Op,tple(_,Els),Tp),Map,Ex,Rp) => do{
    (LEls,Ex1) <- liftExps(Els,Map,Ex,Rp);
    liftExpCallOp(Lc,Op,LEls,Tp,Map,Ex1,Rp)
  }
  liftExp(dot(Lc,Rc,Fld,Tp),Map,Ex,Rp) => do{
    (LRc,Ex1) <- liftExp(Rc,Map,Ex,Rp);
    valis (crDot(Lc,LRc,Fld,Tp),Ex1)
  }
  liftExp(whr(_,E,enm(_,"star.core#true",nomnal("star.core*boolean"))),Map,Ex,Rp) =>
    liftExp(E,Map,Ex,Rp).
  liftExp(whr(Lc,E,C),Map,Ex,Rp) => do{
    (LE,Ex1) <- liftExp(E,Map,Ex,Rp);
    (LC,Ex2) <- liftGoal(C,Map,Ex1,Rp);
    valis (crWhere(Lc,LE,LC),Ex2)
  }
  liftExp(E,Map,Ex,Rp) where isGoal(E) => liftGoal(E,Map,Ex,Rp).
  liftExp(cond(Lc,Ts,Th,El),Map,Ex,Rp) => do{
    (LTs,Ex1) <- liftGoal(Ts,Map,Ex,Rp);
    (LTh,Ex2) <- liftExp(Th,Map,Ex1,Rp);
    (LEl,Exx) <- liftExp(El,Map,Ex2,Rp);
    valis (crCnd(Lc,LTs,LTh,LEl),Exx)
  }
  liftExp(record(Lc,Path,Fields,Tp),Map,Ex,Rp) => do{
--    logMsg("lift record $(record(Lc,Path,Fields,Tp))");
    (LFields,Exx) <- liftFields(Fields,Map,Ex,Rp);
    valis (crRecord(Lc,Path,LFields,Tp),Exx)
  }
  liftExp(letExp(Lc,Grp,Bnd),Map,Ex,Rp) => 
    liftLetExp(Lc,Grp,Bnd,Map,Ex,Rp).
  liftExp(letRec(Lc,Grp,Bnd),Map,Ex,Rp) => 
    liftLetRec(Lc,Grp,Bnd,Map,Ex,Rp).
  liftExp(Lam where lambda(Eqns,Tp).=Lam,Map,Ex,Rp) => do{
    Lc .= locOf(Lam);
--    logMsg("transform lambda $(Lam)");
    (LMap,Extra,FreeTerm) <- lambdaMap(Lam,Map,Rp);
--    logMsg("lambda map $(head(LMap))");
--    logMsg("extra vars in lambda: $(Extra)");
    ATp .= extendFunTp(deRef(Tp),some(Extra));
    FullNm .= layerName(LMap);
    (Eqs,Ex1) <- transformEquations(Eqns,LMap,some(Extra),Ex,Rp);
    LamFun .= functionMatcher(Lc,FullNm,ATp,Eqs);
--    logMsg("lifted lambda fun $(LamFun)");
--    logMsg("lambda closure $(crTerm(Lc,FullNm,[FreeTerm],ATp))");
    valis (crTerm(Lc,FullNm,[FreeTerm],ATp),[LamFun,..Ex1])
  }
  liftExp(csexp(Lc,Gov,Cses,Tp),Map,Ex,Rp) => do{
    (LGov,Ex1) <- liftExp(Gov,Map,Ex,Rp);
    (Cs,Ex2) <- transformEquations(Cses,Map,.none,Ex1,Rp);
    Reslt .= caseMatcher(Lc,LGov,Cs,Tp);
    valis (Reslt,Ex2)
  }

  closureType:(cons[tipe],tipe)=>tipe.
  closureType(Els,T) => consType(tupleType(Els),T).

  liftExps:(cons[canon],nameMap,cons[crDefn],reports) => either[reports,(cons[crExp],cons[crDefn])].
  liftExps([],_,Ex,_) => either(([],Ex)).
  liftExps([P,..Ps],Map,Ex,Rp) => do{
    (A,Ex1) <- liftExp(P,Map,Ex,Rp);
    (As,Exx) <- liftExps(Ps,Map,Ex1,Rp);
    valis ([A,..As],Exx)
  }

  liftFields:(cons[(string,canon)],nameMap,cons[crDefn],reports) =>
    either[reports,(cons[(string,crExp)],cons[crDefn])].
  liftFields([],_,Ex,_) => either(([],Ex)).
  liftFields([(N,P),..Ps],Map,Ex,Rp) => do{
--    logMsg("lift field $(N) = $(P)");
    (A,Ex1) <- liftExp(P,Map,Ex,Rp);
--    logMsg("field exp $(A)");
    (As,Exx) <- liftFields(Ps,Map,Ex1,Rp);
    valis ([(N,A),..As],Exx)
  }
  
  liftExpCallOp:(locn,canon,cons[crExp],tipe,nameMap,cons[crDefn],reports) =>
    either[reports,crFlow].
  liftExpCallOp(Lc,vr(_,Nm,_),Args,Tp,Map,Ex,Rp) where isEscape(Nm) =>
    either((crECall(Lc,Nm,Args,Tp),Ex)).
  liftExpCallOp(Lc,vr(_,Nm,_),Args,Tp,Map,Ex,Rp) where (_,Op) ^= intrinsic(Nm) =>
    either((crIntrinsic(Lc,Op,Args,Tp),Ex)).
  liftExpCallOp(Lc,vr(_,Nm,_),Args,Tp,Map,Ex,Rp) where Entry ^= lookupVarName(Map,Nm) =>
    implementFunCall(Lc,Entry,Nm,Args,Tp,Map,Ex,Rp).
  liftExpCallOp(Lc,enm(_,FullNm,_),[],Tp,Map,Ex,Rp) =>
    either((crLbl(Lc,FullNm,Tp),Ex)).
  liftExpCallOp(Lc,enm(_,FullNm,_),Args,Tp,Map,Ex,Rp) =>
    either((crTerm(Lc,FullNm,Args,Tp),Ex)).
  liftExpCallOp(Lc,Op,Args,Tp,Map,Ex,Rp) => do{
--    logMsg("lifting $(Op) applied to $(Args)");
    (LOp,Ex0) <- liftExp(Op,Map,Ex,Rp);
    valis (crOCall(Lc,LOp,Args,Tp),Ex0)
  }
  liftExpCallOp(Lc,Op,Args,Tp,_,_,Rp) =>
    other(reportError(Rp,"cannot compile function $(Op) applied to $(Args)",Lc)).

  implementFunCall:(locn,nameMapEntry,string,cons[crExp],tipe,nameMap,cons[crDefn],reports) =>
    either[reports,crFlow].
  implementFunCall(Lc,moduleFun(Fn,_,FTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,Fn,Args,Tp),Ex)).
  implementFunCall(Lc,moduleCons(Fn,FTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crTerm(Lc,Fn,Args,Tp),Ex)).
  implementFunCall(Lc,localFun(Nm,_,F),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,Nm,[F,..Args],Tp),Ex)).
  implementFunCall(Lc,labelArg(Base,Ix,_),_,Args,Tp,Map,Ex,Rp) => do{
--    logMsg("checking label base (fun) $(Base)");
--    NBase <- liftVarExp(Lc,Base,BTp,Map,Rp);
    either((crOCall(Lc,crTplOff(Lc,crVar(Lc,Base),Ix,Tp),Args,Tp),Ex))
  }.
  implementFunCall(Lc,localVar(FrVr),_,Args,Tp,Map,Ex,Rp) =>
    either((crOCall(Lc,crVar(Lc,FrVr),Args,Tp),Ex)).
  implementFunCall(Lc,moduleVar(Vr),_,Args,Tp,Map,Ex,Rp) =>
    either((crOCall(Lc,Vr,Args,Tp),Ex)).
  implementFunCall(Lc,globalVar(Nm,GTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,Nm,Args,Tp),Ex)).
  implementFunCall(Lc,mapAccess(Fn),_,Args,Tp,Map,Ex,Rp) =>
    either((crOCall(Lc,Fn(Lc),Args,Tp),Ex)).
  implementFunCall(Lc,V,Vr,Args,Tp,Map,Ex,Rp) =>
    other(reportError(Rp,"illegal variable $(Vr) - $(V)",Lc)).

  liftVarExp:(locn,string,tipe,nameMap,reports) => either[reports,crExp].
  liftVarExp(Lc,Nm,Tp,Map,Rp) where Entry ^= lookupVarName(Map,Nm) => do{
--    logMsg("implement access to $(Nm) via $(Entry)");
    implementVarExp(Lc,Entry,Map,Tp,Rp)
  }.
  liftVarExp(Lc,Nm,Tp,Map,Rp) => do{
--    logMsg("default var $(Nm)");
    valis crVar(Lc,crId(Nm,Tp))
  }

  implementVarExp:(locn,nameMapEntry,nameMap,tipe,reports) => either[reports,crExp].
  implementVarExp(Lc,localFun(Nm,Tp,E),_,_,_) => either(crTerm(Lc,Nm,[E],Tp)).
  implementVarExp(Lc,labelArg(Base,Ix,_),Map,Tp,Rp) => do{
--    logMsg("checking label base $(Base)\[$(Ix)]");
    valis crTplOff(Lc,crVar(Lc,Base),Ix,Tp)
  }.
  implementVarExp(Lc,localVar(Vr),_,Tp,Rp) => either(crVar(Lc,Vr)).
  implementVarExp(Lc,moduleVar(Vr),_,Tp,Rp) => either(Vr).
  implementVarExp(Lc,moduleCons(Enum,CTp),_,Tp,Rp) =>
    either(crLbl(Lc,Enum,Tp)).
  implementVarExp(Lc,moduleFun(_,V,FTp),_,Tp,Rp) => either(V).
  implementVarExp(Lc,globalVar(Nm,GTp),_,Tp,Rp) => either(crVar(Lc,crId(Nm,GTp))).
  implementVarExp(Lc,mapAccess(Fn),_,Tp,Rp) => either(Fn(Lc)).
  implementVarExp(Lc,E,_,_,Rp) => other(reportError(Rp,"cannot transform variable $(E)",Lc)).

  liftGoal:(canon,nameMap,cons[crDefn],reports) => either[reports,crFlow].
  liftGoal(conj(Lc,L,R),Map,Ex,Rp) => do{
    (LL,Ex1) <- liftGoal(L,Map,Ex,Rp);
    (LR,Ex2) <- liftGoal(R,Map,Ex1,Rp);
    valis (crCnj(Lc,LL,LR),Ex2)
  }
  liftGoal(disj(Lc,L,R),Map,Ex,Rp) => do{
    (LL,Ex1) <- liftGoal(L,Map,Ex,Rp);
    (LR,Ex2) <- liftGoal(R,Map,Ex1,Rp);
    valis (crDsj(Lc,LL,LR),Ex2)
  }
  liftGoal(neg(Lc,R),Map,Ex,Rp) => do{
    (LR,Ex1) <- liftGoal(R,Map,Ex,Rp);
    valis (crNeg(Lc,LR),Ex1)
  }
  liftGoal(cond(Lc,T,L,R),Map,Ex,Rp) => do{
    (LT,Ex1) <- liftGoal(T,Map,Ex,Rp);
    (LL,Ex2) <- liftGoal(L,Map,Ex1,Rp);
    (LR,Ex3) <- liftGoal(R,Map,Ex2,Rp);
    valis (crCnd(Lc,LT,LL,LR),Ex3)
  }
  liftGoal(match(Lc,P,E),Map,Ex,Rp) => do{
    (LP,Ex1) <- liftPtn(P,Map,Ex,Rp);
    (LE,Ex2) <- liftExp(E,Map,Ex1,Rp);
    valis (crMatch(Lc,LP,LE),Ex2)
  }
  liftGoal(serch(Lc,Ptn,Gen,Iter),Map,_,Rp) =>
    other(reportError(Rp,"cannot deal with search condition",Lc)).
  liftGoal(G,Map,Ex,Rp) =>
    liftExp(G,Map,Ex,Rp).

  extendFunTp:all x ~~ hasType[x] |: (tipe,option[x])=>tipe.
  extendFunTp(Tp,.none) => Tp.
  extendFunTp(Tp,Vs) where (A,B)^=isFunType(Tp) &&
      tupleType(Es).=deRef(A) =>
    funType(extendTplType(Es,Vs),B).
  extendFunTp(allType(V,B),Vs) => allType(V,extendFunTp(B,Vs)).
  extendFunTp(existType(V,B),Vs) => existType(V,extendFunTp(B,Vs)).
  extendFunTp(constrainedType(T,C),Vs) => constrainedType(extendFunTp(T,Vs),C).

  extendTplType:all x ~~ hasType[x] |: (cons[tipe],option[x])=>cons[tipe].
  extendTplType(Es,.none) => Es.
  extendTplType(Es,some(E)) => [typeOf(E),..Es].

  collectMtd:(canonDef,option[crExp],map[string,nameMapEntry])=>map[string,nameMapEntry].
  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),some(ThVr),LL) where isFunDef(Val) =>
    LL[Nm->localFun(closureNm(FullNm),Tp,ThVr)].
  collectMtd(varDef(Lc,Nm,FullNm,Val,Cx,Tp),some(ThVr),LL) =>
    LL[Nm->localVar(crId(Nm,Tp))].
  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),.none,LL) where isFunDef(Val) =>
    LL[Nm->moduleFun(FullNm,crTerm(Lc,closureNm(FullNm),[mkCrTpl(Lc,[])],funType([],Tp)),funType([],Tp))].
  collectMtd(varDef(Lc,Nm,FullNm,Val,Cx,Tp),.none,LL) =>
    LL[Nm->localVar(crId(Nm,Tp))].
  collectMtd(implDef(Lc,_,FullNm,Val,Cx,Tp),ThVr,LL) =>
    collectMtd(varDef(Lc,FullNm,FullNm,Val,Cx,Tp),ThVr,LL).
--  collectMtd(implDef(Lc,_,FullNm,Def,Cx,Tp),ThVr,LL) where FTp .= funType([],Tp) =>
--    collectMtd(varDef(Lc,FullNm,FullNm,lambda([eqn(Lc,tple(Lc,[]),.none,Def)],FTp),Cx,FTp),ThVr,LL).
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),ThVr,LL) => LL[Nm->moduleCons(FullNm,Tp)].
  collectMtd(typeDef(_,_,_,_),_,LL) => LL.
  collectMtd(conDef(_,_,_,_),_,LL) => LL.

  collectLabelVars:(cons[crVar],crVar,integer,map[string,nameMapEntry]) => map[string,nameMapEntry].
  collectLabelVars([],_,_,LV) => LV.
  collectLabelVars([V,..Vrs],ThV,Ix,Entries) where crId(Nm,Tp) .= V =>
    collectLabelVars(Vrs,ThV,Ix+1,Entries[Nm->labelArg(ThV,Ix,Tp)]).

  isModule(moduleFun(_,_,_))=>some(.true).
  isModule(moduleVar(_)) => some(.true).
  isModule(globalVar(_,_))=>some(.true).
  isModule(_) default => .none.

  lookup:all e ~~ (nameMap,string,(nameMapEntry)=>option[e])=>option[e].
  lookup([],_,_) => .none.
  lookup([lyr(_,Entries),..Map],Nm,P) where E ^= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).

  lookupVarName:(nameMap,string)=>option[nameMapEntry].
  lookupVarName(Map,Nm) => lookup(Map,Nm,anyDef).

  layerName:(nameMap) => string.
  layerName([lyr(Nm,_),.._]) => Nm.
  layerName([]) => "".

  anyDef(D) => some(D).

  genVar:(string,tipe) => crVar.
  genVar(Pr,Tp) => crId(genSym(Pr),Tp).

  makeFunVars:(tipe)=>cons[crVar].
  makeFunVars(Tp) where tupleType(Es).=funTypeArg(deRef(Tp)) => (Es//(E)=>genVar("_",E)).

  makeDotLbl:(string)=>string.
  makeDotLbl(Fld) => "."++Fld.

  crTpl:(locn,cons[crExp]) => crExp.
  crTpl(Lc,Args) => let{
    Tp = typeOf(Args).
    Ar = size(Args).
  } in crTerm(Lc,tplLbl(Ar),Args,Tp).

  crCon:(locn,string,cons[crExp],tipe) => crExp.
  crCon(Lc,Nm,Args,Tp) => crTerm(Lc,Nm,Args,Tp).

  closureNm:(string)=>string.
  closureNm(Nm)=>Nm++"^".
}
