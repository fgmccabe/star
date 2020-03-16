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

  nameMapEntry ::= moduleVar(string,string,tipe)
    | localFun(string,tipe,crVar)
    | localVar(crExp)
    | moduleCons(string,tipe)
    | labelArg(string,tipe,integer,tipe)
    | globalVar(string,tipe).

  mapLayer ::= lyr(string,map[string,nameMapEntry],option[crVar]).

  nameMap ~> list[mapLayer].

  implementation display[mapLayer] => {.
    disp(lyr(Path,Entries,some(ThVr))) =>
      ssSeq([disp(Path),ss(":"),disp(Entries),ss("\nTheta var: "),disp(ThVr),ss("\n")]).
    disp(lyr(Path,Entries,.none)) =>
      ssSeq([disp(Path),ss(":"),disp(Entries),ss("\n")])
  .}

  implementation display[nameMapEntry] => {.
    disp(moduleVar(V,C,Tp)) => ssSeq([ss("module var "),disp(V),ss(", closure "),disp(C),ss(":"),disp(Tp)]).
    disp(moduleCons(Nm,Tp)) => ssSeq([ss("module cons "),ss(Nm),ss(":"),disp(Tp)]).
    disp(localFun(Nm,Tp,V)) => ssSeq([ss("local fun "),ss(Nm),ss(":"),disp(Tp)]).
    disp(localVar(Vr)) => ssSeq([ss("local var"),disp(Vr)]).
    disp(labelArg(BaseNm,_,Ix,Tp)) => ssSeq([ss("label arg"),disp(BaseNm),ss("["),disp(Ix),ss("]:"),disp(Tp)]).
    disp(globalVar(Nm,Tp)) => ssSeq([ss("global "),ss(Nm),ss(":"),disp(Tp)]).
  .}

  crFlow ~> (crExp,list[crDefn]).

  extraVars:(nameMap)=>list[crVar].
  extraVars([lyr(_,_,.none),.._]) => [].
  extraVars([lyr(_,_,some(V)),.._]) => [V].

  implementation hasType[nameMapEntry]=>{.
    typeOf(moduleVar(_,_,Tp)) => Tp.
    typeOf(localFun(_,Tp,_)) => Tp.
    typeOf(moduleCons(_,Tp)) => Tp.
    typeOf(localVar(V))=>typeOf(V).
    typeOf(labelArg(_,_,_,Tp)) => Tp.
    typeOf(globalVar(_,Tp)) => Tp.
  .}

  public normalize:(pkgSpec,canonDef,reports)=>either[reports,list[crDefn]].
  normalize(PkgSpec,Prg,Rp) => do{
    Map .= pkgMap(PkgSpec);
--    logMsg("package map $(Map)");
    (V,Defs) <- transformDef(Prg,Map,[],[],Rp);
--    logMsg("raw transformed def: $(V)");
    Bound .= (V//((Vr,Vl))=>vrDef(locOf(Vl),Vr,Vl));
    valis Bound++Defs
  }

  pkgMap:(pkgSpec) => nameMap.
  pkgMap(pkgSpec(Pkg,Imports,Tp,Cons,Impls,PkgVrs)) =>
    [lyr(pkgName(Pkg),foldRight(((Nm,ITp),D)=>D[Nm->globalVar(Nm,ITp)],[],PkgVrs),.none)].

  groupMap:(locn,list[canonDef],string,nameMap,reports) => either[reports,(nameMap,option[crVar],crExp)].
  groupMap(Lc,Grp,Prefix,Outer,Rp) => do{
--    logMsg("making map for group $(Grp)");
    FreeVars .= findFreeVarsInGroup(Grp,Outer);
    logMsg("free vars = $(FreeVars)");

    if isEmpty(FreeVars) then {
--      logMsg("no theta var for group $(Grp)");
      M .= [lyr("",foldRight((D,LL)=>collectMtd(D,Prefix,.none,LL),[],Grp),.none),..Outer];
      valis (M,.none,crTpl(Lc,FreeVars//(V)=>crVar(Lc,V)))
    } else if [FrVr].=FreeVars then {
      ThVr <- liftVarExp(Lc,crName(FrVr),typeOf(FrVr),Outer,Rp);
      L .= [crName(FrVr)->localVar(ThVr)];
--      logMsg("L=$(L)");
      M .= [lyr("",foldRight((D,LL)=>collectMtd(D,Prefix,some(FrVr),LL),L,Grp),some(FrVr)),..Outer];
--      logMsg("M=$(M)");
      valis (M,some(FrVr),ThVr)
    }
    else {
      ThV .= genVar("_ThVr",tupleType(FreeVars//typeOf));
--      logMsg("theta var for group $(Grp) is $(ThV)");
      L .= collectLabelVars(FreeVars,ThV,0,[]);
--      logMsg("new label vars $(L)");
      ThVr .= some(ThV);
    
      M .= [lyr(Prefix,foldRight((D,LL)=>collectMtd(D,Prefix,ThVr,LL),L,Grp),ThVr),..Outer];
      valis (M,ThVr,crTpl(Lc,FreeVars//(V)=>crVar(Lc,V)))
    }
  }

  findFreeVarsInGroup:(list[canonDef],nameMap) => list[crVar].
  findFreeVarsInGroup(G,Map) => 
    freeLabelVars(freeVarsInGroup(G,[],[]),Map,[])::list[crVar].

  lambdaMap:(canon,nameMap,reports) => either[reports,(nameMap,option[crVar],list[crExp])].
  lambdaMap(Lam,Outer,Rp) => do{
    logMsg("making map for lambda $(Lam)");
--    logMsg("initial free $(freeVarsInTerm(Lam,[],[]))");
    freeVars .= freeLabelVars(freeVarsInTerm(Lam,[],[]),Outer,[])::list[crVar];
    Lc .= locOf(Lam);
    FullNm .= genSym(layerName(Outer)++"λ");
    logMsg("free vars in lambda = $(freeVars), full name $(FullNm)");

    if isEmpty(freeVars) then {
--      logMsg("no theta var for lambda");
      M .= [lyr(FullNm,[],.none),..Outer];
      valis (M,.none,[])
    } else if [FrVr].=freeVars then{
      ThVr <- liftVarExp(Lc,crName(FrVr),typeOf(FrVr),Outer,Rp);
      L .= [crName(FrVr)->localVar(ThVr)];
      M .= [lyr(FullNm,L,some(FrVr)),..Outer];
      valis (M,some(FrVr),[ThVr])
    } else {
      ThV .= genVar("_ThVr",tupleType(freeVars//typeOf));
--      logMsg("theta var is $(ThV)");
      L .= collectLabelVars(freeVars,ThV,0,[]);
--      logMsg("new label vars $(L)");
    
      M .= [lyr(FullNm,L,some(ThV)),..Outer];
      valis (M,some(ThV),freeVars//(V)=>crVar(Lc,V))
    }
  }

  codeGroup([varDef(_,_,_,_,_,_),.._]) => .true.
  codeGroup([implDef(_,_,_,_,_,_),.._]) => .true.
  codeGroup([_,..G])=>codeGroup(G).
  codeGroup([])=>.false.
  
  liftLetExp:(locn,list[canonDef],canon,nameMap,list[crDefn],reports) => either[reports,crFlow].
  liftLetExp(Lc,Defs,Bnd,Map,Ex,Rp) where \+ codeGroup(Defs) => liftExp(Bnd,Map,Ex,Rp).
  liftLetExp(Lc,Defs,Bnd,Map,Ex,Rp) => do{
    logMsg("group $(Defs)");
    (GMap,GrpVr,GrpFree) <- groupMap(Lc,Defs,layerName(Map),Map,Rp);
    logMsg("group map $(GMap)");
    logMsg("free term $(GrpVr) = $(GrpFree)");
    (Vs,Ex1,BMap) <- transformGroup(Defs,GMap,Ex,[],Rp);
    logMsg("Vs=$(Vs)");

    logMsg("lifting bound exp $(Bnd)");
    (BndTrm,Ex2) <- liftExp(Bnd,BMap,Ex1,Rp);
    logMsg("Bound exp s=$(BndTrm)");

    Bound .= foldRight(((Vr,Vl),X)=>crLtt(locOf(Vl),Vr,Vl,X),BndTrm,Vs);

    if GrpThVr ^= GrpVr then
      valis (crLtt(Lc,GrpThVr,GrpFree,Bound),Ex2)
    else
    valis (Bound,Ex2)
  }

  isFunctions:(list[canonDef])=>boolean.
  isFunctions(Defs) => varDef(_,_,_,Vl,_,_) in Defs *> lambda(_,_,_).=Vl.

  transformGroup:(list[canonDef],nameMap,list[crDefn],list[(crVar,crExp)],reports) => either[reports,(list[(crVar,crExp)],list[crDefn],nameMap)].
  transformGroup([],Map,D,Vs,_) => either((Vs,D,Map)).
  transformGroup([D,..Ds],Map,Ex,V,Rp) => do {
    (V1,Ex1) <- transformDef(D,Map,[],V,Rp);
    logMsg("transformed def is $(V1)\:$(Ex1)");
    transformGroup(Ds,Map,Ex++Ex1,V1,Rp)
  }

  transformDef:(canonDef,nameMap,list[crDefn],list[(crVar,crExp)],reports) =>
    either[reports,(list[(crVar,crExp)],list[crDefn])].
  transformDef(varDef(Lc,Nm,FullNm,lambda(_,Eqns,Tp),_,_),Map,Ex,Vs,Rp) => do{
    logMsg("transform function $(FullNm) - $(Eqns)\:$(Tp)");
    Extra .= extraVars(Map);
    logMsg("extra vars in function: $(Extra)");
    ATp .= extendFunTp(deRef(Tp),Extra);
    (Eqs,Ex1) <- transformEquations(Eqns,Map,Extra,Ex,Rp);
    logMsg("normalized equations for $(FullNm)\: $(Eqs)");

    ClosureNm .= closureNm(FullNm);
    ClVar .= genVar(Nm,Tp);
    ClVars .= makeFunVars(Tp);
    ClArgs .= [ClVar,..ClVars];

    ClosTp .= funType([Tp,..tplTypes(deRef(funTypeArg(Tp)))],funTypeRes(Tp));

    if isEmpty(Extra) then{
      ClosEntry .=
	fnDef(Lc,ClosureNm,ClosTp,
	  ClArgs,crCall(Lc,FullNm,ClVars//(V)=>crVar(Lc,V),funTypeRes(Tp)));

      logMsg("closure for $(FullNm) is $(ClosEntry)");

      valis (Vs,[functionMatcher(Lc,FullNm,ATp,Eqs),ClosEntry,..Ex1])
    } else if [Exv].=Extra then {
      ClosEntry .=
	fnDef(Lc,ClosureNm,ClosTp,ClArgs,
	  crCall(Lc,FullNm,[crTplOff(Lc,crVar(Lc,ClVar),0,typeOf(Exv)),..(ClVars//(V)=>crVar(Lc,V))],funTypeRes(Tp)));
      logMsg("closure entry: $(ClosEntry)");
      valis (Vs,[functionMatcher(Lc,FullNm,ATp,Eqs),ClosEntry,..Ex1])
    } else {
      FrTp .= tupleType(Extra//typeOf);
      ClosEntry .=
	fnDef(Lc,ClosureNm,ClosTp,ClArgs,
	  crCall(Lc,FullNm,[crTplOff(Lc,crVar(Lc,ClVar),0,FrTp),..(ClVars//(V)=>crVar(Lc,V))],funTypeRes(Tp)));

      logMsg("closure entry: $(ClosEntry)");
      valis (Vs,[functionMatcher(Lc,FullNm,ATp,Eqs),ClosEntry,..Ex1])
    }
  }
  transformDef(varDef(Lc,Nm,FullNm,Val,_,Tp),Map,Ex,Vs,Rp) => do{
    (Vl,Exx) <- liftExp(Val,Map,Ex,Rp);
    Vr .= crId(Nm,Tp);
    valis ([Vs..,(Vr,Vl)],Exx)
  }
  transformDef(implDef(Lc,Nm,FullNm,Val,Cx,Tp),Map,Ex,Vs,Rp) =>
    transformDef(varDef(Lc,Nm,FullNm,Val,Cx,Tp),Map,Ex,Vs,Rp).
  transformDef(_,_,Ex,Vs,_) => either((Vs,Ex)).

  transformEquations:(list[equation],nameMap,list[crVar],list[crDefn],reports) =>
    either[reports,(list[(locn,list[crExp],option[crExp],crExp)],list[crDefn])].
  transformEquations([],_,_,Ex,_) => either(([],Ex)).
  transformEquations([Eqn,..Eqns],Map,Extra,Ex,Rp) => do{
    (Trple,Ex1) <- transformEquation(Eqn,Map,Extra,Ex,Rp);
    (Rest,Exx) <- transformEquations(Eqns,Map,Extra,Ex1,Rp);
    valis ([Trple,..Rest],Exx)
  }

  transformEquation:(equation,nameMap,list[crVar],list[crDefn],reports) =>
    either[reports,((locn,list[crExp],option[crExp],crExp),list[crDefn])].
  transformEquation(eqn(Lc,tple(_,As),.none,Val),Map,Extra,Ex,Rp) => do{
    (Ptns,Ex1) <- liftPtns(As,Map,Ex,Rp);
    (Rep,Exx) <- liftExp(Val,Map,Ex1,Rp);
    valis ((Lc,(Extra//(V)=>crVar(Lc,V))++Ptns,.none,Rep),Exx)
  }
  transformEquation(eqn(Lc,tple(_,As),some(Wh),Val),Map,Extra,Ex,Rp) => do{
    (Ptns,Ex1) <- liftPtns(As,Map,Ex,Rp);
    (Rep,Ex2) <- liftExp(Val,Map,Ex1,Rp);
    (Cond,Exx) <- liftGoal(Wh,Map,Ex2,Rp);
    valis ((Lc,(Extra//(V)=>crVar(Lc,V))++Ptns,some(Cond),Rep),Exx)
  }

  liftPtn:(canon,nameMap,list[crDefn],reports) => either[reports,crFlow].
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
  
  liftPtns:(list[canon],nameMap,list[crDefn],reports) => either[reports,(list[crExp],list[crDefn])].
  liftPtns([],_,Ex,_) => either(([],Ex)).
  liftPtns([P,..Ps],Map,Ex,Rp) => do{
    (A,Ex1) <- liftPtn(P,Map,Ex,Rp);
    (As,Exx) <- liftPtns(Ps,Map,Ex1,Rp);
    valis ([A,..As],Exx)
  }

  liftPtnCallOp:(locn,string,list[crExp],tipe,nameMap,list[crDefn],reports) =>
    either[reports,(crExp,list[crDefn])].
  liftPtnCallOp(Lc,Nm,Args,Tp,Map,Ex,Rp) where Entry^= lookupVarName(Map,Nm) =>
    implementPtnCall(Lc,Entry,Args,Tp,Map,Ex,Rp).

  implementPtnCall(Lc,moduleCons(Nm,CTp),Args,Tp,_,Ex,_) => either((crTerm(Lc,Nm,Args,Tp),Ex)).
  
  trVarPtn(Lc,Nm,Tp,Map,Ex,Rp) => implementVarPtn(Lc,Nm,lookupVarName(Map,Nm),Tp,Map,Ex,Rp).

  implementVarPtn(Lc,Nm,.none,Tp,_,Ex,_) => either((crVar(Lc,crId(Nm,Tp)),Ex)).
  implementVarPtn(Lc,Nm,some(moduleCons(Enum,CTp)),Tp,_,Ex,_) where ETp^=isEnumType(CTp) =>
    either((crLbl(Lc,Enum,ETp),Ex)).
  implementVarPtn(Lc,Nm,some(V),Tp,Map,_,Rp) => other(reportError(Rp,"not permitted to match against $(Nm)\:$(V)",Lc)).

  liftExp:(canon,nameMap,list[crDefn],reports) => either[reports,crFlow].
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
    valis (mkCrTpl(LEls,Lc),Exx)
  }
  liftExp(apply(Lc,Op,tple(_,Els),Tp),Map,Ex,Rp) => do{
    (LEls,Ex1) <- liftExps(Els,Map,Ex,Rp);
    liftExpCallOp(Lc,Op,LEls,Tp,Map,Ex1,Rp)
  }
  liftExp(dot(Lc,Rc,Fld,Tp),Map,Ex,Rp) => do{
    (LRc,Ex1) <- liftExp(Rc,Map,Ex,Rp);
    valis (crDot(Lc,LRc,makeDotLbl(Fld),Tp),Ex1)
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
  liftExp(letExp(Lc,Grp,Bnd),Map,Ex,Rp) => do{
--    logMsg("transform $(letExp(Lc,Grp,Bnd))");
    (TrExp,Exx) <- liftLetExp(Lc,Grp,Bnd,Map,Ex,Rp);
--    logMsg("transformed let exp $(TrExp)\nwith defs $(Exx)");
    valis (TrExp,Exx)
  }.
  liftExp(Lam where lambda(Lc,Eqns,Tp).=Lam,Map,Ex,Rp) => do{
    logMsg("transform lambda $(Lam)");
    (LMap,ClV,Cl) <- lambdaMap(Lam,Map,Rp);
    logMsg("lambda map $(head(LMap))");
    Extra .= extraVars(LMap);
    logMsg("extra vars in lambda: $(Extra)");
    ATp .= extendFunTp(deRef(Tp),Extra);
    FullNm .= layerName(LMap);
    (Eqs,Ex1) <- transformEquations(Eqns,LMap,Extra,Ex,Rp);
    LamFun .= functionMatcher(Lc,FullNm,ATp,Eqs);
    logMsg("lifted lambda fun $(LamFun)");
    logMsg("lambda closure $(crTerm(Lc,FullNm,Cl,ATp))");

    valis (crTerm(Lc,FullNm,Cl,ATp),[Ex1..,LamFun])
  }

  closureType:(list[tipe],tipe)=>tipe.
  closureType(Els,T) => consType(tupleType(Els),T).

/*
    abstraction(locn,canon,canon,tipe) |
    act(locn,canonAction) | 
*/
    
  liftExps:(list[canon],nameMap,list[crDefn],reports) => either[reports,(list[crExp],list[crDefn])].
  liftExps([],_,Ex,_) => either(([],Ex)).
  liftExps([P,..Ps],Map,Ex,Rp) => do{
    (A,Ex1) <- liftExp(P,Map,Ex,Rp);
    (As,Exx) <- liftExps(Ps,Map,Ex1,Rp);
    valis ([A,..As],Exx)
  }

  liftFields:(list[(string,canon)],nameMap,list[crDefn],reports) =>
    either[reports,(list[(string,crExp)],list[crDefn])].
  liftFields([],_,Ex,_) => either(([],Ex)).
  liftFields([(N,P),..Ps],Map,Ex,Rp) => do{
    (A,Ex1) <- liftExp(P,Map,Ex,Rp);
    (As,Exx) <- liftFields(Ps,Map,Ex1,Rp);
    valis ([(N,A),..As],Exx)
  }
  
  liftExpCallOp:(locn,canon,list[crExp],tipe,nameMap,list[crDefn],reports) =>
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

  implementFunCall:(locn,nameMapEntry,string,list[crExp],tipe,nameMap,list[crDefn],reports) =>
    either[reports,crFlow].
  implementFunCall(Lc,localFun(ClosNm,FTp,crId(LVr,VrTp)),_,Args,Tp,Map,Ex,Rp) => do{
    ThVr <- liftVarExp(Lc,LVr,VrTp,Map,Rp);
    valis (crCall(Lc,ClosNm,[ThVr,..Args],Tp),Ex)
  }
  implementFunCall(Lc,moduleVar(Fn,_,FTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,Fn,Args,Tp),Ex)).
  implementFunCall(Lc,moduleCons(Fn,FTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crTerm(Lc,Fn,Args,Tp),Ex)).
  implementFunCall(Lc,localFun(Nm,_,F),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,Nm,[crVar(Lc,F),..Args],Tp),Ex)).
  implementFunCall(Lc,labelArg(Base,BTp,Ix,_),_,Args,Tp,Map,Ex,Rp) => do{
    logMsg("checking label base (fun) $(Base)");
    NBase <- liftVarExp(Lc,Base,BTp,Map,Rp);
    either((crOCall(Lc,crTplOff(Lc,NBase,Ix,Tp),Args,Tp),Ex))
  }.
  implementFunCall(Lc,localVar(FrVr),_,Args,Tp,Map,Ex,Rp) =>
    either((crOCall(Lc,FrVr,Args,Tp),Ex)).
  implementFunCall(Lc,globalVar(Nm,GTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,Nm,Args,Tp),Ex)).
  implementFunCall(Lc,V,Vr,Args,Tp,Map,Ex,Rp) =>
    other(reportError(Rp,"illegal variable $(Vr) - $(V)",Lc)).

  liftVarExp:(locn,string,tipe,nameMap,reports) => either[reports,crExp].
  liftVarExp(Lc,Nm,Tp,Map,Rp) where Entry ^= lookupVarName(Map,Nm) => do{
    implementVarExp(Lc,Entry,Map,Tp,Rp)
  }.
  liftVarExp(Lc,Nm,Tp,Map,Rp) => do{
    valis crVar(Lc,crId(Nm,Tp))
  }

  implementVarExp:(locn,nameMapEntry,nameMap,tipe,reports) => either[reports,crExp].
  implementVarExp(Lc,localFun(Nm,Tp,E),_,_,_) => either(crTerm(Lc,Nm,[crVar(Lc,E)],Tp)).
  implementVarExp(Lc,labelArg(BaseNm,BTp,Ix,_),Map,Tp,Rp) => do{
    logMsg("checking label base $(BaseNm)");
    NBase <- liftVarExp(Lc,BaseNm,BTp,Map,Rp);
    valis crTplOff(Lc,NBase,Ix,Tp)
  }.
  implementVarExp(Lc,localVar(Vr),_,Tp,Rp) => either(Vr).
  implementVarExp(Lc,moduleCons(Enum,CTp),_,Tp,Rp) =>
    either(crLbl(Lc,Enum,Tp)).
  implementVarExp(Lc,moduleVar(_,V,FTp),_,Tp,Rp) =>
    either(crTerm(Lc,V,[],Tp)).
  implementVarExp(Lc,globalVar(Nm,GTp),_,Tp,Rp) => either(crVar(Lc,crId(Nm,GTp))).
  implementVarExp(Lc,E,_,_,Rp) => other(reportError(Rp,"cannot transform variable $(E)",Lc)).

  liftGoal:(canon,nameMap,list[crDefn],reports) => either[reports,crFlow].
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

  extendFunTp:all x ~~ hasType[x] |: (tipe,list[x])=>tipe.
  extendFunTp(Tp,[]) => Tp.
  extendFunTp(Tp,Vs) where (A,B)^=isFunType(Tp) &&
      tupleType(Es).=deRef(A) =>
    funType(extendTplType(Es,Vs),B).
  extendFunTp(allType(V,B),Vs) => allType(V,extendFunTp(B,Vs)).
  extendFunTp(existType(V,B),Vs) => existType(V,extendFunTp(B,Vs)).
  extendFunTp(constrainedType(T,C),Vs) => constrainedType(extendFunTp(T,Vs),C).

  extendTplType:all x ~~ hasType[x] |: (list[tipe],list[x])=>list[tipe].
  extendTplType(Es,[]) => Es.
  extendTplType(Es,[V,..Vs]) => [typeOf(V),..extendTplType(Es,Vs)].

  collectMtd:(canonDef,string,option[crVar],map[string,nameMapEntry])=>map[string,nameMapEntry].
  collectMtd(varDef(Lc,Nm,FullNm,lambda(_,_,_),_,Tp),Outer,some(ThVr),LL) =>
    LL[Nm->localFun(closureNm(FullNm),Tp,ThVr)].
  collectMtd(varDef(Lc,Nm,FullNm,Val,Cx,Tp),Outer,some(ThVr),LL) =>
    LL[Nm->localVar(crVar(Lc,crId(Nm,Tp)))].
  collectMtd(varDef(Lc,Nm,FullNm,lambda(_,_,_),_,Tp),Outer,.none,LL) =>
    LL[Nm->moduleVar(FullNm,closureNm(FullNm),funType([],Tp))].
  collectMtd(varDef(Lc,Nm,FullNm,Val,Cx,Tp),Outer,.none,LL) =>
    LL[Nm->moduleVar(FullNm,closureNm(FullNm),Tp)].
  collectMtd(implDef(Lc,Nm,FullNm,Val,Cx,Tp),Outer,ThVr,LL) =>
    collectMtd(varDef(Lc,FullNm,FullNm,Val,Cx,Tp),Outer,ThVr,LL).
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),Outer,ThVr,LL) => LL[Nm->moduleCons(FullNm,Tp)].
  collectMtd(typeDef(_,_,_,_),_,_,LL) => LL.
  collectMtd(conDef(_,_,_,_),_,_,LL) => LL.

  collectLabelVars:(list[crVar],crVar,integer,map[string,nameMapEntry]) => map[string,nameMapEntry].
  collectLabelVars([],_,_,LV) => LV.
  collectLabelVars([V,..Vrs],ThV,Ix,Entries) where crId(Nm,Tp) .= V =>
    collectLabelVars(Vrs,ThV,Ix+1,Entries[Nm->labelArg(crName(ThV),typeOf(ThV),Ix,Tp)]).

  freeLabelVars:(set[crVar],nameMap,set[crVar]) => set[crVar].
  freeLabelVars(Q,Map,Fr) =>
    foldRight((V,FF)=>freeThetaVar(V,Map,FF),Fr,Q).

  freeThetaVar:(crVar,nameMap,set[crVar]) => set[crVar].
  freeThetaVar(crId(Nm,_),Map,FF) where Entry ^= lookupVarName(Map,Nm) => let{
    checkEntry(localFun(_,_,Vr)) => _addMem(Vr,FF).
    checkEntry(localVar(crVar(_,Vr))) => _addMem(Vr,FF).
    checkEntry(localVar(crDot(_,crVar(_,V),_,_))) => freeThetaVar(V,Map,FF).
    checkEntry(labelArg(ThVr,TTp,_,_)) => freeThetaVar(crId(ThVr,TTp),Map,FF).
    checkEntry(_) => FF.
  } in checkEntry(Entry).
  freeThetaVar(V,_,FF) default => _addMem(V,FF).

  isModule(moduleVar(_,_,_))=>some(.true).
  isModule(globalVar(_,_))=>some(.true).
  isModule(_) default => .none.

  lookup:all e ~~ (nameMap,string,(nameMapEntry)=>option[e])=>option[e].
  lookup([],_,_) => .none.
  lookup([lyr(_,Entries,_),..Map],Nm,P) where E ^= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).

  lookupVarName:(nameMap,string)=>option[nameMapEntry].
  lookupVarName(Map,Nm) => lookup(Map,Nm,anyDef).

  layerName:(nameMap) => string.
  layerName([lyr(Nm,_,_),.._]) => Nm.
  layerName([]) => "".

  layerVar:(nameMap) => option[crVar].
  layerVar([lyr(_,_,V),.._]) => V.

  anyDef(D) => some(D).

  genVar:(string,tipe) => crVar.
  genVar(Pr,Tp) => crId(genSym(Pr),Tp).

  makeFunVars:(tipe)=>list[crVar].
  makeFunVars(Tp) where tupleType(Es).=funTypeArg(deRef(Tp)) => (Es//(E)=>genVar("_",E)).

  makeDotLbl:(string)=>string.
  makeDotLbl(Fld) => "."++Fld.

  crTpl:(locn,list[crExp]) => crExp.
  crTpl(Lc,Args) => let{
    Tp = tupleType(Args//typeOf).
    Ar = size(Args).
  } in crTerm(Lc,tplLbl(Ar),Args,Tp).

  crCon:(locn,string,list[crExp],tipe) => crExp.
  crCon(Lc,Nm,Args,Tp) => crTerm(Lc,Nm,Args,Tp).

  closureNm:(string)=>string.
  closureNm(Nm)=>Nm++"^".
  
}
