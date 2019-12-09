star.compiler.normalize{
  import star.
  import star.pkg.

  import star.compiler.canon.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.freevars.
  import star.compiler.matcher.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  nameMapEntry ::= moduleVar(string,tipe)
    | localVar(string,string,string,tipe,crVar)
    | moduleCons(string,tipe)
    | labelArg(crExp,integer,crVar)
    | labelVar(crExp)
    | importVar(crExp,string,tipe)
    | letVar(string,crExp,tipe).

  mapLayer ::= lyr(string,map[string,nameMapEntry],option[crVar]).

  nameMap ~> list[mapLayer].

  implementation display[mapLayer] => {.
    disp(lyr(Path,Entries,ThVr)) =>
      ssSeq([ss(Path),ss(":"),disp(Entries),ss("\nTheta var: "),disp(ThVr),ss("\n")])
  .}

  implementation display[nameMapEntry] => {.
    disp(moduleVar(Nm,Tp)) => ssSeq([ss("module var "),ss(Nm),ss(":"),disp(Tp)]).
    disp(moduleCons(Nm,Tp)) => ssSeq([ss("module cons "),ss(Nm),ss(":"),disp(Tp)]).
    disp(localVar(Nm,_,_,Tp,V)) => ssSeq([ss("local var "),ss(Nm),ss("~"),disp(V),ss(":"),disp(Tp)]).
    disp(labelVar(Vr)) => ssSeq([ss("label var"),disp(Vr)]).
    disp(labelArg(Base,Ix,Vr)) => ssSeq([ss("label arg"),disp(Vr),ss("@"),disp(Base),ss("["),disp(Ix),ss("]")]).
    disp(importVar(M,Nm,Tp)) => ssSeq([ss("import "),disp(M),ss("#"),ss(Nm),ss(":"),disp(Tp)]).
  .}  

  crFlow ~> (crExp,list[crDefn]).

  extraVars:(nameMap)=>list[crVar].
  extraVars([lyr(_,_,none),.._]) => [].
  extraVars([lyr(_,_,some(V)),.._]) => [V].

  implementation hasType[nameMapEntry]=>{.
    typeOf(moduleVar(_,Tp)) => Tp.
    typeOf(localVar(_,_,_,Tp,_)) => Tp.
    typeOf(moduleCons(_,Tp)) => Tp.
    typeOf(labelVar(V))=>typeOf(V).
    typeOf(labelArg(_,_,V)) => typeOf(V).
  .}

  public normalize:(pkgSpec,canon,reports)=>either[reports,crFlow].
  normalize(PkgSpec,Prg,Rp) => do{
    Map = pkgMap(PkgSpec);
--    logMsg("package map $(Map)");
    liftExp(Prg,Map,[],Rp)
  }

  pkgMap:(pkgSpec) => nameMap.
  pkgMap(pkgSpec(Pkg,Imports,Tp,Defs,Impls,PkgVrs)) => [lyr(pkgName(Pkg),[],none)].

  groupMap:(locn,list[canonDef],nameMap,reports) => either[reports,(nameMap,option[crVar],crExp)].
  groupMap(Lc,Grp,Outer,Rp) => do{
--    logMsg("making map for group $(Grp), outer map=$(Outer)");
    FreeVars = findFreeVarsInGroup(Grp,Outer);
--    logMsg("free vars = $(FreeVars)");

    if isEmpty(FreeVars) then {
--      logMsg("no theta var for group $(Grp)");
      M = [lyr("",foldRight((D,LL)=>collectMtd(D,layerName(Outer),none,LL),[],Grp),none),..Outer];
      valis (M,none,crTpl(Lc,FreeVars//(V)=>crVar(Lc,V)))
    } else if [FrVr].=FreeVars then {
      ThVr = crVar(Lc,FrVr);
      L = [crName(FrVr)->labelVar(ThVr)];
      M = [lyr("",foldRight((D,LL)=>collectMtd(D,layerName(Outer),none,LL),L,Grp),none),..Outer];
      valis (M,none,ThVr)
    }
    else {
      ThV = genVar("_ThVr",tupleType(FreeVars//typeOf));
--      logMsg("theta var for group $(Grp) is $(ThV)");
      L = collectLabelVars(FreeVars,crVar(Lc,ThV),0,[]);
--      logMsg("new label vars $(L)");
      ThVr = some(ThV);
    
      M = [lyr("",foldRight((D,LL)=>collectMtd(D,layerName(Outer),ThVr,LL),L,Grp),ThVr),..Outer];
      valis (M,ThVr,crTpl(Lc,FreeVars//(V)=>crVar(Lc,V)))
    }
  }

  lambdaMap:(canon,nameMap,reports) => either[reports,(nameMap,option[crVar],list[crExp])].
  lambdaMap(Lam,Outer,Rp) => do{
--    logMsg("making map for lambda $(Lam)");
    freeVars = freeLabelVars(freeVarsInTerm(Lam,[],[]),Outer,[])::list[crVar];
    Lc = locOf(Lam);
    FullNm = genSym(layerName(Outer)++"λ");

--    logMsg("free vars = $(freeVars), full name $(FullNm)");

    if isEmpty(freeVars) then {
--      logMsg("no theta var for lambda");
      M = [lyr(FullNm,[],none),..Outer];
      valis (M,none,[])
    } else {
      ThV = genVar("_ThVr",tupleType(freeVars//typeOf));
--      logMsg("theta var is $(ThV)");
      L = collectLabelVars(freeVars,crVar(Lc,ThV),0,[]);
--      logMsg("new label vars $(L)");
      ThVr = some(ThV);
    
      M = [lyr(FullNm,L,ThVr),..Outer];
      valis (M,ThVr,freeVars//(V)=>crVar(Lc,V))
    }
  }

  findFreeVarsInGroup:(list[canonDef],nameMap) => list[crVar].
  findFreeVarsInGroup(G,Map) => 
    freeLabelVars(freeVarsInGroup(G,definedProgs(Map),[]),Map,[])::list[crVar].

  definedProgs:(nameMap)=>set[crVar].
  definedProgs(Map) => foldRight((lyr(_,Defs,_),Dfs)=>ixRight((_,Entry,DDfs)=>checkDefined(Entry,DDfs),Dfs,Defs),[],Map).

  checkDefined(moduleVar(Nm,Tp),Dfs) => _addMem(crId(Nm,Tp),Dfs).
  checkDefined(localVar(Nm,_,_,Tp,_),Dfs) => _addMem(crId(Nm,Tp),Dfs).
  checkDefined(importVar(_,Nm,Tp),Dfs) => _addMem(crId(Nm,Tp),Dfs).
  checkDefined(Entry,Dfs) default => Dfs.

  liftLetExp:(locn,list[canonDef],canon,nameMap,list[crDefn],reports) => either[reports,crFlow].
  liftLetExp(Lc,Defs,Bnd,Map,Ex,Rp) where isFunctions(Defs) => do{
    (GMap,GrpVr,GrpFree) <- groupMap(Lc,Defs,Map,Rp);
--    logMsg("function group map $(GMap)");
    (_,Ex1,BMap) <- transformGroup(Defs,GMap,[],[],Rp);

    (BndTrm,Ex2) <- liftExp(Bnd,BMap,Ex1,Rp);
    if GrpThVr ^= GrpVr then
      valis (foldRight((D,I)=>crLtt(locOf(D),D,I),crLtt(Lc,vrDef(Lc,GrpThVr,GrpFree),BndTrm),Ex2),Ex)
    else
    valis (foldRight((D,I)=>crLtt(locOf(D),D,I),BndTrm,Ex2),Ex)
  }
  liftLetExp(Lc,Defs,Bnd,Map,Ex,Rp) => do{
    (GMap,GrpVr,GrpFree) <- groupMap(Lc,Defs,Map,Rp);
--    logMsg("var group map $(GMap)");
    (Vars,Ex1,BMap) <- transformGroup(Defs,GMap,Ex,[],Rp);

    ThV = genVar("_ThVr",tupleType(Vars//((V,_))=>typeOf(V)));
--    logMsg("var theta var for group is $(ThV)");
    Vrs = Vars//((V,_))=>V;
    Vls = Vars//((_,V))=>V;
    L = collectLabelVars(Vrs//(crVar(_,V))=>V,crVar(Lc,ThV),0,[]);
--    logMsg("new label vars $(L)");
    ThVr = some(ThV);
    BBMap = [lyr("",L,ThVr),..BMap];
    (BndTrm,Ex2) <- liftExp(Bnd,BBMap,Ex1,Rp);

    if GrpThVr ^= GrpVr then
      valis (foldRight((D,I)=>crLtt(locOf(D),D,I),crLtt(Lc,vrDef(Lc,GrpThVr,GrpFree),BndTrm),Ex2),Ex)
    else
    valis (foldRight((D,I)=>crLtt(locOf(D),D,I),BndTrm,Ex2),Ex)
  }

  isFunctions:(list[canonDef])=>boolean.
  isFunctions(Defs) => varDef(_,_,Vl,_,_) in Defs *> lambda(_,_,_).=Vl.

  mkLetBinding:(locn,crVar,crExp,crExp) => crExp.
  mkLetBinding(_,crId(Nm,_),crVar(Lc,W),Exp) => rewriteTerm(Exp,[Nm->crVar(Lc,W)]).
  mkLetBinding(Lc,V,B,E) default => crLtt(Lc,vrDef(Lc,V,B),E).

  transformGroup:(list[canonDef],nameMap,list[crDefn],list[(crExp,crExp)],reports) => either[reports,(list[(crExp,crExp)],list[crDefn],nameMap)].
  transformGroup([],Map,D,Vs,_) => either((Vs,D,Map)).
  transformGroup([D,..Ds],Map,Ex,V,Rp) => do {
    logMsg("transform def $(D)");
    (V1,Ex1) <- transformDef(D,Map,Ex,V,Rp);
    logMsg("transformed def $(D) is $(V1)");
    transformGroup(Ds,Map,Ex1,V1,Rp)
  }

  transformDef:(canonDef,nameMap,list[crDefn],list[(crExp,crExp)],reports) =>
    either[reports,(list[(crExp,crExp)],list[crDefn])].
  transformDef(varDef(Lc,Nm,lambda(_,Eqns,Tp),_,_),Map,Ex,Vs,Rp) => do{
    Exx <- transformFunction(Eqns,Nm,Lc,Tp,Map,Ex,Rp);
    valis (Vs,Exx)
  }
  transformDef(varDef(Lc,Nm,Val,_,Tp),Map,Ex,Vs,Rp) => do{
    (Vl,Exx) <- liftExp(Val,Map,Ex,Rp);
    Vr = crId(Nm,Tp);
    valis ([Vs..,(crVar(Lc,Vr),Vl)],[Exx..,vrDef(Lc,Vr,Vl)])
  }
  transformDef(implDef(Lc,Nm,FullNm,Val,Tp),Map,Ex,Vs,Rp) =>
    transformDef(varDef(Lc,FullNm,Val,[],Tp),Map,Ex,Vs,Rp).
  transformDef(_,_,Ex,Vs,_) => either((Vs,Ex)).

  transformFunction:(list[equation],string,locn,tipe,
    nameMap,list[crDefn],reports) => either[reports,list[crDefn]].
  transformFunction(Eqns,Nm,Lc,Tp,Map,Ex,Rp) => do{
    logMsg("transform function $(Nm)\n$(Eqns)\:$(Tp)");
    Extra = extraVars(Map);
    logMsg("extra vars in function: $(Extra)");
    ATp = extendFunTp(deRef(Tp),Extra);
    (Eqs,Ex1) <- transformEquations(Eqns,Map,Extra,Ex,Rp);
    logMsg("normalized equations for $(Nm)\: $(Eqs)");
    valis [functionMatcher(Lc,Nm,ATp,Eqs),..Ex1]
  }

  transformEquations:(list[equation],nameMap,list[crVar],list[crDefn],reports) =>
    either[reports,(list[(locn,list[crExp],crExp)],list[crDefn])].
  transformEquations([],_,_,Ex,_) => either(([],Ex)).
  transformEquations([Eqn,..Eqns],Map,Extra,Ex,Rp) => do{
    (Trple,Ex1) <- transformEquation(Eqn,Map,Extra,Ex,Rp);
    (Rest,Exx) <- transformEquations(Eqns,Map,Extra,Ex1,Rp);
    valis ([Trple,..Rest],Exx)
  }

  transformEquation:(equation,nameMap,list[crVar],list[crDefn],reports) =>
    either[reports,((locn,list[crExp],crExp),list[crDefn])].
  transformEquation(eqn(Lc,tple(_,As),Val),Map,Extra,Ex,Rp) => do{
    (Ptns,Ex1) <- liftPtns(As,Map,Ex,Rp);
    (Rep,Exx) <- liftExp(Val,Map,Ex1,Rp);
    valis ((Lc,(Extra//(V)=>crVar(Lc,V))++Ptns,Rep),Exx)
  }

  transformVarDef:(locn,string,string,canon,tipe,nameMap,list[crDefn],reports) =>
    either[reports,list[crDefn]].

  liftPtn:(canon,nameMap,list[crDefn],reports) => either[reports,crFlow].
  liftPtn(vr(Lc,Nm,Tp),Map,Ex,Rp) => trVarPtn(Lc,Nm,Tp,Map,Ex,Rp).
  liftPtn(enm(Lc,Nm,Tp),Map,Ex,Rp) => trVarPtn(Lc,Nm,Tp,Map,Ex,Rp).
  liftPtn(intr(Lc,Ix),Map,Ex,Rp) => either((crInt(Lc,Ix),Ex)).
  liftPtn(flot(Lc,Dx),Map,Ex,Rp) => either((crFlot(Lc,Dx),Ex)).
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

  implementPtnCall(Lc,moduleCons(Nm,CTp),Args,Tp,_,Ex,_) => either((crTerm(Lc,crLbl(Lc,Nm,size(Args),CTp),Args,Tp),Ex)).
  
  trVarPtn(Lc,Nm,Tp,Map,Ex,Rp) => implementVarPtn(Lc,Nm,lookupVarName(Map,Nm),Tp,Map,Ex,Rp).

  implementVarPtn(Lc,Nm,none,Tp,_,Ex,_) => either((crVar(Lc,crId(Nm,Tp)),Ex)).
  implementVarPtn(Lc,Nm,some(_),Tp,_,_,Rp) => other(reportError(Rp,"not permitted to match against $(Nm)",Lc)).

  liftExp:(canon,nameMap,list[crDefn],reports) => either[reports,crFlow].
  liftExp(vr(Lc,Nm,Tp),Map,Ex,Rp) => liftVarExp(Lc,Nm,Tp,Map,Ex,Rp).
  liftExp(intr(Lc,Ix),Map,Ex,Rp) => either((crInt(Lc,Ix),Ex)).
  liftExp(flot(Lc,Dx),Map,Ex,Rp) => either((crFlot(Lc,Dx),Ex)).
  liftExp(strng(Lc,Sx),Map,Ex,Rp) => either((crStrg(Lc,Sx),Ex)).
  liftExp(enm(Lc,Nm,Tp),Map,Ex,Rp) => liftVarExp(Lc,Nm,Tp,Map,Ex,Rp).
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
    valis (crDte(Lc,LRc,makeDotLbl(Fld),Tp),Ex1)
  }
  liftExp(whr(_,E,enm(_,"true",nomnal("star.core*boolean"))),Map,Ex,Rp) =>
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
    logMsg("lift record $(record(Lc,Path,Fields,Tp))");
    (LFields,Exx) <- liftFields(Fields,Map,Ex,Rp);
    valis (crRecord(Lc,Path,LFields,Tp),Exx)
  }
  liftExp(letExp(Lc,Grp,Bnd),Map,Ex,Rp) =>
    liftLetExp(Lc,Grp,Bnd,Map,Ex,Rp).
  liftExp(Lam where lambda(Lc,Eqns,Tp).=Lam,Map,Ex,Rp) => do{
--    logMsg("transform lambda $(Lam)");
    (LMap,ClV,Cl) <- lambdaMap(Lam,Map,Rp);
    Extra = extraVars(LMap);
--    logMsg("extra vars in lambda: $(Extra)");
    ATp = extendFunTp(deRef(Tp),Extra);
    FullNm = layerName(LMap);
    (Eqs,Ex1) <- transformEquations(Eqns,LMap,Extra,[],Rp);
--    logMsg("normalized equations $(Eqs)");
    LamFun = functionMatcher(Lc,FullNm,ATp,Eqs);

    valis (foldRight((D,I)=>crLtt(locOf(D),D,I),crLtt(Lc,LamFun,crTpl(Lc,[crVar(Lc,crId(FullNm,ATp)),..Cl])),Ex1),Ex)
  }

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
  liftExpCallOp(Lc,vr(_,Nm,_),Args,Tp,Map,Ex,Rp) where Entry ^= lookupVarName(Map,Nm) =>
    implementFunCall(Lc,Entry,Nm,Args,Tp,Map,Ex,Rp).
  liftExpCallOp(Lc,enm(_,Nm,_),Args,Tp,Map,Ex,Rp) where Entry ^= lookupVarName(Map,Nm) =>
    implementFunCall(Lc,Entry,Nm,Args,Tp,Map,Ex,Rp).
  liftExpCallOp(Lc,Op,Args,Tp,Map,Ex,Rp) => do{
    (LOp,Ex0) <- liftExp(Op,Map,Ex,Rp);
    valis (crOCall(Lc,LOp,Args,Tp),Ex0)
  }
  liftExpCallOp(Lc,Op,Args,Tp,_,_,Rp) =>
    other(reportError(Rp,"cannot compile function $(Op) applied to $(Args)",Lc)).

  implementFunCall:(locn,nameMapEntry,string,list[crExp],tipe,nameMap,list[crDefn],reports) =>
    either[reports,crFlow].
  implementFunCall(Lc,localVar(Fn,_,_,FTp,crId(ThNm,ThTp)),_,Args,Tp,Map,Ex,Rp) => do{
    (ThVr,Ex1) <- liftVarExp(Lc,ThNm,ThTp,Map,Ex,Rp);
    valis (crCall(Lc,crLbl(Lc,Fn,size(Args)+1,FTp),[ThVr,..Args],Tp),Ex1)
  }
  implementFunCall(Lc,moduleVar(Fn,FTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,crLbl(Lc,Fn,size(Args),FTp),Args,Tp),Ex)).
  implementFunCall(Lc,moduleCons(Fn,FTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crTerm(Lc,crLbl(Lc,Fn,size(Args),FTp),Args,Tp),Ex)).
  implementFunCall(Lc,labelArg(Base,Ix,ThVr),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,crTplDte(Lc,Base,Ix,Tp),Args,Tp),Ex)).
  implementFunCall(Lc,labelVar(FrVr),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,FrVr,Args,Tp),Ex)).

  liftVarExp:(locn,string,tipe,nameMap,list[crDefn],reports) => either[reports,crFlow].
  liftVarExp(Lc,Nm,Tp,Map,Ex,Rp) where Entry ^= lookupVarName(Map,Nm) =>
    implementVarExp(Lc,Entry,Tp,Map,Ex,Rp).
  liftVarExp(Lc,Nm,Tp,_,Ex,Rp) => either((crVar(Lc,crId(Nm,Tp)),Ex)).

  implementVarExp:(locn,nameMapEntry,tipe,nameMap,list[crDefn],reports) => either[reports,crFlow].
  implementVarExp(Lc,localVar(Vn,_,_,VTp,ThVr),Tp,Map,Ex,Rp) =>
    either((crCall(Lc,crVar(Lc,crId(Vn,VTp)),[crVar(Lc,ThVr)],Tp),Ex)).
  implementVarExp(Lc,labelArg(Base,Ix,ThVr),Tp,Map,Ex,Rp) =>
    either((crTplDte(Lc,Base,Ix,Tp),Ex)).
  implementVarExp(Lc,labelVar(ThVr),Tp,Map,Ex,Rp) =>
    either((ThVr,Ex)).
  implementVarExp(Lc,moduleCons(Enum,CTp),Tp,Map,Ex,Rp) =>
    either((crLbl(Lc,Enum,arity(CTp),Tp),Ex)).
  implementVarExp(Lc,moduleVar(FNm,FTp),Tp,Map,Ex,Rp) =>
    either((crVar(Lc,crId(FNm,FTp)),Ex)).

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

  extendFunTp(Tp,[]) => Tp.
  extendFunTp(Tp,Vs) where (A,B)^=isFunType(Tp) &&
      tupleType(Es).=deRef(A) =>
    funType(tupleType(extendTplType(Es,Vs)),B).
  extendFunTp(allType(V,B),Vs) => allType(V,extendFunTp(B,Vs)).
  extendFunTp(existType(V,B),Vs) => existType(V,extendFunTp(B,Vs)).
  extendFunTp(constrainedType(T,C),Vs) => constrainedType(extendFunTp(T,Vs),C).

  extendTplType:(list[tipe],list[crVar]) => list[tipe].
  extendTplType(Es,[]) => Es.
  extendTplType(Es,[V,..Vs]) => [typeOf(V),..extendTplType(Es,Vs)].

  collectMtd:(canonDef,string,option[crVar],map[string,nameMapEntry])=>map[string,nameMapEntry].
  collectMtd(varDef(Lc,Nm,lambda(_,_,_),_,Tp),Outer,some(ThVr),LL) =>
    LL[Nm->localVar(Nm,qualifiedName(Outer,"%",Nm),qualifiedName(Outer,"^",Nm),Tp,ThVr)].
  collectMtd(varDef(Lc,Nm,lambda(_,_,_),_,Tp),Outer,none,LL) =>
    LL[Nm->moduleVar(Nm,Tp)].
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),Outer,ThVr,LL) => LL[Nm->moduleCons(FullNm,Tp)].
  collectMtd(_,_,_,LL) default => LL.

  findFreeVars:(canon,nameMap,set[crVar]) => list[crVar].
  findFreeVars(Term,Map,Q) => valof action{
    Df = definedProgs(Map);
    Lv = labelVars(Map);
    Q1 = Q\/Df\/Lv;
    ThFr = freeVarsInTerm(Term,Df,Q1);
    valis freeLabelVars(Q1,Map,ThFr)::list[crVar]
  }

  collectLabelVars:(list[crVar],crExp,integer,map[string,nameMapEntry]) => map[string,nameMapEntry].
  collectLabelVars([],_,_,LV) => LV.
  collectLabelVars([V,..Vrs],ThV,Ix,Entries) where crId(Nm,_) .= V =>
    collectLabelVars(Vrs,ThV,Ix+1,Entries[Nm->labelArg(ThV,Ix,V)]).

  definedProgs:(nameMap)=>set[crVar].
  definedProgs(Map) => let{
    defProgs([],Df) => Df.
    defProgs([lyr(_,Defs,_),..Mp],Df) =>
      defProgs(Mp,ixRight(definedInDefs,Df,Defs)).

    definedInDefs(Nm,Entry,Df) where definedP(Entry) =>
      (crId(Nm,_) in Df ?
	  Df ||
	  [crId(Nm,typeOf(Entry)),..Df]).
    definedInDefs(_,_,Df) => Df.

    definedP(moduleVar(_,_))=>true.
    definedP(localVar(_,_,_,_,_))=>true.
    definedP(_) default => false.
  } in defProgs(Map,[]).

  labelVars:(nameMap) => set[crVar].
  labelVars(Map) => lblVars(Map,[]).

  lblVars:(nameMap,set[crVar])=>set[crVar].
  lblVars([],Vrs) => Vrs.
  lblVars([lyr(_,Defs,none),..Map],Vrs) =>
    lblVars(Map,ixRight(labelVarsInDef,Vrs,Defs)).
  lblVars([lyr(_,Defs,some(ThVr)),..Map],Vrs) =>
    lblVars(Map,ixRight(labelVarsInDef,_addMem(ThVr,Vrs),Defs)).

  labelVarsIndef:(string,nameMapEntry,set[crVar])=>set[crVar].
  labelVarsInDef(_,labelArg(crVar(_,V),_,_),Vrs) => _addMem(V,Vrs).
  labelVarsInDef(_,_,Vrs) => Vrs.

  freeLabelVars:(set[crVar],nameMap,set[crVar]) => set[crVar].
  freeLabelVars(Q,Map,Fr) =>
    foldRight((V,FF)=>lookupThetaVar(V,Map,FF),Fr,Q).

  lookupThetaVar:(crVar,nameMap,set[crVar]) => set[crVar].
  lookupThetaVar(crId(Nm,_),Map,FF) where
      ThVr^=lookup(Map,Nm,isLocal) => _addMem(ThVr,FF).
  lookupThetaVar(crId(Nm,_),Map,FF) where
      ThVr^=lookup(Map,Nm,isModule) => FF.
  lookupThetaVar(V,_,FF) default => _addMem(V,FF).

  isLocal(localVar(_,_,_,_,ThVr))=>some(ThVr).
  isLocal(_) default => none.

  isModule(moduleVar(_,_))=>some(true).
  isModule(_) default => none.

  lookup:all e ~~ (nameMap,string,(nameMapEntry)=>option[e])=>option[e].
  lookup([],_,_) => none.
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

  makeDotLbl:(string)=>string.
  makeDotLbl(Fld) => "."++Fld.

  crTpl:(locn,list[crExp]) => crExp.
  crTpl(Lc,Args) => let{
    Tp = tupleType(Args//typeOf).
    Ar = size(Args).
  } in crTerm(Lc,crLbl(Lc,tplLbl(Ar),Ar,Tp),Args,Tp).

  crCon:(locn,string,list[crExp],tipe) => crExp.
  crCon(Lc,Nm,Args,Tp) => crTerm(Lc,crLbl(Lc,Nm,size(Args),Tp),Args,Tp).
  
}
