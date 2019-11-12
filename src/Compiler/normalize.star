star.compiler.normalize{
  import star.

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
    disp(labelArg(Base,Ix,Vr)) => ssSeq([ss("label arg"),disp(Vr),ss("@"),disp(Base),ss("["),disp(Ix),ss("]")]).
  .}  

  crFlow ~> (crExp,list[crDefn]).

  extraVars:(nameMap)=>list[crVar].
  extraVars([lyr(_,_,none),.._]) => [].
  extraVars([lyr(_,_,some(V)),.._]) => [V].

  implementation hasType[nameMapEntry]=>{.
    typeOf(moduleVar(_,Tp)) => Tp.
    typeOf(localVar(_,_,_,Tp,_)) => Tp.
    typeOf(moduleCons(_,Tp)) => Tp.
    typeOf(labelArg(_,_,V)) => typeOf(V).
  .}

  public normalize:(canon,reports)=>either[reports,list[crDefn]].
  normalize(Th,Rp) => do{
    logMsg("normalizing $(Th)");
    (_,Defs) <- liftTheta(Th,[],[],[],Rp);
    valis Defs
  }

  liftTheta:(canon,set[crVar],nameMap,list[crDefn],reports) => either[reports,crFlow].
  liftTheta(Th,Q,Outer,Ex,Rp) => do{
    logMsg("nomrlizing theta $(Th)");
    (Map,FrTerm) <- thetaMap(Th,Q,Outer,Rp);
    logMsg("theta map: $(Map)");
    Defs <- transformThetaDefs(Th,Map,Outer,Ex,Rp);
    valis (FrTerm,Defs)
  }

  thetaMap:(canon,set[crVar],nameMap,reports) => either[reports,(nameMap,crExp)].
  thetaMap(Theta,Q,Outer,Rp) => do{
    logMsg("making map");
    ThFree = findFreeVars(Theta,Outer,Q);
    logMsg("free vars = $(ThFree)");
    CellVars = cellVars(Theta);
    FreeVars = CellVars++ThFree;
    ThVr = genVar("_ThVr",tupleType(FreeVars//typeOf));
    L = collectLabelVars(FreeVars,crVar(locOf(Theta),ThVr),0,[]);
    M = makeMtdMap(Theta,Outer,some(ThVr),L);
    Lc = locOf(Theta);
    valis (M,crTpl(Lc,FreeVars//(V)=>crVar(Lc,V)))
  }

  groupMap:(locn,list[canonDef],nameMap,reports) => either[reports,(nameMap,option[crVar],crExp)].
  groupMap(Lc,Grp,Outer,Rp) => do{
    logMsg("making map for group $(Grp)");
    FreeVars = findFreeVarsInGroup(Grp,Outer);
    logMsg("free vars = $(FreeVars)");

    if isEmpty(FreeVars) then {
      logMsg("no theta var for group $(Grp)");
      M = [lyr("",foldRight((D,LL)=>collectMtd(D,layerName(Outer),none,LL),[],Grp),none),..Outer];
      valis (M,none,crTpl(Lc,FreeVars//(V)=>crVar(Lc,V)))
    } else {
      ThV = genVar("_ThVr",tupleType(FreeVars//typeOf));
      logMsg("theta var for group $(Grp) is $(ThV)");
      L = collectLabelVars(FreeVars,crVar(Lc,ThV),0,[]);
      logMsg("new label vars $(L)");
      ThVr = some(ThV);
    
      M = [lyr("",foldRight((D,LL)=>collectMtd(D,layerName(Outer),ThVr,LL),L,Grp),ThVr),..Outer];
      valis (M,ThVr,crTpl(Lc,FreeVars//(V)=>crVar(Lc,V)))
    }
  }

  findFreeVarsInGroup:(list[canonDef],nameMap) => list[crVar].
  findFreeVarsInGroup(G,Map) => valof action{
    DfFr = freeVarsInGroup(G,[],[]);
    logMsg("DfFr=$(DfFr)");
    valis freeLabelVars(DfFr,Map,[])::list[crVar]
  }

  liftLetExp:(locn,list[canonDef],canon,nameMap,list[crDefn],reports) => either[reports,crFlow].
  liftLetExp(Lc,Defs,Bnd,Map,Ex,Rp) where isFunctions(Defs) => do{
    (GMap,GrpVr,GrpFree) <- groupMap(Lc,Defs,Map,Rp);
    logMsg("group map $(GMap)");
    (Ex1,Vars) <- transformGroup(Defs,GMap,Map,Ex,[],Rp);

    if isEmpty(Vars) then{
      (BndTrm,Ex2) <- liftExp(Bnd,GMap,Ex1,Rp);
      if GrpThVr ^= GrpVr then
	valis (crLet(Lc,GrpThVr,GrpFree,BndTrm),Ex2)
      else
      valis (BndTrm,Ex2)
    } else{
      ThV = genVar("_ThVr",tupleType(Vars//(V,_)=>typeOf(V)));
      logMsg("var theta var for group is $(ThV)");
      L = collectLabelVars(Vars//(V,_)=>V,crVar(Lc,ThV),0,[]);
      logMsg("new label vars $(L)");
      ThVr = some(ThV);
      BMap = [lyr("",L,some(ThVr)),..GMap];
      (BndTrm,Ex2) <- liftExp(Bnd,BMap,Ex1,Rp);
      if GrpThVr ^= GrpVr then
	valis (crLet(Lc,GrpThVr,GrpFree,crLet(Lc,ThV,crTpl(Lc,Vars),BndTrm)),Ex2)
      else
      valis (crLet(Lc,ThV,crTpl(Lc,Vars),BndTrm),Ex2)
    }
  }

  isFunctions:(list[canonDef])=>boolean.
  isFunctions(Defs) => varDef(_,_,Vl,_,_) in Defs *> lambda(_,_).=Vl.

  transformThetaDefs(theta(Lc,Path,Lbled,Groups,_,Tp),Map,Outer,Ex,Rp) => do{
    Exx := Ex;
    MMp := Map;
    FrTerms := ([]:set[crVar]);

    for Gp in Groups do{
      Df = definedProgs(MMp!);
      Lv = labelVars(MMp!);
      
      for D in Gp do {
	Ex0 <- transformThetaDef(D,Map,Outer,Exx!,Rp);
	Exx := Ex0
      }
    };
    valis Exx!
  }

  transformGroup:(list[canonDef],nameMap,nameMap,list[crDefn],list[(crExp,crExp)],reports) => either[reports,(list[crDefn],list[(crExp,crExp)]].
  transformGroup([],_,_,D,Vs,_) => either((D,Vs)).
  transformGroup([D,..Ds],Map,Outer,Ex,V,Rp) => do {
    (Ex1,V1) <- transformDef(D,Map,Ex,V,Rp);
    transformGroup(Ds,Map,Outer,Ex1,V1,Rp)
  }

  transformDef:(canonDef,nameMap,list[crDefn],list[(crExp,crExp)],reports) =>
    either[reports,(list[crDefn],list[(crExp,crExp)])].
  transformDef(varDef(Lc,Nm,FullNm,lambda(Eqns,Tp),_,_),Map,Ex,Vs,Rp) => do{
    Exx <- transformFunction(Eqns,Nm,FullNm,Lc,Tp,Map,Ex,Rp);
    valis (Exx,Vs)
  }
  transformDef(varDef(Lc,Nm,FullNm,Val,_,Tp),Map,Ex,Vs,Rp) => do{
    (Exx,Vl) <- liftExp(Val,Map,Ex,Rp);
    valis (Exx,[Vs..,(crVar(Lc,crId(Nm,Tp)),Vl)])
  }
  transformDef(_,_,Ex,Vs,_) => either((Ex,Vs)).

  transformThetaDef(varDef(Lc,Nm,FullNm,lambda(Eqns,Tp),_,_),Map,Outer,Ex,Rp) => 
    transformFunction(Eqns,Nm,FullNm,Lc,Tp,Map,Ex,Rp).
/*  transformThetaDef(varDef(Lc,Nm,FullNm,Val,_,Tp),Map,Outer,Ex,Rp) => 
      transformVarDef(Lc,Nm,FullNm,Val,Tp,Map,Ex,Rp).
*/
  transformThetaDef(typeDef(_,_,_,_),Map,Outer,Ex,Rp) => either(Ex).
  transformThetaDef(conDef(_,_,_,_),Map,Outer,Ex,Rp) => either(Ex).
  transformThetaDef(implDef(_,_,_,_,_),Map,Outer,Ex,Rp) => either(Ex).
  transformThetaDef(cnsDef(_,_,_,_),Map,Outer,Ex,Rp) => either(Ex).

  transformFunction:(list[equation],string,string,locn,tipe,
    nameMap,list[crDefn],reports) => either[reports,list[crDefn]].
  transformFunction(Eqns,Nm,FullNm,Lc,Tp,Map,Ex,Rp) => do{
    logMsg("transform function $(Nm)");
    Extra = extraVars(Map);
    logMsg("extra vars in function: $(Extra)");
    ATp = extendFunTp(deRef(Tp),Extra);
    LclPrg = lbl(FullNm,arity(ATp));
    (Eqs,Ex1) <- transformEquations(Eqns,Map,Extra,Ex,Rp);
    logMsg("normalized equations for $(Nm)\: $(Eqs)");
    valis [functionMatcher(Lc,FullNm,ATp,Eqs),..Ex1]
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

  implementPtnCall(Lc,moduleCons(Nm,CTp),Args,Tp,_,Ex,_) => either((crApply(Lc,crLbl(Lc,Nm,size(Args),CTp),Args,Tp),Ex)).
  
  trVarPtn(Lc,Nm,Tp,Map,Ex,Rp) => implementVarPtn(Lc,Nm,lookupVarName(Map,Nm),Tp,Map,Ex,Rp).

  implementVarPtn(Lc,Nm,none,Tp,_,Ex,_) => either((crVar(Lc,crId(Nm,Tp)),Ex)).
  implementVarPtn(Lc,Nm,some(moduleVar(_,_)),Tp,_,_,Rp) => other(reportError(Rp,"not permitted",Lc)).
  implementVarPtn(Lc,Nm,some(labelArg(Base,Ix,V)),Tp,Map,Ex,Rp) => do{
    valis (crWhere(Lc,crVar(Lc,V),crMatch(Lc,crVar(Lc,V),crTplDte(Lc,Base,Ix,Tp))),Ex)
  }

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
  liftExp(theta(Lc,Path,Anon,Defs,Others,Tp),Map,Ex,Rp) =>
    liftTheta(theta(Lc,Path,Anon,Defs,Others,Tp),[],Map,Ex,Rp).
  liftExp(record(Lc,Path,Fields,Tp),Map,Ex,Rp) => do{
    (LFields,Exx) <- liftFields(Fields,Map,Ex,Rp);
    valis (crRecord(Lc,Path,LFields,Tp),Exx)
  }

  liftExp(letExp(Lc,Grp,Bnd),Map,Ex,Rp) =>
    liftLetExp(Lc,Grp,Bnd,Map,Ex,Rp).

/*
    abstraction(locn,canon,canon,tipe) |
    act(locn,canonAction) | 
    lambda(list[equation],tipe) |
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
    either((crApply(Lc,crLbl(Lc,Fn,size(Args),FTp),Args,Tp),Ex)).

  liftVarExp:(locn,string,tipe,nameMap,list[crDefn],reports) => either[reports,crFlow].
  liftVarExp(Lc,Nm,Tp,Map,Ex,Rp) where Entry ^= lookupVarName(Map,Nm) =>
    implementVarExp(Lc,Entry,Tp,Map,Ex,Rp).
  liftVarExp(Lc,Nm,Tp,_,Ex,Rp) => either((crVar(Lc,crId(Nm,Tp)),Ex)).

  implementVarExp:(locn,nameMapEntry,tipe,nameMap,list[crDefn],reports) => either[reports,crFlow].
  implementVarExp(Lc,localVar(Vn,_,_,VTp,ThVr),Tp,Map,Ex,Rp) =>
    either((crCall(Lc,crLbl(Lc,Vn,1,VTp),[crVar(Lc,ThVr)],Tp),Ex)).
  implementVarExp(Lc,labelArg(Base,Ix,ThVr),Tp,Map,Ex,Rp) =>
    either((crTplDte(Lc,Base,Ix,Tp),Ex)).
  implementVarExp(Lc,moduleCons(Enum,CTp),Tp,Map,Ex,Rp) =>
    either((crLbl(Lc,Enum,arity(CTp),Tp),Ex)).
  implementVarExp(Lc,moduleVar(FNm,FTp),Tp,Map,Ex,Rp) =>
    either((crApply(Lc,crLbl(Lc,FNm,0,FTp),[],Tp),Ex)).

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
    valis (crMtch(Lc,LP,LE),Ex2)
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

  makeMtdMap(theta(_,Nm,_,Defs,_,_),Outer,ThVr,L) =>
    [lyr(Nm,collectMtds(Defs,layerName(Outer),ThVr,L),ThVr),..Outer].

  collectMtds(Defs,Outer,ThVr,L) =>
    foldRight((Dfs,Li)=>foldRight((D,LL)=>collectMtd(D,Outer,ThVr,LL),Li,Dfs),L,Defs).

  collectMtd:(canonDef,string,option[crVar],map[string,nameMapEntry])=>map[string,nameMapEntry].
  collectMtd(varDef(Lc,Nm,FullNm,_,_,Tp),Outer,some(ThVr),LL) =>
    LL[Nm->localVar(FullNm,qualifiedName(Outer,"%",Nm),qualifiedName(Outer,"^",Nm),Tp,ThVr)].
  collectMtd(varDef(Lc,Nm,FullNm,_,_,Tp),Outer,none,LL) =>
    LL[Nm->moduleVar(qualifiedName(Outer,"%",Nm),Tp)].
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),Outer,ThVr,LL) =>
    LL[Nm->moduleCons(qualifiedName(Outer,"#",Nm),Tp)].
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

  makeFreeTerm:(set[crVar],set[crVar],nameMap,locn) => crExp.
  makeFreeTerm(CellVars,FreeVrs,Map,Lc) =>
    mkCrTpl(foldRight((V,FV) => [FV..,crVar(Lc,V)],
	foldRight((V,FV)=>[FV..,emptyCell(V,Lc)],[],CellVars),FreeVrs),Lc).

  emptyCell(V,Lc) => crVar(Lc,V).

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

  cellDefs:(canon) => list[canonDef].
  cellDefs(theta(_,_,_,Defs,_,_)) =>
    foldRight((Ds,CV)=>foldRight(pickCellDef,CV,Ds),[],Defs).

  pickCellDef:(canonDef,list[canonDef])=>list[canonDef].
  pickCellDef(Df where varDef(Lc,Nm,_,_,_,Tp).=Df,CellDefs) where isRefType(Tp) =>
    [CellDefs..,Df].
  pickCellDef(_,CellDefs) => CellDefs.
  
  cellVars:(canon) => list[crVar].
  cellVars(theta(_,_,_,Defs,_,_)) =>
    foldRight(cellVarsInGroup,[],Defs).

  cellVarsInGroup:(list[canonDef],list[crVar]) => list[crVar].
  cellVarsInGroup(Grp,CV) => foldRight(pickCellVar,CV,Grp).

  pickCellVar:(canonDef,list[crVar])=>list[crVar].
  pickCellVar(varDef(Lc,Nm,_,_,_,Tp),CellVars) where isRefType(Tp) =>
    [crId(Nm,Tp),..CellVars].
  pickCellVar(_,CellVars) => CellVars.

  varsInGroup:(list[canonDef],list[crVar]) => list[crVar].
  varsInGroup(Grp,CV) => foldRight(pickVar,CV,Grp).

  pickVar:(canonDef,list[crVar])=>list[crVar].
  pickVar(varDef(Lc,Nm,_,Val,_,Tp),CellVars) where \+ lambda(_,_).=Val =>
    [crId(Nm,Tp),..CellVars].
  pickVar(_,CellVars) => CellVars.

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
  } in crApply(Lc,crLbl(Lc,tplLbl(Ar),Ar,Tp),Args,Tp).
  
}
