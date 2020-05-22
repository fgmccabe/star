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

  mapLayer ::= lyr(map[string,nameMapEntry]).

  nameMap ~> cons[mapLayer].

  implementation display[mapLayer] => {.
    disp(lyr(Entries)) =>
      disp(Entries)
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
    (V,Defs) <- transformDef(Prg,Map,[],.none,[],[],Rp);
--    logMsg("raw transformed def: $(V)");
    Bound .= (V//((Vr,Vl))=>vrDef(locOf(Vl),Vr,Vl));
    valis Bound++Defs
  }

  pkgMap:(pkgSpec) => nameMap.
  pkgMap(pkgSpec(Pkg,Imports,Tp,Cons,Impls,PkgVrs)) =>
    [lyr(foldRight(((Nm,ITp),D)=>D[Nm->globalVar(Nm,ITp)],[],PkgVrs))].

  letRecMap:(locn,cons[canonDef],canon,nameMap,set[crVar],reports) =>
    either[reports,(nameMap,set[crVar],option[crExp],crExp,cons[(crVar,crExp)])].
  letRecMap(Lc,Grp,Bnd,Outer,Q,Rp) => do{
--    logMsg("making map for group $(Grp)");
--    logMsg("outer map $(head(Outer))");
    rawGrpFree .= freeVarsInLetRec(Grp,Bnd,Q)::cons[crVar];
--    logMsg("raw group free $(rawGrpFree)");
    freeVars .=
      foldLeft((crId(Nm,Tp),So) =>
	  (_ ^= lookup(Outer,Nm,isModule) ? So || So\+crId(Nm,Tp)),[],
	rawGrpFree);
--    logMsg("group free vars $(freeVars)");

    GrpQ .= foldRight(collectQ,Q,Grp);

--    logMsg("GrpQ = $(GrpQ)");

    if isEmpty(freeVars) then {
      M .= [lyr(foldRight((D,LL)=>collectMtd(D,.none,LL),[],Grp)),..Outer];
--      logMsg("new group map $(head(M))");
      valis (M,GrpQ,.none,crTpl(Lc,[]),[])
    } else if [FrVr].=freeVars then {
      ThVr .= some(crVar(Lc,FrVr));
      FrTrm <- liftVarExp(Lc,crName(FrVr),typeOf(FrVr),Outer,Rp);
      L .= [crName(FrVr)->localVar(FrVr)]; -- protect against looking up more
      M .= [lyr(foldRight((D,LL)=>collectMtd(D,ThVr,LL),L,Grp)),..Outer];
--      logMsg("new group map $(head(M))\nfree var $(ThVr)");
      valis (M,GrpQ,ThVr,FrTrm,[])
    }
    else {
      ThV .= genVar("_ThVr",typeOf(freeVars));
--      logMsg("theta var is $(ThV)");
      ThVr .= some(crVar(Lc,ThV));
      L .= collectLabelVars(freeVars,ThV,0,[]);
--      logMsg("label vars $(L)");
    
      M .= [lyr(foldRight((D,LL)=>collectMtd(D,ThVr,LL),L,Grp)),..Outer];
--      logMsg("new group map $(head(M))");

      freeArgs <- seqmap((crId(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer,Rp),freeVars);
--      logMsg("free term $(crTpl(Lc,freeArgs))");
      valis (M,GrpQ,ThVr,crTpl(Lc,freeArgs),[(ThV,crTpl(Lc,freeArgs))])
    }
  }

  letMap:(locn,cons[canonDef],canon,nameMap,set[crVar],reports) =>
    either[reports,(nameMap,nameMap,set[crVar],option[crExp],crExp,cons[(crVar,crExp)])].
  letMap(Lc,Grp,Bnd,Outer,Q,Rp) => do{
    rawGrpFree .= freeVarsInLetGroup(Grp,Bnd,Q)::cons[crVar];
    freeVars .=
      foldLeft((crId(Nm,Tp),So) =>
	  (_ ^= lookup(Outer,Nm,isModule) ? So || So\+crId(Nm,Tp)),[],
	rawGrpFree);

    GrpQ .= foldRight(collectQ,Q,Grp);

    if isEmpty(freeVars) then {
      M .= [lyr(foldRight((D,LL)=>collectMtd(D,.none,LL),[],Grp)),..Outer];
      valis (Outer,M,GrpQ,.none,crTpl(Lc,[]),[])
    } else if [FrVr].=freeVars then {
      ThVr .= some(crVar(Lc,FrVr));
      L .= [crName(FrVr)->localVar(FrVr)]; -- protect against looking up more
      M .= [lyr(L),..Outer];
      MM .= [lyr(foldRight((D,LL)=>collectMtd(D,ThVr,LL),L,Grp)),..Outer];
      
      Fr <- liftVarExp(Lc,crName(FrVr),typeOf(FrVr),Outer,Rp);
      valis (M,MM,GrpQ,ThVr,Fr,[])
    }
    else {
      ThV .= genVar("_ThVr",typeOf(freeVars));
      ThVr .= some(crVar(Lc,ThV));
      L .= collectLabelVars(freeVars,ThV,0,[]);
    
      M .= [lyr(L),..Outer];
      MM .= [lyr(foldRight((D,LL)=>collectMtd(D,ThVr,LL),L,Grp)),..Outer];

      freeArgs <- seqmap((crId(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer,Rp),freeVars);
      valis (M,MM,GrpQ,ThVr,crTpl(Lc,freeArgs),[(ThV,crTpl(Lc,freeArgs))])
    }
  }
  
  lambdaMap:(canon,nameMap,set[crVar],reports) => either[reports,(nameMap,crExp,crExp)].
  lambdaMap(Lam,Outer,Q,Rp) => do{
    LF .= freeVarsInTerm(Lam,[],Q,[]);

--    logMsg("raw free vars in lambda $(LF)");
    
    freeVars .=
      foldLeft((crId(Nm,Tp),So) =>
	  (_ ^= lookup(Outer,Nm,isModule) ? So || So\+crId(Nm,Tp)),[],
	LF);

--    logMsg("free vars in lambda $(freeVars)");

    Lc .= locOf(Lam);

    if [FrVr].=freeVars then{
      ThVr .= some(crVar(Lc,FrVr));
      FrTrm <- liftVarExp(Lc,crName(FrVr),typeOf(FrVr),Outer,Rp);

      L .= [crName(FrVr)->localVar(FrVr)]; -- protect against looking up more
      M .= [lyr(L),..Outer];
      valis (M,crVar(Lc,FrVr),FrTrm)
    } else {
      ThV .= genVar("_ThVr",typeOf(freeVars));
      freeArgs <- seqmap((crId(Nm,Tp))=>liftVarExp(Lc,Nm,Tp,Outer,Rp),freeVars);
      L .= collectLabelVars(freeVars,ThV,0,[]);
      M .= [lyr(L),..Outer];
      valis (M,crVar(Lc,ThV),crTpl(Lc,freeArgs))
    }
  }

  liftLetExp:(locn,cons[canonDef],canon,nameMap,set[crVar],cons[crDefn],reports) =>
    either[reports,crFlow].
  liftLetExp(Lc,Defs,Bnd,Map,Q,Ex,Rp) => do{
--    logMsg("let $(Lc)");
    (GMap,BMap,GrpQ,GrpVr,GrpFree,IVs) <- letMap(Lc,Defs,Bnd,Map,Q,Rp);
--    logMsg("group map $(head(GMap))");
--    logMsg("free term $(GrpVr) = $(GrpFree)");
    (Vs,Ex1) <- transformGroup(Defs,GMap,GrpQ,GrpVr,Ex,[],Rp);
--    logMsg("let Vs=$(Vs)");

--    logMsg("lifting bound exp $(Bnd)");
    (BndTrm,Ex2) <- liftExp(Bnd,BMap,GrpQ,Ex1,Rp);
--    logMsg("Bound exp s=$(BndTrm)");

    Bound .= foldRight(((Vr,Vl),X)=>
	((crVar(_,VV).=Vl && VV==Vr) ? X || crLtt(locOf(Vl),Vr,Vl,X)),BndTrm,IVs++Vs);

    if crVar(_,GrpThVr) ^= GrpVr && !(crVar(_,GrpThVr).=GrpFree) then
      valis (crLtt(Lc,GrpThVr,GrpFree,Bound),Ex2)
    else
    valis (Bound,Ex2)
  }

  liftLetRec:(locn,cons[canonDef],canon,nameMap,set[crVar],cons[crDefn],reports) =>
    either[reports,crFlow].
  liftLetRec(Lc,Defs,Bnd,Map,Q,Ex,Rp) => do{
--    logMsg("lift let rec $(Defs) in $(Bnd), Q=$(Q)");
    (GMap,GrpQ,GrpVr,GrpFree,IVs) <- letRecMap(Lc,Defs,Bnd,Map,Q,Rp);
--    logMsg("group map $(GMap)");
--    logMsg("free term $(GrpVr) = $(GrpFree)");

    (Vs,Ex1) <- transformGroup(Defs,GMap,GrpQ,GrpVr,Ex,[],Rp);

--    logMsg("let rec Vs=$(Vs)");

    (BndTrm,Ex2) <- liftExp(Bnd,GMap,GrpQ,Ex1,Rp);

    Bound .= foldRight(((Vr,Vl),X)=>
	(crVar(_,VV).=Vl && VV==Vr ? X || crLtRec(locOf(Vl),Vr,Vl,X)),BndTrm,IVs++Vs);

--    logMsg("Bound let rec for $(Defs) = $(Bound)");
    valis (Bound,Ex2)
  }

  isFunctions:(cons[canonDef])=>boolean.
  isFunctions(Defs) => varDef(_,_,_,Vl,_,_) in Defs *> isFunDef(Vl).

  transformGroup:(cons[canonDef],nameMap,set[crVar],option[crExp],cons[crDefn],cons[(crVar,crExp)],reports) => either[reports,(cons[(crVar,crExp)],cons[crDefn])].
  transformGroup([],Map,_,_,D,Vs,_) => either((Vs,D)).
  transformGroup([D,..Ds],Map,Q,Extra,Ex,V,Rp) => do {
    (V1,Ex1) <- transformDef(D,Map,Q,Extra,[],V,Rp);
--    logMsg("transformed def is $(V1)\:$(Ex1)");
    transformGroup(Ds,Map,Q,Extra,Ex++Ex1,V1,Rp)
  }

  transformDef:(canonDef,nameMap,set[crVar],option[crExp],cons[crDefn],cons[(crVar,crExp)],reports) =>
    either[reports,(cons[(crVar,crExp)],cons[crDefn])].
  transformDef(varDef(Lc,Nm,FullNm,lambda(FullNm,Eqns,Tp),_,_),Map,Q,Extra,Ex,Vs,Rp) => do{
--    logMsg("transform function $(Nm) - $(Eqns)");
--    logMsg("free vars in function: $(Nm) are $(Extra)");
    ATp .= extendFunTp(deRef(Tp),Extra);
    (Eqs,Ex1) <- transformEquations(Eqns,Map,Q,Extra,Ex,Rp);
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
  transformDef(varDef(Lc,Nm,FullNm,Val,_,Tp),Map,Q,.none,Ex,Vs,Rp) => do{
    (Vl,Exx) <- liftExp(Val,Map,Q,Ex,Rp);
    Vr .= crId(Nm,Tp);
--    logMsg("global $(Nm)\[$(FullNm)] lifts to $(Vl)");

    valis (Vs/*[(Vr,Vl),..Vs]*/,[vrDef(Lc,crId(FullNm,Tp),Vl),..Exx])
  }
  transformDef(varDef(Lc,Nm,FullNm,Val,_,Tp),Map,Q,_,Ex,Vs,Rp) => do{
    (Vl,Exx) <- liftExp(Val,Map,Q,Ex,Rp);
    Vr .= crId(Nm,Tp);
--    logMsg("var $(Nm)\[$(FullNm)] lifts to $(Vl)");

    valis ([(Vr,Vl),..Vs],Exx)
  }
  transformDef(implDef(Lc,_,FullNm,Val,Cx,Tp),Map,Q,Extra,Ex,Vs,Rp) => do{
--    logMsg("transform implementation $(FullNm) = $(Val)");
    transformDef(varDef(Lc,FullNm,FullNm,Val,Cx,Tp),Map,Q,Extra,Ex,Vs,Rp)
  }.
  transformDef(_,_,_,_,Ex,Vs,_) => either((Vs,Ex)).

  transformEquations:(cons[equation],nameMap,set[crVar],option[crExp],cons[crDefn],reports) =>
    either[reports,(cons[(locn,cons[crExp],option[crExp],crExp)],cons[crDefn])].
  transformEquations([],_,_,_,Ex,_) => either(([],Ex)).
  transformEquations([Eqn,..Eqns],Map,Q,Extra,Ex,Rp) => do{
    (Trple,Ex1) <- transformEquation(Eqn,Map,Q,Extra,Ex,Rp);
    (Rest,Exx) <- transformEquations(Eqns,Map,Q,Extra,Ex1,Rp);
    valis ([Trple,..Rest],Exx)
  }

  addExtra(.none,Args) => Args.
  addExtra(some(P),Args) => [P,..Args].

  transformEquation:(equation,nameMap,set[crVar],option[crExp],cons[crDefn],reports) =>
    either[reports,((locn,cons[crExp],option[crExp],crExp),cons[crDefn])].
  transformEquation(eqn(Lc,tple(ALc,As),.none,Val),Map,Q,Extra,Ex,Rp) => do{
--    logMsg("transform $(eqn(Lc,tple(ALc,As),.none,Val))");
    EQ .= ptnVars(tple(ALc,As),Q,[]);
--    logMsg("EQ=$(EQ), Q=$(Q)");
    (Ptns,Ex1) <- liftPtns(As,Map,Q,Ex,Rp);
    (Rep,Exx) <- liftExp(Val,Map,EQ,Ex1,Rp);
    valis ((Lc,addExtra(Extra,Ptns),.none,Rep),Exx)
  }
  transformEquation(eqn(Lc,tple(ALc,As),some(Wh),Val),Map,Q,Extra,Ex,Rp) => do{
--    logMsg("transform $(eqn(Lc,tple(ALc,As),some(Wh),Val))");
    (Ptns,Ex1) <- liftPtns(As,Map,Q,Ex,Rp);
    EQ .= ptnVars(tple(Lc,As),Q,[]);
--    logMsg("EQ=$(EQ), Q=$(Q)");
    (Cond,Ex2) <- liftGoal(Wh,Map,EQ,Ex1,Rp);
    EQ2 .= glVars(Wh,EQ);
--    logMsg("EQ2=$(EQ2)");
    (Rep,Exx) <- liftExp(Val,Map,EQ2,Ex2,Rp);

    valis ((Lc,addExtra(Extra,Ptns),some(Cond),Rep),Exx)
  }

  liftPtn:(canon,nameMap,set[crVar],cons[crDefn],reports) => either[reports,crFlow].
  liftPtn(vr(Lc,Nm,Tp),Map,_,Ex,Rp) => trVarPtn(Lc,Nm,Tp,Map,Ex,Rp).
  liftPtn(enm(Lc,FullNm,Tp),Map,_,Ex,Rp) => either((crLbl(Lc,FullNm,Tp),Ex)).
  liftPtn(intr(Lc,Ix),Map,_,Ex,Rp) => either((crInt(Lc,Ix),Ex)).
  liftPtn(flt(Lc,Dx),Map,_,Ex,Rp) => either((crFlot(Lc,Dx),Ex)).
  liftPtn(strng(Lc,Sx),Map,_,Ex,Rp) => either((crStrg(Lc,Sx),Ex)).
  liftPtn(whr(Lc,Ptn,Cond),Map,Q,Ex,Rp) => do{
--    logMsg("where ptn $(whr(Lc,Ptn,Cond)), Q=$(Q)");
    (LPtn,Ex1) <- liftPtn(Ptn,Map,Q,Ex,Rp);
    EQ .= ptnVars(Ptn,Q,[]);
--    logMsg("EQ=$(EQ)");
    
    (LCond,Exx) <- liftGoal(Cond,Map,EQ,Ex,Rp);
    valis (crWhere(Lc,LPtn,LCond),Exx)
  }
  liftPtn(tple(Lc,Els),Map,Q,Ex,Rp) => do{
    (LEls,Exx) <- liftPtns(Els,Map,Q,Ex,Rp);
    valis (crTpl(Lc,LEls),Exx)
  }
  liftPtn(apply(Lc,vr(VLc,VNm,_),tple(_,Els),Tp),Map,Q,Ex,Rp) => do{
    (LArgs,Ex1) <- liftPtns(Els,Map,Q,Ex,Rp);
    liftPtnCallOp(Lc,VNm,LArgs,Tp,Map,Q,Ex1,Rp)
  }
  liftPtn(apply(Lc,enm(VLc,FullNm,_),tple(_,Els),Tp),Map,Q,Ex,Rp) => do{
    (LArgs,Ex1) <- liftPtns(Els,Map,Q,Ex,Rp);

    valis (crTerm(Lc,FullNm,LArgs,Tp),Ex1)
  }
  
  liftPtns:(cons[canon],nameMap,set[crVar],cons[crDefn],reports) => either[reports,(cons[crExp],cons[crDefn])].
  liftPtns([],_,_,Ex,_) => either(([],Ex)).
  liftPtns([P,..Ps],Map,Q,Ex,Rp) => do{
    (A,Ex1) <- liftPtn(P,Map,Q,Ex,Rp);
    (As,Exx) <- liftPtns(Ps,Map,Q,Ex1,Rp);
    valis ([A,..As],Exx)
  }

  liftPtnCallOp:(locn,string,cons[crExp],tipe,nameMap,set[crVar],cons[crDefn],reports) =>
    either[reports,(crExp,cons[crDefn])].
  liftPtnCallOp(Lc,Nm,Args,Tp,Map,Q,Ex,Rp) where Entry^= lookupVarName(Map,Nm) =>
    implementPtnCall(Lc,Entry,Args,Tp,Map,Q,Ex,Rp).

  implementPtnCall(Lc,moduleCons(Nm,CTp),Args,Tp,_,_,Ex,_) => either((crTerm(Lc,Nm,Args,Tp),Ex)).
  
  trVarPtn(Lc,Nm,Tp,Map,Ex,Rp) => implementVarPtn(Lc,Nm,lookupVarName(Map,Nm),Tp,Map,Ex,Rp).

  implementVarPtn(Lc,Nm,.none,Tp,_,Ex,_) => either((crVar(Lc,crId(Nm,Tp)),Ex)).
  implementVarPtn(Lc,Nm,some(moduleCons(Enum,CTp)),Tp,_,Ex,_) where ETp^=isEnumType(CTp) =>
    either((crLbl(Lc,Enum,ETp),Ex)).
  implementVarPtn(Lc,Nm,some(labelArg(Th,Ix,_)),Tp,_,Ex,Rp) where
      NN .= crVar(Lc,crId(Nm,Tp)) =>
    either((crWhere(Lc,NN,crMatch(Lc,NN,crTplOff(Lc,crVar(Lc,Th),Ix,Tp))),Ex)).

  implementVarPtn(Lc,Nm,some(V),Tp,Map,_,Rp) => other(reportError(Rp,"not permitted to match against $(Nm)\:$(V)",Lc)).

  liftExp:(canon,nameMap,set[crVar],cons[crDefn],reports) => either[reports,crFlow].
  liftExp(vr(Lc,Nm,Tp),Map,Q,Ex,Rp) => do{
    V <- liftVarExp(Lc,Nm,Tp,Map,Rp);
    valis (V,Ex)
  }
  liftExp(intr(Lc,Ix),_,Map,Ex,Rp) => either((crInt(Lc,Ix),Ex)).
  liftExp(flt(Lc,Dx),_,Map,Ex,Rp) => either((crFlot(Lc,Dx),Ex)).
  liftExp(strng(Lc,Sx),_,Map,Ex,Rp) => either((crStrg(Lc,Sx),Ex)).
  liftExp(enm(Lc,FullNm,Tp),_,Map,Ex,Rp) => either((crLbl(Lc,FullNm,Tp),Ex)).
  liftExp(tple(Lc,Els),Q,Map,Ex,Rp) => do{
    (LEls,Exx) <- liftExps(Els,Q,Map,Ex,Rp);
    valis (mkCrTpl(Lc,LEls),Exx)
  }
  liftExp(apply(Lc,Op,tple(_,Els),Tp),Q,Map,Ex,Rp) => do{
    (LEls,Ex1) <- liftExps(Els,Q,Map,Ex,Rp);
    liftExpCallOp(Lc,Op,LEls,Tp,Q,Map,Ex1,Rp)
  }
  liftExp(dot(Lc,Rc,Fld,Tp),Map,Q,Ex,Rp) => do{
    (LRc,Ex1) <- liftExp(Rc,Map,Q,Ex,Rp);
    valis (crDot(Lc,LRc,Fld,Tp),Ex1)
  }
  liftExp(whr(_,E,enm(_,"star.core#true",nomnal("star.core*boolean"))),Map,Q,Ex,Rp) =>
    liftExp(E,Map,Q,Ex,Rp).
  liftExp(whr(Lc,E,C),Map,Q,Ex,Rp) => do{
    (LE,Ex1) <- liftExp(E,Map,Q,Ex,Rp);
    (LC,Ex2) <- liftGoal(C,Map,Q,Ex1,Rp);
    valis (crWhere(Lc,LE,LC),Ex2)
  }
  liftExp(E,Map,Q,Ex,Rp) where isGoal(E) => liftGoal(E,Map,Q,Ex,Rp).
  liftExp(cond(Lc,Ts,Th,El),Map,Q,Ex,Rp) => do{
--    logMsg("transform conditional exp $(cond(Lc,Ts,Th,El)), Q=$(Q)");
    (LTs,Ex1) <- liftGoal(Ts,Map,Q,Ex,Rp);
    Q1 .= glVars(Ts,Q);
--    logMsg("Q1=$(Q1)");
    (LTh,Ex2) <- liftExp(Th,Map,Q1,Ex1,Rp);
    (LEl,Exx) <- liftExp(El,Map,Q,Ex2,Rp);
    valis (crCnd(Lc,LTs,LTh,LEl),Exx)
  }
  liftExp(record(Lc,some(Nm),Fields,Tp),Map,Q,Ex,Rp) => do{
--    logMsg("lift record $(record(Lc,some(Nm),Fields,Tp))");
    (LFields,Exx) <- liftFields(Fields,Map,Q,Ex,Rp);
    valis (crRecord(Lc,Nm,LFields,Tp),Exx)
  }
  liftExp(record(Lc,.none,Fields,Tp),Map,Q,Ex,Rp) => do{
--    logMsg("lift record $(record(Lc,.none,Fields,Tp))");
    (LFields,Exx) <- liftFields(Fields,Map,Q,Ex,Rp);
    Path .= genSym("record");
    valis (crRecord(Lc,Path,LFields,Tp),Exx)
  }
  liftExp(letExp(Lc,Grp,Bnd),Map,Q,Ex,Rp) => 
    liftLetExp(Lc,Grp,Bnd,Map,Q,Ex,Rp).
  liftExp(letRec(Lc,Grp,Bnd),Map,Q,Ex,Rp) => 
    liftLetRec(Lc,Grp,Bnd,Map,Q,Ex,Rp).
  liftExp(Lam where lambda(FullNm,Eqns,Tp).=Lam,Map,Q,Ex,Rp) => do{
    Lc .= locOf(Lam);
--    logMsg("transform lambda $(Lam), Q=$(Q)");
    (LMap,Extra,FreeTerm) <- lambdaMap(Lam,Map,Q,Rp);
--    logMsg("lambda map $(head(LMap))");
--    logMsg("extra vars in lambda: $(Extra)");
    ATp .= extendFunTp(deRef(Tp),some(Extra));
    (Eqs,Ex1) <- transformEquations(Eqns,LMap,Q,some(Extra),Ex,Rp);
    LamFun .= functionMatcher(Lc,FullNm,ATp,Eqs);
--    logMsg("lifted lambda fun $(LamFun)");
--    logMsg("lambda closure $(crTerm(Lc,FullNm,[FreeTerm],ATp))");
    valis (crTerm(Lc,FullNm,[FreeTerm],ATp),[LamFun,..Ex1])
  }
  liftExp(csexp(Lc,Gov,Cses,Tp),Map,Q,Ex,Rp) => do{
    (LGov,Ex1) <- liftExp(Gov,Map,Q,Ex,Rp);
    (Cs,Ex2) <- transformEquations(Cses,Map,Q,.none,Ex1,Rp);
    if crVar(_,_).=LGov then{
      Reslt .= caseMatcher(Lc,LGov,Cs,Tp);
      valis (Reslt,Ex2)
    } else {
      V .= genVar("C",typeOf(LGov));
      Res .= caseMatcher(Lc,crVar(Lc,V),Cs,Tp);
      valis (crLtt(Lc,V,LGov,Res),Ex2)
    }
  }

  closureType:(cons[tipe],tipe)=>tipe.
  closureType(Els,T) => consType(tupleType(Els),T).

  liftExps:(cons[canon],nameMap,set[crVar],cons[crDefn],reports) => either[reports,(cons[crExp],cons[crDefn])].
  liftExps([],_,_,Ex,_) => either(([],Ex)).
  liftExps([P,..Ps],Q,Map,Ex,Rp) => do{
    (A,Ex1) <- liftExp(P,Q,Map,Ex,Rp);
    (As,Exx) <- liftExps(Ps,Q,Map,Ex1,Rp);
    valis ([A,..As],Exx)
  }

  liftFields:(cons[(string,canon)],nameMap,set[crVar],cons[crDefn],reports) =>
    either[reports,(cons[(string,crExp)],cons[crDefn])].
  liftFields([],_,_,Ex,_) => either(([],Ex)).
  liftFields([(N,P),..Ps],Map,Q,Ex,Rp) => do{
    (A,Ex1) <- liftExp(P,Map,Q,Ex,Rp);
    (As,Exx) <- liftFields(Ps,Map,Q,Ex1,Rp);
    valis ([(N,A),..As],Exx)
  }
  
  liftExpCallOp:(locn,canon,cons[crExp],tipe,nameMap,set[crVar],cons[crDefn],reports) =>
    either[reports,crFlow].
  liftExpCallOp(Lc,vr(_,Nm,_),Args,Tp,Map,_,Ex,Rp) where isEscape(Nm) =>
    either((crECall(Lc,Nm,Args,Tp),Ex)).
  liftExpCallOp(Lc,vr(_,Nm,_),Args,Tp,Map,_,Ex,Rp) where (_,Op) ^= intrinsic(Nm) =>
    either((crIntrinsic(Lc,Op,Args,Tp),Ex)).
  liftExpCallOp(Lc,vr(_,Nm,_),Args,Tp,Map,_,Ex,Rp) where Entry ^= lookupVarName(Map,Nm) =>
    implementFunCall(Lc,Entry,Nm,Args,Tp,Map,Ex,Rp).
  liftExpCallOp(Lc,enm(_,FullNm,_),[],Tp,Map,_,Ex,Rp) =>
    either((crLbl(Lc,FullNm,Tp),Ex)).
  liftExpCallOp(Lc,enm(_,FullNm,_),Args,Tp,Map,_,Ex,Rp) =>
    either((crTerm(Lc,FullNm,Args,Tp),Ex)).
  liftExpCallOp(Lc,Op,Args,Tp,Map,Q,Ex,Rp) => do{
    (LOp,Ex0) <- liftExp(Op,Map,Q,Ex,Rp);
    valis (crOCall(Lc,LOp,Args,Tp),Ex0)
  }
  liftExpCallOp(Lc,Op,Args,Tp,_,_,_,Rp) =>
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

  liftGoal:(canon,nameMap,set[crVar],cons[crDefn],reports) => either[reports,crFlow].
  liftGoal(conj(Lc,L,R),Map,Q,Ex,Rp) => do{
    (LL,Ex1) <- liftGoal(L,Map,Q,Ex,Rp);
    (LR,Ex2) <- liftGoal(R,Map,glVars(L,Q),Ex1,Rp);
    valis (crCnj(Lc,LL,LR),Ex2)
  }
  liftGoal(disj(Lc,L,R),Map,Q,Ex,Rp) => do{
    (LL,Ex1) <- liftGoal(L,Map,Q,Ex,Rp);
    (LR,Ex2) <- liftGoal(R,Map,Q,Ex1,Rp);
    valis (crDsj(Lc,LL,LR),Ex2)
  }
  liftGoal(neg(Lc,R),Map,Q,Ex,Rp) => do{
    (LR,Ex1) <- liftGoal(R,Map,Q,Ex,Rp);
    valis (crNeg(Lc,LR),Ex1)
  }
  liftGoal(cond(Lc,T,L,R),Map,Q,Ex,Rp) => do{
--    logMsg("transform conditional goal $(cond(Lc,T,L,R)), Q=$(Q)");
    (LT,Ex1) <- liftGoal(T,Map,Q,Ex,Rp);
    Q1 .= glVars(T,Q);
--    logMsg("Q1=$(Q1)");
    (LL,Ex2) <- liftGoal(L,Map,Q1,Ex1,Rp);
    (LR,Ex3) <- liftGoal(R,Map,Q,Ex2,Rp);
    valis (crCnd(Lc,LT,LL,LR),Ex3)
  }
  liftGoal(match(Lc,P,E),Map,Q,Ex,Rp) => do{
    (LP,Ex1) <- liftPtn(P,Map,Q,Ex,Rp);
    (LE,Ex2) <- liftExp(E,Map,Q,Ex1,Rp);
    valis (crMatch(Lc,LP,LE),Ex2)
  }
  liftGoal(serch(Lc,Ptn,Gen,Iter),Map,_,_,Rp) =>
    other(reportError(Rp,"cannot deal with search condition",Lc)).
  liftGoal(G,Map,Q,Ex,Rp) =>
    liftExp(G,Map,Q,Ex,Rp).

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
    LL[Nm->globalVar(FullNm,Tp)].
  collectMtd(implDef(Lc,_,FullNm,Val,Cx,Tp),ThVr,LL) =>
    collectMtd(varDef(Lc,FullNm,FullNm,Val,Cx,Tp),ThVr,LL).
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),ThVr,LL) => LL[Nm->moduleCons(FullNm,Tp)].
  collectMtd(typeDef(_,_,_,_),_,LL) => LL.
  collectMtd(conDef(_,_,_,_),_,LL) => LL.

  collectQ:(canonDef,set[crVar]) => set[crVar].
  collectQ(varDef(Lc,Nm,FullNm,Val,_,Tp),Q) => Q\+crId(Nm,Tp).
  collectQ(implDef(Lc,_,FullNm,Val,_,Tp),Q) => Q\+crId(FullNm,Tp).
  collectQ(cnsDef(_,Nm,FullNm,Tp),Q) => Q.
  collectQ(typeDef(_,_,_,_),Q) => Q.
  collectQ(conDef(_,_,_,_),Q) => Q.

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
  lookup([lyr(Entries),..Map],Nm,P) where E ^= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).

  lookupVarName:(nameMap,string)=>option[nameMapEntry].
  lookupVarName(Map,Nm) => lookup(Map,Nm,anyDef).

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
