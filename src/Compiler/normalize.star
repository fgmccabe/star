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

  nameMapEntry ::= moduleFun(crExp,string)
    | localFun(string,string,crVar)
    | localVar(crExp)
    | moduleCons(string,tipe)
    | localCons(string,tipe,crVar)
    | labelArg(crVar,integer)
    | memoArg(crVar,integer)
    | globalVar(string,tipe).


  mapLayer ::= lyr(map[string,nameMapEntry]).

  nameMap ~> cons[mapLayer].

  implementation display[mapLayer] => {.
    disp(lyr(Entries)) =>
      disp(Entries)
  .}

  implementation display[nameMapEntry] => {.
    disp(moduleFun(C,V)) => ssSeq([ss("module fun "),disp(V),ss(", closure "),disp(C)]).
    disp(moduleCons(Nm,Tp)) => ssSeq([ss("module cons "),ss(Nm)]).
    disp(localCons(Nm,Tp,Vr)) => ssSeq([ss("local cons "),ss(Nm),ss("["),disp(Vr),ss("]")]).
    disp(localFun(Nm,ClNm,V)) => ssSeq([ss("local fun "),ss(Nm),ss(", closure "),disp(ClNm),
	ss(" ThV "),disp(V)]).
    disp(localVar(Vr)) => ssSeq([ss("local var "),disp(Vr)]).
    disp(labelArg(Base,Ix)) => ssSeq([ss("label arg "),disp(Base),ss("["),disp(Ix),ss("]")]).
    disp(memoArg(Base,Ix)) => ssSeq([ss("memo arg "),disp(Base),ss("["),disp(Ix),ss("]")]).
    disp(globalVar(Nm,Tp)) => ssSeq([ss("global "),ss(Nm)]).
  .}

  crFlow ~> (crExp,cons[crDefn]).

  public normalize:(pkgSpec,canonDef,reports)=>either[reports,cons[crDefn]].
  normalize(PkgSpec,varDef(Lc,Nm,_,PkgVal,_,Tp),Rp) => do{
    Map .= pkgMap(PkgSpec);
--    logMsg("package map $(Map)");
    (Vl,Defs) <- liftExp(PkgVal,Map,[],[],Rp);
--    logMsg("transformed package: $(Vl)");
    valis [glbDef(Lc,crId(Nm,Tp),Vl),..Defs]
  }

  pkgMap:(pkgSpec) => nameMap.
  pkgMap(pkgSpec(Pkg,Imports,Tp,Cons,Impls,PkgVrs)) =>
    [lyr(foldRight(((Nm,ITp),D)=>D[Nm->globalVar(Nm,ITp)],[],PkgVrs))].

  cellDefs:(cons[canonDef]) => cons[(crVar,canon)].
  cellDefs(Defs) =>
    foldLeft((D,FF) => (V^=isCellDef(D) ? FF\+V || FF),
      [],Defs).

  isCellDef(varDef(_,Nm,_,Vl,_,Tp)) where isRefType(Tp) =>
    some((crId(Nm,Tp),Vl)).
  isCellDef(_) default => .none.
  
  liftLetExp:(locn,cons[canonDef],canon,nameMap,set[crVar],cons[crDefn],reports) =>
    either[reports,crFlow].
  liftLetExp(Lc,Grp,Bnd,Outer,Q,Ex,Rp) => do{
    logMsg("lift let group $(Grp) @ $(Lc)");
    logMsg("Q=$(Q)");

    (lVars,fDefs) .= unzip(varDefs(Grp));

    GrpFns .= (Grp^/(D)=>~_^=isVarDef(D));
    rawGrpFree .= freeLabelVars(freeVarsInTerm(letExp(Lc,Grp,Bnd),[],Q,[]),Outer)::cons[crVar];

    logMsg("cell vars $(lVars)");
    logMsg("raw free vars $(rawGrpFree)");
--    logMsg("outer map $(Outer)");
    
    -- ffreeVars .=
    --   foldLeft((crId(Nm,Tp),So) =>
    -- 	  (_ ^= lookup(Outer,Nm,isModule) ? So || So\+crId(Nm,Tp)),
    -- 	[],
    -- 	rawGrpFree \ lVars);

    ffreeVars .= rawGrpFree \ lVars;

    logMsg("simplifying $(ffreeVars)");
    varParents .= freeParents(ffreeVars,Outer);
    logMsg("parents $(varParents)");
    freeVars <- reduceFreeArgs(varParents,Outer,Rp);
    logMsg("simplified free $(freeVars)\:$(typeOf(freeVars))");

    if isEmpty(lVars) && isEmpty(freeVars) then{
      M .= [lyr(foldRight((D,LL)=>collectMtd(D,.none,LL),[],Grp)),..Outer];
--      logMsg("let map is $(head(M))");

      Ex1 <- transformGroup(Grp,M,Q,.none,Ex,Rp);
      liftExp(Bnd,M,Q,Ex1,Rp)
--     } else if isEmpty(lVars) && size(freeVars)==1 then{
--       ThV ^= head(freeVars);
--       ThVr .= some(crVar(Lc,ThV));
--       L .= [crName(ThV)->localVar(crVar(Lc,ThV))];

--       M .= [lyr(L),..Outer];
--       MM .= [lyr(foldRight((D,LL)=>collectMtd(D,some(ThV),LL),L,Grp)),..Outer];
-- --      logMsg("let map is $(head(M))");

--       GrpQ .= foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q,lVars),Grp);
--       Ex1 <- transformGroup(Grp,M,GrpQ,ThVr,Ex,Rp);
--       liftExp(Bnd,MM,GrpQ,Ex1,Rp)
    }
    else {
      allFree .= freeVars++lVars;
      logMsg("free vars in let $(allFree)");
      ThV .= genVar("_ThVr",typeOf(allFree));
      ThVr .= crVar(Lc,ThV);

      L .= collectLabelVars(allFree,ThV,0,[]);
      MM .= [lyr(foldRight((D,LL)=>collectMtd(D,some(ThV),LL),L,GrpFns)),..Outer];

      logMsg("theta var $(ThV) ~ $(L)");
    
      M .= [lyr(L),..Outer];

      logMsg("let map is $(head(MM))");

      GrpQ .= foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q,lVars),Grp);
      
      Ex1 <- transformGroup(GrpFns,M,GrpQ,some(ThVr),Ex,Rp);
      freeArgs <- seqmap((crId(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,M,Rp),freeVars);
      (cellArgs,Ex2) <- liftExps(fDefs,GrpQ,M,Ex1,Rp);

      GrpFree .= crTpl(Lc,freeArgs++cellArgs);

      logMsg("free term $(ThV) = $(GrpFree)\:$(typeOf(GrpFree))");
      (BndTrm,Exx) <- liftExp(Bnd,MM,GrpQ,Ex2,Rp);
--      logMsg("Bound exp $(BndTrm)");
    
      valis (crLtt(Lc,ThV,GrpFree,BndTrm),Exx)
    }
  }

  -- In a let rec, all the non functions must end up in the free term
  varDefs:(cons[canonDef]) => cons[(crVar,canon)].
  varDefs(Defs) =>
    foldLeft((D,FF) => (V^=isVarDef(D) ? FF\+V || FF),
      [],Defs).

--  isVarDef(varDef(_,Nm,_,Vl,_,Tp)) where isRefType(Tp) =>
--    some((crId(Nm,Tp),Vl)).
  isVarDef(varDef(_,Nm,FullNm,Vl,_,Tp)) where ~isFunDef(Vl) =>
    some((crId(Nm,Tp),Vl)).
  isVarDef(implDef(_,_,Nm,Vl,_,Tp)) where ~isFunDef(Vl) =>
    some((crId(Nm,Tp),Vl)).
  isVarDef(_) default => .none.

  liftLetRec:(locn,cons[canonDef],canon,nameMap,set[crVar],
    cons[crDefn],reports) => either[reports,crFlow].
  liftLetRec(Lc,Grp,Bnd,Outer,Q,Ex,Rp) => do{
--    logMsg("lift let rec group $(Grp) @ $(Lc)");
--    logMsg("Q=$(Q)");
    GrpFns .= (Grp^/(D)=>~_^=isVarDef(D));
    (lVars,vrDefs) .= unzip(varDefs(Grp));
--    logMsg("lVars = $(lVars)");
--    logMsg("vrDefs = $(vrDefs)");
    
    rawGrpFree .= freeLabelVars(freeVarsInTerm(letExp(Lc,Grp,Bnd),[],Q,[]),Outer)::cons[crVar];
--    logMsg("raw free vars $(rawGrpFree)");
    varParents .= freeParents(rawGrpFree \ lVars,Outer);
    freeVars <- reduceFreeArgs(varParents,Outer,Rp);
--    logMsg("free variables $(freeVars)\:$(typeOf(freeVars))");

    if isEmpty(lVars) && isEmpty(freeVars) then {
      M .= [lyr(foldRight((D,LL)=>collectMtd(D,.none,LL),[],GrpFns)),..Outer];

      Ex1 <- transformGroup(Grp,M,Q,.none,Ex,Rp);
      liftExp(Bnd,M,Q,Ex1,Rp)
/*    } else if isEmpty(lVars) && size(freeVars)==1 then{
      ThV ^= head(freeVars);
      logMsg("singleton free $(ThV)");
      OThV <- liftVarExp(Lc,crName(ThV),typeOf(ThV),Outer,Rp);
      ThVr .= some(OThV);
      L .= [crName(ThV)->localVar(OThV)];

      MM .= [lyr(foldRight((D,LL)=>collectMtd(D,some(ThV),LL),L,Grp)),..Outer];
      logMsg("let map is $(head(MM))");

      GrpQ .= foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q,lVars),Grp);
      Ex1 <- transformGroup(Grp,MM,GrpQ,ThVr,Ex,Rp);
      liftExp(Bnd,MM,GrpQ,Ex1,Rp)
*/
    } else{
      allFree .= freeVars++lVars;
      ThV .= genVar("_ThVr",typeOf(allFree));
      ThVr .= crVar(Lc,ThV);

      L .= collectLabelVars(allFree,ThV,0,[]);
      M .= [lyr(foldRight((D,LL)=>collectMtd(D,some(ThV),LL),L,Grp)),..Outer];

--      logMsg("theta var $(ThV)\:$(typeOf(ThV)) ~ $(L)");
--      logMsg("letrec map is $(head(M))");

      freeArgs <- seqmap((crId(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer,Rp),freeVars);
--      logMsg("free vars lift to $(freeArgs)");
      cellVoids .= (vrDefs//(E)=>crVoid(Lc,typeOf(E)));
      (freeArgs1,Ex1) <- liftRecArgs(vrDefs,size(freeVars),freeArgs++cellVoids,Q,ThV,M,Ex,Rp);
      GrpFree .= crTpl(Lc,freeArgs1);
      
--      logMsg("free term $(ThV) = $(GrpFree)\:$(typeOf(GrpFree))");

      GrpQ .= foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q,lVars),Grp);
      Ex2 <- transformGroup(GrpFns,M,GrpQ,some(ThVr),Ex1,Rp);
      
--      logMsg("transform bound exp $(Bnd)");
      (BndTrm,Exx) <- liftExp(Bnd,M,GrpQ,Ex2,Rp);
      logMsg("extra defs $(Exx)");

      valis (crLtRec(Lc,ThV,GrpFree,BndTrm),Exx)
    }
  }

  liftRecArgs:(cons[canon],integer,cons[crExp],set[crVar],crVar,nameMap,cons[crDefn],reports) =>
    either[reports,(cons[crExp],cons[crDefn])].
  liftRecArgs([],_,LetArgs,_,_,_,Ex,_) => either((LetArgs,Ex)).
  liftRecArgs([E,..Es],Ix,LetArgs,Q,ThV,Map,Ex,Rp) => do{
    (LT,Ex1) <- liftRecTerm(E,Ix,LetArgs,Q,ThV,Map,Ex,Rp);
    liftRecArgs(Es,Ix+1,LT,Q,ThV,Map,Ex1,Rp)
  }

  liftRecTerm:(canon,integer,cons[crExp],set[crVar],crVar,nameMap,cons[crDefn],reports) =>
    either[reports,(cons[crExp],cons[crDefn])].
  liftRecTerm(Term,Ix,LetArgs,Q,ThV,Map,Ex,Rp) => do{
    (Arg,Ex1) <- liftExp(Term,Map,Q,Ex,Rp);
    valis (LetArgs[Ix->refactor(Arg,ThV,LetArgs)],Ex1)
  }

  refactor:(crExp,crVar,cons[crExp])=>crExp.
  refactor(T,ThV,LetArgs) => let{.
    test(crTplOff(_,crVar(_,ThV),Ix,_)) where R^=LetArgs[Ix] && ~crVoid(_,_).=R => some(R).
    test(_) default => .none
  .} in rwTerm(T,test).

  isFunctions:(cons[canonDef])=>boolean.
  isFunctions(Defs) => varDef(_,_,_,Vl,_,_) in Defs *> isFunDef(Vl).

  transformGroup:(cons[canonDef],nameMap,set[crVar],option[crExp],cons[crDefn],reports) => either[reports,cons[crDefn]].
  transformGroup([],Map,_,_,D,_) => either(D).
  transformGroup([D,..Ds],Map,Q,Extra,Ex,Rp) => do {
    Ex1 <- transformDef(D,Map,Q,Extra,Ex,Rp);
    transformGroup(Ds,Map,Q,Extra,Ex1,Rp)
  }

  transformDef:(canonDef,nameMap,set[crVar],option[crExp],cons[crDefn],reports) =>
    either[reports,cons[crDefn]].
  transformDef(varDef(Lc,Nm,FullNm,lambda(_,Eqns,Tp),_,_),Map,Q,Extra,Ex,Rp) => do{
    logMsg("transform function $(Nm) - $(Eqns)");
    logMsg("extra = $(Extra)");
    ATp .= extendFunTp(deRef(Tp),Extra);
    (Eqs,Ex1) <- transformEquations(Eqns,Map,Q,Extra,Ex,Rp);
    logMsg("transformed eqns $(Nm) - $(Eqs)");
    Func .= functionMatcher(Lc,FullNm,ATp,Eqs);
    logMsg("transformed function $(Nm) = $(Func)");
    logMsg("extra defs for $(FullNm)\: $(front(Ex1,size(Ex1)-size(Ex)))");

    ClosureNm .= closureNm(FullNm);
    ClVar .= (crVar(_,Exv)^=Extra ? Exv || crId("_",unitTp));
    ClVars .= makeFunVars(Tp);
    ClArgs .= [ClVar,..ClVars];

--    logMsg("ClArgs = $(ClArgs)");

    ClosTp .= extendFunTp(deRef(Tp),some(ClVar));

    if Exv^=Extra then {
--    logMsg("extra $(Exv)");
      ClosEntry .=
	fnDef(Lc,ClosureNm,ClosTp,ClArgs,
	  crCall(Lc,FullNm,ClArgs//(V)=>crVar(Lc,V),funTypeRes(Tp)));
      logMsg("closure entry for $(FullNm) : $(ClosEntry)");
      valis [Func,ClosEntry,..Ex1]
    } else {
      ClosEntry .=
	fnDef(Lc,ClosureNm,ClosTp,
	  ClArgs,crCall(Lc,FullNm,ClVars//(V)=>crVar(Lc,V),funTypeRes(Tp)));

      logMsg("value expression of $(Nm) is crTerm(Lc,ClosureNm,[],Tp)");
      logMsg("closure entry for $(FullNm) : $(ClosEntry)");
      
      unit .= crTpl(Lc,[]);

      valis [Func,ClosEntry,..Ex1]
    }
  }
  transformDef(varDef(Lc,_,FullNm,Val,Cx,Tp),Map,Q,.none,Ex,Rp) => do{
--    logMsg("transform var $(FullNm) = $(Val)");
    (Vl,Defs) <- liftExp(Val,Map,Q,Ex,Rp);
    valis [glbDef(Lc,crId(FullNm,Tp),Vl),..Defs]
  }
  transformDef(varDef(Lc,_,FullNm,Val,Cx,Tp),Map,Q,some(crVar(_,ThVr)),Ex,Rp) => do{
    logMsg("transform var $(FullNm) = $(Val) with $(ThVr)");
    (Vl,Defs) <- liftExp(Val,Map,Q,Ex,Rp);
    logMsg("transformed var $(Val) = $(Vl)");
    ClosureNm .= varClosureNm(FullNm);
    logMsg("closure name for $(FullNm) is $(ClosureNm)");
    ClosEntry .= fnDef(Lc,ClosureNm,funType([typeOf(ThVr)],Tp),[ThVr],Vl);
    valis [ClosEntry,..Defs]
  }
  transformDef(implDef(Lc,_,FullNm,Val,Cx,Tp),Map,Q,Extra,Ex,Rp) => 
    transformDef(varDef(Lc,FullNm,FullNm,Val,Cx,Tp),Map,Q,Extra,Ex,Rp).
  transformDef(_,_,_,_,Ex,_) => either(Ex).

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
  implementPtnCall(Lc,localCons(Nm,CTp,Vr),Args,Tp,_,_,Ex,_) => either((crTerm(Lc,Nm,[crVar(Lc,Vr),..Args],Tp),Ex)).
  
  trVarPtn(Lc,Nm,Tp,Map,Ex,Rp) => implementVarPtn(Lc,Nm,lookupVarName(Map,Nm),Tp,Map,Ex,Rp).

  implementVarPtn(Lc,Nm,.none,Tp,_,Ex,_) => either((crVar(Lc,crId(Nm,Tp)),Ex)).
  implementVarPtn(Lc,Nm,some(moduleCons(Enum,CTp)),Tp,_,Ex,_) where ETp^=isEnumType(CTp) =>
    either((crLbl(Lc,Enum,ETp),Ex)).
  implementVarPtn(Lc,Nm,some(localCons(Enum,CTp,Vr)),Tp,_,Ex,_) =>
    either((crTerm(Lc,Enum,[crVar(Lc,Vr)],Tp),Ex)).
  implementVarPtn(Lc,Nm,some(labelArg(Base,Ix)),Tp,Map,Ex,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),typeOf(Base),Map,Rp);
    NN .= crVar(Lc,crId(Nm,Tp));
    valis (crWhere(Lc,NN,crMatch(Lc,NN,crMemoGet(Lc,crTplOff(Lc,V,Ix,Tp),Tp))),Ex)
  }
  implementVarPtn(Lc,Nm,some(memoArg(Base,Ix)),Tp,Map,Ex,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),typeOf(Base),Map,Rp);
    NN .= crVar(Lc,crId(Nm,Tp));
    valis (crWhere(Lc,NN,crMatch(Lc,NN,crTplOff(Lc,V,Ix,Tp))),Ex)
  }
  implementVarPtn(Lc,Nm,some(V),Tp,Map,_,Rp) => other(reportError(Rp,"not permitted to match against $(Nm)\:$(V)",Lc)).

  liftExp:(canon,nameMap,set[crVar],cons[crDefn],reports) => either[reports,crFlow].
  liftExp(vr(Lc,Nm,Tp),Map,Q,Ex,Rp) => do{
    VV <- liftVarExp(Lc,Nm,Tp,Map,Rp);
    valis (VV,Ex)
  }
  liftExp(intr(Lc,Ix),_,Map,Ex,Rp) => either((crInt(Lc,Ix),Ex)).
  liftExp(flt(Lc,Dx),_,Map,Ex,Rp) => either((crFlot(Lc,Dx),Ex)).
  liftExp(strng(Lc,Sx),_,Map,Ex,Rp) => either((crStrg(Lc,Sx),Ex)).
  liftExp(enm(Lc,FullNm,Tp),_,Map,Ex,Rp) => either((crLbl(Lc,FullNm,Tp),Ex)).
  liftExp(tple(Lc,Els),Q,Map,Ex,Rp) => do{
    (LEls,Exx) <- liftExps(Els,Map,Q,Ex,Rp);
    valis (mkCrTpl(Lc,LEls),Exx)
  }
  liftExp(apply(Lc,Op,tple(_,Els),Tp),Q,Map,Ex,Rp) => do{
    (LEls,Ex1) <- liftExps(Els,Map,Q,Ex,Rp);
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
--    logMsg("lift record $(record(Lc,some(Nm),Fields,Tp)) using map $(Map)");
    (LFields,Exx) <- liftFields(Fields,Map,Q,Ex,Rp);
    valis (crRecord(Lc,Nm,LFields,Tp),Exx)
  }
  liftExp(record(Lc,.none,Fields,Tp),Map,Q,Ex,Rp) => do{
--    logMsg("lift record $(record(Lc,.none,Fields,Tp)), using map $(Map)");
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
--    logMsg("transform lambda $(Lam)");

    ffreeVars .=
      foldLeft((crId(Nm,VTp),So) =>
	  (.true ^= lookup(Map,Nm,isModule) ? So || So\+crId(Nm,VTp)),
	[],
	freeLabelVars(freeVarsInTerm(Lam,[],Q,[]),Map));


    varParents .= freeParents(ffreeVars,Map);
    freeVars <- reduceFreeArgs(varParents,Map,Rp);

--    logMsg("lambda free vars $(freeVars), Q=$(Q)");

    ThV .= genVar("_ThVr",typeOf(freeVars));
    freeArgs <- seqmap((crId(Nm,VTp))=>liftVarExp(Lc,Nm,VTp,Map,Rp),freeVars);
    LMap .= [lyr(collectLabelVars(freeVars,ThV,0,[])),..Map];

    FreeTerm .= crTpl(Lc,freeArgs);

--    logMsg("lambda map $(LMap)");
--    logMsg("free lambda term $(ThV) = $(FreeTerm)");
    Extra .= some(crVar(Lc,ThV));
    ATp .= extendFunTp(deRef(Tp),Extra);
    (Eqs,Ex1) <- transformEquations(Eqns,LMap,Q,Extra,Ex,Rp);
--    logMsg("lambda equations $(Eqs)");
    LamFun .= functionMatcher(Lc,FullNm,ATp,Eqs);

--    logMsg("lambda closure $(crTerm(Lc,FullNm,[FreeTerm],ATp))");
--    logMsg("lambda function $(LamFun)");
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

  liftExps:(cons[canon],set[crVar],nameMap,cons[crDefn],reports) => either[reports,(cons[crExp],cons[crDefn])].
  liftExps([],_,_,Ex,_) => either(([],Ex)).
  liftExps([P,..Ps],Q,Map,Ex,Rp) => do{
    (A,Ex1) <- liftExp(P,Map,Q,Ex,Rp);
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
  implementFunCall(Lc,moduleFun(_,Fn),_,Args,Tp,Map,Ex,Rp) =>
    either((crCall(Lc,Fn,Args,Tp),Ex)).
  implementFunCall(Lc,moduleCons(Fn,FTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crTerm(Lc,Fn,Args,Tp),Ex)).
  implementFunCall(Lc,localCons(Fn,FTp,Vr),_,Args,Tp,Map,Ex,Rp) => do{
    VV<-liftVarExp(Lc,crName(Vr),typeOf(Vr),Map,Rp);
    valis (crTerm(Lc,Fn,[VV,..Args],Tp),Ex)
  }
  implementFunCall(Lc,localFun(Nm,ClNm,Th),_,Args,Tp,Map,Ex,Rp) => do{
    V <- liftVarExp(Lc,crName(Th),Tp,Map,Rp);
    valis (crCall(Lc,Nm,[V,..Args],Tp),Ex)
  }
  implementFunCall(Lc,labelArg(Base,Ix),_,Args,Tp,Map,Ex,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),typeOf(Base),Map,Rp);
    valis (crOCall(Lc,crTplOff(Lc,V,Ix,Tp),Args,Tp),Ex)
  }
  implementFunCall(Lc,memoArg(Base,Ix),_,Args,Tp,Map,Ex,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),typeOf(Base),Map,Rp);
    valis (crOCall(Lc,crMemoGet(Lc,crTplOff(Lc,V,Ix,Tp),Tp),Args,Tp),Ex)
  }
  implementFunCall(Lc,localVar(Vr),_,Args,Tp,Map,Ex,Rp) =>
    either((crOCall(Lc,Vr,Args,Tp),Ex)).
  implementFunCall(Lc,globalVar(Nm,GTp),_,Args,Tp,Map,Ex,Rp) =>
    either((crOCall(Lc,crVar(Lc,crId(Nm,GTp)),Args,Tp),Ex)).
  implementFunCall(Lc,V,Vr,Args,Tp,Map,Ex,Rp) =>
    other(reportError(Rp,"illegal variable $(Vr) - $(V)",Lc)).

  liftVarExp:(locn,string,tipe,nameMap,reports) => either[reports,crExp].
  liftVarExp(Lc,Nm,Tp,Map,Rp) where Entry ^= lookupVarName(Map,Nm) => do{
    logMsg("var $(Nm) entry $(Entry)");
    implementVarExp(Lc,Entry,Map,Tp,Rp)
  }.
  liftVarExp(Lc,Nm,Tp,Map,Rp) => do{
    valis crVar(Lc,crId(Nm,Tp))
  }

  implementVarExp:(locn,nameMapEntry,nameMap,tipe,reports) => either[reports,crExp].
  implementVarExp(Lc,localFun(_,ClNm,ThVr),Map,Tp,Rp) => do{
    V <- liftVarExp(Lc,crName(ThVr),Tp,Map,Rp);
    valis crTerm(Lc,ClNm,[V],Tp)
  }
  implementVarExp(Lc,labelArg(Base,Ix),Map,Tp,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),typeOf(Base),Map,Rp);
    valis crTplOff(Lc,V,Ix,Tp)
  }.
  implementVarExp(Lc,memoArg(Base,Ix),Map,Tp,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),typeOf(Base),Map,Rp);
    valis crMemoGet(Lc,crTplOff(Lc,V,Ix,Tp),Tp)
  }.
  implementVarExp(Lc,localVar(Vr),_,Tp,Rp) => either(Vr).
  implementVarExp(Lc,moduleCons(Enum,CTp),_,Tp,Rp) => either(crLbl(Lc,Enum,Tp)).
  implementVarExp(Lc,localCons(Enum,CTp,Vr),Map,Tp,Rp) => do{
    V <- liftVarExp(Lc,crName(Vr),typeOf(Vr),Map,Rp);
    valis crTerm(Lc,Enum,[V],Tp)
  }
  implementVarExp(Lc,moduleFun(V,_),_,Tp,Rp) => either(V).
  implementVarExp(Lc,globalVar(Nm,GTp),_,Tp,Rp) => either(crVar(Lc,crId(Nm,GTp))).
  implementVarExp(Lc,E,_,_,Rp) => other(reportError(Rp,"cannot transform variable $(E)",Lc)).

  pathExp:(locn,crExp,cons[integer]) => crExp.
  pathExp(_,Th,[]) => Th.
  pathExp(Lc,Exp,[Off,..Pth]) where EE.=pathExp(Lc,Exp,Pth) && Tp^=nthTp(typeOf(Exp),Off) =>
    crTplOff(Lc,EE,Off,Tp).

  nthTp(tupleType(Els),Ix) => Els[Ix].


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
    (LT,Ex1) <- liftGoal(T,Map,Q,Ex,Rp);
    Q1 .= glVars(T,Q);
    (LL,Ex2) <- liftGoal(L,Map,Q1,Ex1,Rp);
    (LR,Ex3) <- liftGoal(R,Map,Q,Ex2,Rp);
    valis (crCnd(Lc,LT,LL,LR),Ex3)
  }
  liftGoal(match(Lc,P,E),Map,Q,Ex,Rp) => do{
    (LP,Ex1) <- liftPtn(P,Map,Q,Ex,Rp);
    (LE,Ex2) <- liftExp(E,Map,Q,Ex1,Rp);
    valis (crMatch(Lc,LP,LE),Ex2)
  }
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

  collectMtd:(canonDef,option[crVar],map[string,nameMapEntry])=>map[string,nameMapEntry].

  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),some(ThVr),LL) where isFunDef(Val) =>
    LL[Nm->localFun(FullNm,closureNm(FullNm),ThVr)].
  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),.none,LL) where isFunDef(Val) =>
    LL[Nm->moduleFun(crTerm(Lc,closureNm(FullNm),[crTpl(Lc,[])],Tp),FullNm)].
  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),.none,LL) =>
    LL[Nm->globalVar(FullNm,Tp)].
  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),some(ThVr),LL) => LL.
--  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),some(ThVr),LL) =>
--    LL[Nm->localVar(crCall(Lc,varClosureNm(FullNm),[crVar(Lc,ThVr)],Tp))].
  collectMtd(implDef(Lc,_,FullNm,Val,Cx,Tp),ThVr,LL) =>
    collectMtd(varDef(Lc,FullNm,FullNm,Val,Cx,Tp),ThVr,LL).
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),.none,LL) => LL[Nm->moduleCons(FullNm,Tp)].
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),some(ThVr),LL) => LL[Nm->localCons(FullNm,Tp,ThVr)].
  collectMtd(typeDef(_,_,_,_),_,LL) => LL.
  collectMtd(conDef(_,_,_,_),_,LL) => LL.

  collectQ:(canonDef,set[crVar]) => set[crVar].
  collectQ(varDef(Lc,Nm,FullNm,Val,_,Tp),Q) => Q\+crId(Nm,Tp).
  collectQ(implDef(Lc,_,FullNm,Val,_,Tp),Q) => Q\+crId(FullNm,Tp).
  collectQ(cnsDef(_,Nm,FullNm,Tp),Q) => Q.
  collectQ(typeDef(_,_,_,_),Q) => Q.
  collectQ(conDef(_,_,_,_),Q) => Q.

  collectLabelVars:(cons[crVar],crVar,integer,map[string,nameMapEntry]) =>
    map[string,nameMapEntry].
  collectLabelVars([],_,_,LV) => LV.
  collectLabelVars([V,..Vrs],ThV,Ix,Entries) where crId(Nm,Tp) .= V =>
    collectLabelVars(Vrs,ThV,Ix+1,Entries[Nm->labelArg(ThV,Ix)]).

  freeLabelVars:(set[crVar],nameMap)=>set[crVar].
  freeLabelVars(Fr,Map) => foldLeft((V,So)=>labelVar(V,Map,So),Fr,Fr).

  labelVar(crId(Nm,_),Map,So) where Entry^=lookupVarName(Map,Nm) =>
    case Entry in {
      labelArg(ThVr,_) => So\+ThVr.
      localFun(_,_,ThVr) => So\+ThVr.
      _ => So
    }.
  labelVar(_,_,So) default => So.

  collectMemoVars:(cons[crVar],crVar,integer,map[string,nameMapEntry]) =>
    map[string,nameMapEntry].
  collectMemoVars([],_,_,LV) => LV.
  collectMemoVars([V,..Vrs],ThV,Ix,Entries) where crId(Nm,Tp) .= V =>
    collectMemoVars(Vrs,ThV,Ix+1,Entries[Nm->memoArg(ThV,Ix)]).

  isModule(moduleFun(_,_))=>some(.true).
--  isModule(localFun(_,_,_))=>some(.true).
  isModule(globalVar(_,_))=>some(.true).
  isModule(_) default => .none.

  -- eliminate free variables that can be computed from other free vars
  reduceFreeArgs:(cons[crVar],nameMap,reports) => either[reports,cons[crVar]].
  reduceFreeArgs(FrVrs,Map,Rp) => let{
    reduceArgs:(cons[crVar],cons[crVar]) => either[reports,cons[crVar]].
    reduceArgs([],Frs) => either(Frs).
    reduceArgs([FrV,..FrArgs],Frs) where
	FrNm .= crName(FrV) &&
	OTh ^= lookupThetaVar(Map,FrNm) &&
	OTh .<. Frs =>
      reduceArgs(FrArgs,drop(FrV,Frs)).
    reduceArgs([_,..FrArgs],Frs) => reduceArgs(FrArgs,Frs).
  } in reduceArgs(FrVrs,FrVrs).

  freeParents:(cons[crVar],nameMap) => cons[crVar].
  freeParents(Frs,Map) => foldLeft((F,Fs)=>Fs\+freeParent(F,Map),[],Frs).

  freeParent(V,Map) where ThV ^= lookupThetaVar(Map,crName(V)) =>
    freeParent(ThV,Map).
  freeParent(V,_) default => V.

  lookup:all e ~~ (nameMap,string,(nameMapEntry)=>option[e])=>option[e].
  lookup([],_,_) => .none.
  lookup([lyr(Entries),..Map],Nm,P) where E ^= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).

  lookupVarName:(nameMap,string)=>option[nameMapEntry].
  lookupVarName(Map,Nm) => lookup(Map,Nm,anyDef).

  anyDef(D) => some(D).

  lookupThetaVar:(nameMap,string)=>option[crVar].
  lookupThetaVar(Map,Nm) where E^=lookupVarName(Map,Nm) =>
    case E in {
      labelArg(ThV,_) => some(ThV).
      memoArg(ThV,_) => some(ThV).
      localFun(_,_,ThV) => some(ThV).
      _ default => .none
    }.
  lookupThetaVar(_,_) default => .none.

  genVar:(string,tipe) => crVar.
  genVar(Pr,Tp) => crId(genSym(Pr),Tp).

  makeFunVars:(tipe)=>cons[crVar].
  makeFunVars(Tp) where tupleType(Es).=funTypeArg(deRef(Tp)) => (Es//(E)=>genVar("_",E)).

  crTpl:(locn,cons[crExp]) => crExp.
  crTpl(Lc,Args) => let{
    Tp = typeOf(Args).
    Ar = size(Args).
  } in crTerm(Lc,tplLbl(Ar),Args,Tp).

  closureNm:(string)=>string.
  closureNm(Nm)=>Nm++"^".

  varClosureNm:(string)=>string.
  varClosureNm(Nm) => Nm++"$".
}
