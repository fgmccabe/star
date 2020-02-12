star.compiler.action{
  import star.

  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.

  genAction:(canonAction,tipe,option[canon],string,reports) => either[reports,option[canon]].
  genAction(seqnDo(Lc,A,B),Contract,Cont,Path,Rp) => do{
    RR <- genAction(B,Contract,Cont,Path,Rp);
    genAction(A,Contract,RR,Path,Rp)
  }
  genAction(returnDo(Lc,A,ExTp,VlTp,ErTp),Contract,none,Path,Rp) =>
    genReturn(Lc,A,ExTp,VlTp,ErTp,Contract,Rp).
  genAction(returnDo(Lc,A,ExTp,VlTp,ErTp),Contract,some(_),Path,Rp) =>
    other(reportError(Rp,"return $(A) must be last action",Lc)).
  genAction(bindDo(Lc,Ptn,Gen,ExTp,VlTp,ErTp),Contract,none,_,Rp) =>
    other(reportError(Rp,"$(Ptn) <- $(Gen) may not be last action",Lc)).
  genAction(bindDo(Lc,Ptn,Gen,ExTp,VlTp,ErTp),Contract,some(Cont),_,Rp) => do{
    ConTp = typeOf(Cont);
    PtnTp = typeOf(Ptn);
    GenTp = typeOf(Gen);
    LamTp = funType([PtnTp],ConTp);
    Lam = lambda(Lc,[eqn(Lc,tple(Lc,[Ptn]),none,Cont)],LamTp);
    SeqTp = funType([GenTp,LamTp],ConTp);
    Seqn = over(Lc,mtd(Lc,"_sequence",SeqTp),SeqTp,[typeConstraint(mkTypeExp(Contract,[ExTp]))]);
    valis some(apply(Lc,Seqn,tple(Lc,[Gen,Lam]),ConTp))
  }
  genAction(varDo(Lc,Ptn,Gen),Contract,none,_,Rp) =>
    other(reportError(Rp,"$(Ptn) = $(Gen) may not be last action",Lc)).
  genAction(varDo(Lc,Ptn,Gen),Contract,some(Cont),_,Rp) => do{
    ConTp = typeOf(Cont);
    PtnTp = typeOf(Ptn);
    GenTp = typeOf(Gen);
    LamTp = funType([PtnTp],ConTp);
    Lam = lambda(Lc,[eqn(Lc,tple(Lc,[Ptn]),none,Cont)],LamTp);
    valis some(apply(Lc,Lam,tple(Lc,[Gen]),ConTp))
  }
  genAction(delayDo(Lc,Actn,ExTp,ValTp,ErTp),Contract,Cont,Path,Rp) => do{
    some(RtnUnit) <- genReturn(Lc,tple(Lc,[]),ExTp,ValTp,ErTp,Contract,Rp);
    NAct <- genAction(Actn,Contract,Cont,Path,Rp);
    valis some(combineActs(Lc,RtnUnit,NAct,Contract,ExTp))
  }
  genAction(tryCatchDo(Lc,Body,Hndlr,ExTp,ValTp,ErTp),Contract,Cont,Path,Rp) => do{
    HndlrTp = typeOf(Hndlr);
    ConTp = mkTypeExp(ExTp,[ErTp,ValTp]);
    some(BdyExp) <- genAction(Body,Contract,none,Path,Rp);
    BType = typeOf(BdyExp);
    LamType = funType([BType,HndlrTp],ConTp);
    H = over(Lc,mtd(Lc,"_handle",LamType),LamType,[typeConstraint(mkTypeExp(Contract,[ExTp]))]);
    HB = apply(Lc,H,tple(Lc,[BdyExp,Hndlr]),ConTp);
    valis some(combineActs(Lc,HB,Cont,Contract,ExTp))
  }
  genAction(throwDo(Lc,Exc,ExTp,VlTp,ErTp),Contract,Cont,Path,Rp) => do{
    MdlTp = mkTypeExp(ExTp,[ErTp,VlTp]);
    ThrTp = funType([ErTp],MdlTp);
    Gen = over(Lc,mtd(Lc,"_raise",ThrTp),ThrTp,[typeConstraint(mkTypeExp(Contract,[ExTp]))]);
    valis some(combineActs(Lc,apply(Lc,Gen,tple(Lc,[Exc]),MdlTp),Cont,Contract,ExTp))
  }
  genAction(simpleDo(Lc,Exp,ExTp),Contract,Cont,Path,Rp) => do{
    valis some(combineActs(Lc,Exp,Cont,Contract,ExTp))
  }
  genAction(ifThenElseDo(Lc,Tst,Th,El,ExTp,ValTp,ErTp),Contract,Cont,Path,Rp) => do{
    MdlTp = mkTypeExp(ExTp,[ErTp,ValTp]);
    some(Then) <- genAction(Th,Contract,none,Path,Rp);
    some(Else) <- genAction(El,Contract,none,Path,Rp);
    if isIterableGoal(Tst) then {
      valis some(combineActs(Lc,cond(Lc,genIterableGoal(Tst,Contract,Path),Then,Else),Cont,Contract,ExTp))
    } else{
      valis some(combineActs(Lc,cond(Lc,Tst,Then,Else),Cont,Contract,ExTp))
    }
  }
  /* Construct a local iterator function:
   let{
     loop() => do{ if C then { B; loop() } else  <Cont> 
   } in loop()
  */
  genAction(whileDo(Lc,Tst,Bdy,StTp,ErTp),Contract,some(Cont),Path,Rp) => do{
    LclName = genNewName(Path,"loop");
    ThPath = genNewName(Path,"lp");
    Fn = genSym("loop");
    UnitTp = tupleType([]);
    LpTp = mkTypeExp(StTp,[ErTp,UnitTp]);
    FnTp = funType([],LpTp);
    FnCall = apply(Lc,vr(Lc,Fn,FnTp),tple(Lc,[]),LpTp);
    some(Then) <- genAction(seqnDo(Lc,Bdy,simpleDo(Lc,FnCall,StTp)),
      Contract,none,ThPath,Rp);
    FF = varDef(Lc,Fn,LclName,lambda(Lc,[eqn(Lc,tple(Lc,[]),none,cond(Lc,Tst,Then,Cont))],FnTp),[],FnTp);
    valis some(letExp(Lc,[FF],FnCall))
  }
    /*
   for C do {A}
  becomes:
  
  <iterator>( do{return ()}, (Lcls,St) => do {A; return St})
*/
  genAction(forDo(Lc,Tst,Body,StTp,ErTp),Contract,Cont,Path,Rp) => do{
    Unit = tple(Lc,[]);
    UnitTp = tupleType([]);
    Zed <- genReturn(Lc,Unit,StTp,UnitTp,ErTp,Contract,Rp);
    IterBody <- genAction(Body,Contract,Zed,Path,Rp);
    ForLoop <- genCondition(Tst,Path,
      genRtn(Lc,StTp,UnitTp,ErTp,Contract),
      genSeq(Lc,StTp,UnitTp,ErTp,Contract),
      genForBody(Lc,StTp,UnitTp,ErTp,Contract,IterBody),
      unlifted(Unit),Rp);
    valis some(combineActs(Lc,ForLoop,Cont,Contract,StTp))
  }

  genReturn:(locn,canon,tipe,tipe,tipe,tipe,reports) => either[reports,option[canon]].
  genReturn(Lc,A,ExTp,VlTp,ErTp,Contract,Rp) => do{
    ActionTp = mkTypeExp(ExTp,[ErTp,VlTp]);
    MtdTp = funType([VlTp],ActionTp);
    Gen = over(Lc,mtd(Lc,"_valis",MtdTp),MtdTp,
      [typeConstraint(mkTypeExp(Contract,[ExTp]))]);
    valis some(apply(Lc,Gen,tple(Lc,[A]),ActionTp))
  }
/*
  genSeq:(locn,tipe,tipe,tipe,canon,canon,canon,reports) => either[reports,canon].
  genSeq(Lc,ExStTp,ErTp,Contract,St,Init,Reslt,Rp) => do{
    ATp = typeOf(St);
    MdTp = mkTypeExp(Contract,[ErTp,ATp]);
    LamTp = funType([ATp],MdTp);
    Lam = lambda(Lc,[eqn(Lc,tple(Lc,[St]),none,Reslt)],LamTp);
    GenTp = funType([MdTp,LamTp],MdTp);
    Gen = over(Lc,mtd(Lc,"_sequence",GenTp),GenTp,[typeConstraint(mkTpExp(Contract,[ExStTp]))]);
    Initial <- genRtn(Lc,ExStTp,
  */
  combineActs:(locn,canon,option[canon],tipe,tipe)=>canon.
  combineActs(_,A1,none,_,_) => A1.
  combineActs(Lc,A1,some(A2),Contract,StTp) => let{
    A1Tp = typeOf(A1).
    Anon = anonVar(Lc,A1Tp).
    ConTp = typeOf(A2).
    LamTp = funType([A1Tp],ConTp).
    Lam = lambda(Lc,[eqn(Lc,tple(Lc,[Anon]),none,A2)],LamTp).
    SeqTp = funType([A1Tp,LamTp],ConTp).
    Gen = over(Lc,mtd(Lc,"_sequence",SeqTp),SeqTp,[typeConstraint(mkTypeExp(Contract,[StTp]))]).
  } in apply(Lc,Gen,tple(Lc,[A1,Lam]),ConTp).

  anonVar(Lc,Tp) => genVar(Lc,"_",Tp).
  genVar(Lc,Nm,Tp) => vr(Lc,genSym(Nm),Tp).

  -- Convert iterable conditions into iterations

  lifted ::= lifted(canon) | unlifted(canon).

  /*
  * Ptn in Src
  * becomes
  * let{
  *  sF(Ptn,St) => AddEl(X,St).
  *  sF(_,St) default => do { return St}.
  * } in _iter(Src,Initial,sF)
  *
  * where AddEl, InitState are parameters to the conversion
  */
  genCondition:(canon,string,lifted,lifted,lifted,reports) => either[reports,lifted].
  genCondition(serch(Lc,Ptn,Src,Iterator),Path,Lift,_Seq,Succ,Initial,Rp) => do{
    PtnTp .= typeOf(Ptn);
    logMsg("ptn in search has type $(PtnTp)");
    Anon .= anonVar(Lc,PtnTp);
    ItrTp .= typeOf(Iterator);
    logMsg("iterator has type $(ItrTp)");
    SrcTp .= typeOf(Src);
    ResltTp .= newTypeVar("_strm");
    St .= genVar(Lc,"_st",ResltTp);
    AddToFront .= Succ(unlifted(St));

  splitPtn(Ptn,Pttrn,PtnCond),
  call(Lift,unlifted(St),Dflt),
  typeOfCanon(AddToFront,MdlTp),
  FnTp = funType(tupleType([PtnTp,RsltTp]),MdlTp),
    reportMsg("entangle iterator type %s with monad %s",[ItrTp,funType(tupleType([SrcTp,MdlTp,FnTp]),MdlTp)]),
  sameType(funType(tupleType([SrcTp,MdlTp,FnTp]),MdlTp),ItrTp,[]),
  %  reportMsg("local fun type: %s",[FnTp]),
  genstr("f",Fn),
  genNewName(Path,"Γ",ThPath),
  packageVarName(ThPath,Fn,LclName),

  FF=funDef(Lc,Fn,LclName,FnTp,[],[
      equation(Lc,tple(Lc,[Pttrn,St]),PtnCond,AddToFront),
      equation(Lc,tple(Lc,[Anon,St]),enm(Lc,"true",type("star.core*boolean")),Dflt)
    ]),
  Let = letExp(Lc,theta(Lc,ThPath,true,[FF],[],[],faceType([],[])),v(Lc,Fn,FnTp)),
  call(Lift,Initial,Init),
  Exp = apply(Lc,Iterator,tple(Lc,[Src,Init,Let]),MdlTp).

    
}

  
