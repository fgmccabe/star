star.compiler.action{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freevars.
  import star.compiler.freshen.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.unify.

  public genAction:(canonAction,tipe,option[canon],string,reports) => either[reports,canon].
  genAction(seqnDo(Lc,A,B),Contract,Cont,Path,Rp) => do{
    RR <- genAction(B,Contract,Cont,Path,Rp);
    genAction(A,Contract,some(RR),Path,Rp)
  }
  genAction(returnDo(Lc,A,ExTp,VlTp,ErTp),Contract,.none,Path,Rp) =>
    genReturn(Lc,A,ExTp,VlTp,ErTp,Contract,Rp).
  genAction(returnDo(Lc,A,ExTp,VlTp,ErTp),Contract,some(_),Path,Rp) =>
    other(reportError(Rp,"return $(A) must be last action",Lc)).
  genAction(bindDo(Lc,Ptn,Gen,ExTp,VlTp,ErTp),Contract,.none,_,Rp) =>
    other(reportError(Rp,"$(Ptn) <- $(Gen) may not be last action",Lc)).
  genAction(bindDo(Lc,Ptn,Gen,VlTp,ExTp,ErTp),Contract,some(Cont),_,Rp) => do{
    ConTp .= typeOf(Cont);
    PtnTp .= typeOf(Ptn);
    GenTp .= typeOf(Gen);
    LamTp .= funType([PtnTp],ConTp);
    Lam .= lambda(Lc,[eqn(Lc,tple(Lc,[Ptn]),.none,Cont)],LamTp);
    SeqTp .= funType([GenTp,LamTp],ConTp);
    Seqn .= over(Lc,mtd(Lc,"_sequence",SeqTp),SeqTp,[typeConstraint(mkTypeExp(Contract,[ExTp]))]);
    valis apply(Lc,Seqn,tple(Lc,[Gen,Lam]),ConTp)
  }
  genAction(varDo(Lc,Ptn,Gen),Contract,.none,_,Rp) =>
    other(reportError(Rp,"$(Ptn) = $(Gen) may not be last action",Lc)).
  genAction(varDo(Lc,Ptn,Gen),Contract,some(Cont),_,Rp) => do{
    ConTp .= typeOf(Cont);
    PtnTp .= typeOf(Ptn);
    GenTp .= typeOf(Gen);
    LamTp .= funType([PtnTp],ConTp);
    Lam .= lambda(Lc,[eqn(Lc,tple(Lc,[Ptn]),.none,Cont)],LamTp);
    valis apply(Lc,Lam,tple(Lc,[Gen]),ConTp)
  }
  genAction(delayDo(Lc,Actn,ExTp,ValTp,ErTp),Contract,Cont,Path,Rp) => do{
    RtnUnit <- genReturn(Lc,tple(Lc,[]),ExTp,ValTp,ErTp,Contract,Rp);
    NAct <- genAction(Actn,Contract,Cont,Path,Rp);
    valis combineActs(Lc,RtnUnit,some(NAct),Contract,ExTp)
  }
  genAction(tryCatchDo(Lc,Body,Hndlr,ExTp,ValTp,ErTp),Contract,Cont,Path,Rp) => do{
    HndlrTp .= typeOf(Hndlr);
    ConTp .= mkTypeExp(ExTp,[ErTp,ValTp]);
    BdyExp <- genAction(Body,Contract,.none,Path,Rp);
    BType .= typeOf(BdyExp);
    LamType .= funType([BType,HndlrTp],ConTp);
    H .= over(Lc,mtd(Lc,"_handle",LamType),LamType,[typeConstraint(mkTypeExp(Contract,[ExTp]))]);
    HB .= apply(Lc,H,tple(Lc,[BdyExp,Hndlr]),ConTp);
    valis combineActs(Lc,HB,Cont,Contract,ExTp)
  }
  genAction(throwDo(Lc,Exc,ExTp,VlTp,ErTp),Contract,Cont,Path,Rp) => do{
    MdlTp .= mkTypeExp(ExTp,[ErTp,VlTp]);
    ThrTp .= funType([ErTp],MdlTp);
    Gen .= over(Lc,mtd(Lc,"_raise",ThrTp),ThrTp,[typeConstraint(mkTypeExp(Contract,[ExTp]))]);
    valis combineActs(Lc,apply(Lc,Gen,tple(Lc,[Exc]),MdlTp),Cont,Contract,ExTp)
  }
  genAction(simpleDo(Lc,Exp,ExTp),Contract,Cont,Path,Rp) => do{
    valis combineActs(Lc,Exp,Cont,Contract,ExTp)
  }
  genAction(performDo(Lc,Exp,ExTp,VlTp,ErTp),Contract,Cont,Path,Rp) => do{
    Perf <- genReturn(Lc,Exp,ExTp,VlTp,ErTp,Contract,Rp);
    valis combineActs(Lc,Perf,Cont,Contract,ExTp)
  }
  genAction(ifThenElseDo(Lc,Tst,Th,El,ExTp,ValTp,ErTp),Contract,Cont,Path,Rp) => do{
    MdlTp .= mkTypeExp(ExTp,[ErTp,ValTp]);
    Then <- genAction(Th,Contract,.none,Path,Rp);
    Else <- genAction(El,Contract,.none,Path,Rp);
    if isIterableGoal(Tst) then {
      ITst <- genIterableGoal(Tst,Contract,Path,Rp);
      valis combineActs(Lc,cond(Lc,ITst,Then,Else),Cont,Contract,ExTp)
    } else{
      valis combineActs(Lc,cond(Lc,Tst,Then,Else),Cont,Contract,ExTp)
    }
  }
  /* Construct a local iterator function:
   let{
     loop() => do{ if C then { B; loop() } else  <Cont> 
   } in loop()
  */
  genAction(whileDo(Lc,Tst,Bdy,StTp,ErTp),Contract,some(Cont),Path,Rp) => do{
    Fn .= genSym("loop");
    LclName .= qualifiedName(Path,.valMark,Fn);
    ThPath .= qualifiedName(Path,.valMark,"lp");
    UnitTp .= tupleType([]);
    LpTp .= mkTypeExp(StTp,[ErTp,UnitTp]);
    FnTp .= funType([],LpTp);
    FnCall .= apply(Lc,vr(Lc,Fn,FnTp),tple(Lc,[]),LpTp);
    Then <- genAction(seqnDo(Lc,Bdy,simpleDo(Lc,FnCall,StTp)),
      Contract,.none,ThPath,Rp);
    FF .= varDef(Lc,Fn,LclName,lambda(Lc,[eqn(Lc,tple(Lc,[]),.none,cond(Lc,Tst,Then,Cont))],FnTp),[],FnTp);
    valis letExp(Lc,[FF],FnCall)
  }
    /*
   for C do {A}
  becomes:
  
  <iterator>( do{return ()}, (Lcls,St) => do {A; return St})
*/
  genAction(forDo(Lc,Tst,Body,StTp,ErTp),Contract,Cont,Path,Rp) => do{
    Unit .= tple(Lc,[]);
    UnitTp .= tupleType([]);
    Zed <- genReturn(Lc,Unit,StTp,UnitTp,ErTp,Contract,Rp);
    IterBody <- genAction(Body,Contract,some(Zed),Path,Rp);
    ForLoop <- genCondition(Tst,Path,
      genRtn(Lc,StTp,UnitTp,ErTp,Contract,Rp),
      genSeq(Lc,StTp,UnitTp,ErTp,Contract,Rp),
      genForBody(Lc,StTp,UnitTp,ErTp,Contract,Rp,IterBody),
      unlifted(Unit),Rp);
    valis combineActs(Lc,ForLoop,Cont,Contract,StTp)
  }
  genAction(Act,_,_,_,Rp) =>
    other(reportError(Rp,"cannot handle action $(Act)",locOf(Act))).

  genForBody(Lc,StTp,VlTp,ErTp,Contract,Rp,IterBody) => let{
    f(E) => combineActs(Lc,IterBody,some(genRtn(Lc,StTp,VlTp,ErTp,Contract,Rp)(E)),Contract,StTp).
  } in f.

  public genReturn:(locn,canon,tipe,tipe,tipe,tipe,reports) => either[reports,canon].
  genReturn(Lc,A,ExTp,VlTp,ErTp,Contract,Rp) => do{
    ActionTp .= mkTypeExp(ExTp,[ErTp,VlTp]);
    MtdTp .= funType([VlTp],ActionTp);
    Gen .= over(Lc,mtd(Lc,"_valis",MtdTp),MtdTp,
      [typeConstraint(mkTypeExp(Contract,[ExTp]))]);
    valis apply(Lc,Gen,tple(Lc,[A]),ActionTp)
  }

  public genRtn:(locn,tipe,tipe,tipe,tipe,reports) => canonLift.
  genRtn(Lc,StTp,VlTp,ErTp,Contract,Rp) => let{
    f(lifted(Exp))=>Exp.
    f(unlifted(Exp)) =>
      valof genReturn(Lc,Exp,StTp,VlTp,ErTp,Contract,Rp).
  } in f.

  genVl:(locn,canon,tipe,tipe,tipe,reports) => (lifted)=>canon.
  genVl(Lc,Ptn,EitherTp,ErTp,Contract,Rp) => let{
    f(_)=> valof genReturn(Lc,Ptn,EitherTp,typeOf(Ptn),ErTp,Contract,Rp)
  } in f.

  public genPerform:(locn,canon,tipe,tipe,tipe) => canon.
  genPerform(Lc,A,Tp,ExTp,Contract) => let{
    MdTp = typeOf(A).
    Gen = over(Lc,mtd(Lc,"_perform",funType([MdTp],Tp)),funType([MdTp],Tp),[typeConstraint(mkTypeExp(Contract,[ExTp]))])
  } in apply(Lc,Gen,tple(Lc,[A]),MdTp).

  combineActs:(locn,canon,option[canon],tipe,tipe)=>canon.
  combineActs(_,A1,.none,_,_) => A1.
  combineActs(Lc,A1,some(A2),Contract,StTp) => let{.
    A1Tp = typeOf(A1).
    Anon = anonVar(Lc,A1Tp).
    ConTp = typeOf(A2).
    LamTp = funType([A1Tp],ConTp).
    Lam = lambda(Lc,[eqn(Lc,tple(Lc,[Anon]),.none,A2)],LamTp).
    SeqTp = funType([A1Tp,LamTp],ConTp).
    Gen = over(Lc,mtd(Lc,"_sequence",SeqTp),SeqTp,[typeConstraint(mkTypeExp(Contract,[StTp]))]).
  .} in apply(Lc,Gen,tple(Lc,[A1,Lam]),ConTp).

  genSeq(Lc,StTp,VlTp,ErTp,Contract,Rp) => let{
    genSq(St,Init,Reslt) => let{.
      ATp = typeOf(St).
      MdTp = mkTypeExp(StTp,[ErTp,ATp]).
      LamTp = funType([ATp],MdTp).
      Lam = lambda(Lc,[eqn(Lc,tple(Lc,[St]),.none,Reslt)],LamTp).
      SeqTp = funType([MdTp,LamTp],MdTp).
      Gen = over(Lc,mtd(Lc,"_sequence",SeqTp),SeqTp,[typeConstraint(mkTypeExp(Contract,[StTp]))]).
      Initial = genRtn(Lc,StTp,VlTp,ErTp,Contract,Rp)(Init).
    .} in apply(Lc,Gen,tple(Lc,[Initial,Lam]),MdTp)
  } in genSq.
  

  /*
  * An 'iterable' conditions become a match on the result of a search
  *
  * becomes:
  *
  * either(some(PtnV)) .= <genCondition>(C,either(.none),...)
  *
  * This will need more optimization ...
  */

  public genIterableGoal:(canon,tipe,string,reports)=>either[reports,canon].
  genIterableGoal(Cond,Contract,Path,Rp) => do{
    Lc .= locOf(Cond);
    EitherTp .= tpFun("star.either*either",2);
    OptionTp .= tpFun("star.core*option",1);
    VTpl .= tple(Lc,goalVars(Lc,Cond));
    VTtp .= typeOf(VTpl);
    UnitTp .= tupleType([]);
    OptTp .= mkTypeExp(OptionTp,[VTtp]);
    MTp .= mkTypeExp(EitherTp,[UnitTp,OptTp]);
    EitherFunTp .= funType([OptTp],MTp);
    Unit .= apply(Lc,vr(Lc,"either",EitherFunTp),
      tple(Lc,[vr(Lc,"none",OptTp)]),MTp);
    Zed <- genReturn(Lc,Unit,EitherTp,OptTp,UnitTp,Contract,Rp);
    Ptn .= apply(Lc,vr(Lc,"either",funType([OptTp],MTp)),
      tple(Lc,[apply(Lc,vr(Lc,"some",funType([VTtp],OptTp)),
	    tple(Lc,[VTpl]),OptTp)]),MTp);
    Seq <- genCondition(Cond,Path,genRtn(Lc,EitherTp,OptTp,UnitTp,Contract,Rp),
      genSeq(Lc,EitherTp,OptTp,UnitTp,Contract,Rp),
      genVl(Lc,Ptn,EitherTp,UnitTp,Contract,Rp),
      lifted(Zed),Rp);
    Gl .= genPerform(Lc,Seq,MTp,EitherTp,Contract);
    valis match(Lc,Ptn,Gl)
  }

  public genAbstraction:(locn,canon,canon,dict,string,reports) => either[reports,canon].
  genAbstraction(Lc,Bnd,Cond,Env,Path,Rp) => do{
    (ExContract,ExTp,VlTp) <- pickupContract(Lc,Env,"execution",Rp);
    (SeqContract,StTp,ElTp) <- pickupContract(Lc,Env,"sequence",Rp);
    (_,ActionTp,_) ^= findType(Env,"action");
    ErTp .= newTypeVar("_");
    SeqConstraint .= typeConstraint(funDeps(mkTypeExp(SeqContract,[StTp]),[ElTp]));
    if sameType(ActionTp,ExTp,Env) then{
      Zed <- genReturn(Lc,over(Lc,mtd(Lc,"_nil",StTp),StTp,[SeqConstraint]),ExTp,StTp,ErTp,ExContract,Rp);
      Gen .= over(Lc,mtd(Lc,"_cons",funType([ElTp,StTp],StTp)),funType([ElTp,StTp],StTp),
	[SeqConstraint]);
      Seq <- genCondition(Cond,Path,genRtn(Lc,ExTp,StTp,ErTp,ExContract,Rp),
	genSeq(Lc,StTp,ExTp,ErTp,ExContract,Rp),
	genEl(Lc,Gen,Bnd,StTp,ExContract,ExTp,ErTp,Rp),
	lifted(Zed),Rp);
      valis genPerform(Lc,Seq,StTp,ExTp,ExContract)
    }
    else
    throw reportError(Rp,"action type $(ActionTp) not consistent with sequence contract",Lc)
  }

  genEl(Lc,Gen,Bnd,StTp,ExTp,ErTp,ExContract,Rp) => let{
    f(unlifted(Strm)) => valof genReturn(Lc,apply(Lc,Gen,tple(Lc,[Bnd,Strm]),StTp),
      ExTp,StTp,ErTp,ExContract,Rp).
  } in f.
      
    
  public pickupContract:(locn,dict,string,reports) => either[reports,(tipe,tipe,tipe)].
  pickupContract(Lc,Env,Nm,Rp) => do{
    if Con ^= findContract(Env,Nm) then{
      (_,typeExists(funDeps(tpExp(Op,StTp),[ErTp]),_)) .=
	freshen(Con,Env);
      valis (Op,StTp,ErTp)
    } else
    throw reportError(Rp,"$(Nm) contract not defined",Lc)
  }

  public pickupIxContract:(locn,dict,string,reports) => either[reports,(tipe,tipe,tipe,tipe)].
  pickupIxContract(Lc,Env,Nm,Rp) => do{
    if Con ^= findContract(Env,Nm) then{
      (_,typeExists(funDeps(tpExp(Op,IxTp),[KyTp,VlTp]),_)) .=
	freshen(Con,Env);
      valis (Op,IxTp,KyTp,VlTp)
    } else
      throw reportError(Rp,"$(Nm) contract not defined",Lc)
  }

  anonVar(Lc,Tp) => genVar(Lc,"_",Tp).
  genVar(Lc,Nm,Tp) => vr(Lc,genSym(Nm),Tp).

  -- Convert iterable conditions into iterations

  public lifted ::= lifted(canon) | unlifted(canon).

  canonLift ~> (lifted)=>canon.

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
  public genCondition:(canon,string,canonLift,(canon,lifted,canon)=>canon,canonLift,lifted,reports) =>
    either[reports,canon].
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
    (Pttrn,PtnCond) .= splitPtn(Ptn);
    Dflt .= Lift(unlifted(St));
    MdlTp .= typeOf(AddToFront);
    sFTp .= funType([PtnTp,ResltTp],MdlTp);
    -- entangle iterator type with monad
    _ .= sameType(funType([SrcTp,MdlTp,sFTp],MdlTp),ItrTp,[]);
    logMsg("local fun type: $(sFTp)");
    sF .= genSym("f");
    ThPath .= genNewName(Path,"Γ");
    LclName .= packageVarName(ThPath,sF);

    FF.=varDef(Lc,sF,LclName,lambda(Lc,[
	  eqn(Lc,tple(Lc,[Pttrn,St]),PtnCond,AddToFront),
	  eqn(Lc,tple(Lc,[Anon,St]),.none,Dflt)
	],sFTp),[],sFTp);

    Let .= letExp(Lc,[FF],vr(Lc,sF,sFTp));
    Init .= Lift(Initial);
    valis apply(Lc,Iterator,tple(Lc,[Src,Init,Let]),MdlTp)
  }
  genCondition(conj(_Lc,A,B),Path,Lift,Seq,Succ,Initial,Rp) =>
    genCondition(A,Path,Lift,Seq,(Lf)=>valof genCondition(B,Path,Lift,Seq,Succ,Lf,Rp),Initial,Rp).
  genCondition(disj(_Lc,A,B),Path,Lift,Seq,Succ,Initial,Rp) => do{
    E1<-genCondition(A,Path,Lift,Seq,Succ,Initial,Rp);
    genCondition(B,Path,Lift,Seq,Succ,lifted(E1),Rp)
  }
  genCondition(neg(Lc,A),Path,Lift,Seq,Succ,Initial,Rp) => do{
    Negated <- genCondition(A,Path,Lift,Seq,(_)=>Lift(unlifted(vr(Lc,"true",boolType))),
      unlifted(vr(Lc,"false",boolType)),Rp);
    St .= anonVar(Lc,boolType);
    SuccCase .= Succ(Initial);
    FalseCase .= Lift(Initial);
    Tp .= typeOf(FalseCase);
    valis Seq(St,lifted(Negated),cond(Lc,St,FalseCase,SuccCase))
  }
  genCondition(implies(Lc,A,B),Path,Lift,Seq,Succ,Initial,Rp) =>
    genCondition(neg(Lc,conj(Lc,A,neg(Lc,B))),Path,Lift,Seq,Succ,Initial,Rp).
  genCondition(Other,Path,Lift,Seq,Succ,Initial,Rp) => do{
    AddToSucc .= Succ(Initial);
    StTp .= newTypeVar("_");
    St .= anonVar(locOf(Other),StTp);
    Tp .= typeOf(AddToSucc);
    Init .= Lift(Initial);
    valis Seq(St,Initial,cond(locOf(Other),Other,AddToSucc,Init))
  }
}

  
