star.compiler.gencode{
  import star.
  import star.pkg.
  import star.sort.

  import star.compiler.asm.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  label ::= label(string) | ulbl(integer).

  srcLoc ::= lclVar(integer,tipe) | argVar(integer,tipe) | glbVar(string,tipe) | cdeVar(string,tipe,codeSegment).

  codeCtx ::= codeCtx(map[string,srcLoc],locn,integer).

  emptyCtx(Lc) => codeCtx([],Lc,0).

  public compDefs:(list[crDefn],compilerOptions,list[codeSegment],reports)=>
    either[reports,list[codeSegment]].
  compDefs([],_,Cs,_)=>either(Cs).
  compDefs([D,..Dfs],Opts,Cs,Rp) => do{
    Code<-compDefn(D,Opts,Rp);
    compDefs(Dfs,Opts,[Cs..,Code],Rp)
  }

  public compDefn:(crDefn,compilerOptions,reports) => either[reports,codeSegment].
  compDefn(fnDef(Lc,Nm,Tp,Args,Val),Opts,Rp) => do{
    Ctx = emptyCtx(Lc);
    Ctxa = argVars(Args,Ctx,0);
    (Code,Stk) <- compExp(Val,Opts,Ctxa,[],Rp);
    if [Top].=Stk then
      valis codeSeg(tLbl(Nm,size(Args)),Tp,block(Top,Code))
    else
    throw reportError(Rp,"top of stack should have exactly one type, not $(Stk)",Lc)
  }

  compExp:(crExp,compilerOptions,codeCtx,list[tipe],reports) =>
    either[reports,(list[assemOp],list[tipe])].
  compExp(crInt(Lc,Ix),_,Ctx,Stk,Rp) => either(([iLdC(intgr(Ix))],[Stk..,intType])).
  compExp(crFlot(Lc,Dx),_,Ctx,Stk,Rp) => either(([iLdC(flot(Dx))],[Stk..,fltType])).
  compExp(crStrg(Lc,Sx),_,Ctx,Stk,Rp) => either(([iLdC(strg(Sx))],[Stk..,strType])).
  compExp(crLbl(Lc,Nm,Ar,Tp),_,Ctx,Stk,Rp) => either(([iLdC(enum(Nm))],[Stk..,Tp])).
  compExp(crVar(Lc,crId(Vr,Tp)),Opts,Ctx,Stk,Rp) => do{
    if Loc^=locateVar(Vr,Ctx) then {
      compVar(Lc,Loc,Opts,Ctx,Stk,Rp)
    } else
    throw reportError(Rp,"cannot locate variable $(Vr)\:$(Tp)",Lc)
  }
  compExp(crTerm(Lc,crLbl(_,Nm,Ar,_),Args,Tp),Opts,Ctx,Stk,Rp) => do{
    if size(Args)==Ar then{
      (ArgCode,ArgStk) <- compExps(Args,Opts,Ctx,[],Rp);
      if size(ArgStk) == Ar then {
	valis ([ArgCode..,iAlloc(tLbl(Nm,Ar))],[Stk..,Tp])
      }
      else
      throw reportError(Rp,"stack depth should be $(Ar), got $(size(ArgStk))",Lc)
    }
    else
    throw reportError(Rp,"expecting $(Ar) args, got $(Args)",Lc)
  }
  compExp(crECall(Lc,Nm,Args,Tp),Opts,Ctx,Stk,Rp) => do{
    (LcC,Ctx0) = changeLoc(Lc,Opts,Ctx);
    (ArgCode,ArgStk) <- compExps(Args,Opts,Ctx0,[],Rp);
    (LcX,_) = changeLoc(locOf(Ctx),Opts,Ctx0);
    valis (LcC++[ArgCode..,iEscape(Nm)]++LcX,[Stk..,Tp])
  }
  compExp(crCall(Lc,crLbl(_,Nm,Ar,_),Args,Tp),Opts,Ctx,Stk,Rp) => do{
    if size(Args)==Ar then{
      (LcC,Ctx0) = changeLoc(Lc,Opts,Ctx);
      (ArgCode,ArgStk) <- compExps(Args,Opts,Ctx0,[],Rp);
      (LcX,_) = changeLoc(locOf(Ctx),Opts,Ctx0);
      valis (LcC++ArgCode++[iCall(tLbl(Nm,Ar))]++LcX,[Stk..,Tp])
    }
    else
    throw reportError(Rp,"expecting $(Ar) args, got $(Args)",Lc)
  }
  compExp(crCall(Lc,Op,Args,Tp),Opts,Ctx,Stk,Rp) => do{
    (LcC,Ctx0) = changeLoc(Lc,Opts,Ctx);
    (ArgCode,ArgStk) <- compExps(Args,Opts,Ctx0,[],Rp);
    (OpCode,OpStk) <- compExp(Op,Opts,Ctx,ArgStk,Rp);
    (LcX,_) = changeLoc(locOf(Ctx),Opts,Ctx0);
    valis (LcC++ArgCode++[OpCode..,iOCall(size(Args))]++LcX,[Stk..,Tp])
  }
  compExp(crOCall(Lc,Op,Args,Tp),Opts,Ctx,Stk,Rp) => do{
    (LcC,Ctx0) = changeLoc(Lc,Opts,Ctx);
    (ArgCode,ArgStk) <- compExps(Args,Opts,Ctx0,[],Rp);
    (OpCode,OpStk) <- compExp(Op,Opts,Ctx,ArgStk,Rp);
    (LcX,_) = changeLoc(locOf(Ctx),Opts,Ctx0);
    valis (LcC++ArgCode++[OpCode..,iOCall(size(Args))]++LcX,[Stk..,Tp])
  }
  compExp(crRecord(Lc,Nm,Fields,Tp),Opts,Ctx,Stk,Rp) => do{
    Sorted = sort(Fields,((F1,_),(F2,_))=>F1<F2);
    (ElCode,ElStk) <- compExps(Sorted//((_,V))=>V,Opts,Ctx,[],Rp);
    valis ([ElCode..,iAlloc(tStrct(Nm,Sorted//((Nm,V))=>(Nm,typeOf(V))))],[Stk..,Tp])
  }
  compExp(crDte(Lc,Rc,Fld,Tp),Opts,Ctx,Stk,Rp) => do{
    (RecCode,RecStk) <- compExp(Rc,Opts,Ctx,[],Rp);
    valis (RecCode++[iGetFld(Fld)],[Stk..,Tp])
  }
  compExp(crTplDte(Lc,Rc,Ix,Tp),Opts,Ctx,Stk,Rp) => do{
    (RecCode,RecStk) <- compExp(Rc,Opts,Ctx,[],Rp);
    valis (RecCode++[iNth(Ix)],[Stk..,Tp])
  }
  compExp(crLtt(Lc,V,Val,Exp),Opts,Ctx,Stk,Rp) => do{
    (VlCode,VlStk) <- compExp(Val,Opts,Ctx,[],Rp);
    (Off,Ctx1) = defineLclVar(V,Ctx);
    (Code,RStk) <- compExp(Exp,Opts,Ctx1,Stk,Rp);
    valis (VlCode++[iStL(Off)]++Code,RStk)
  }
  compExp(C,Opts,Ctx,Stk,Rp) where isCrCond(C) => do{
    Code <- compCond(C,block(boolType,[iLdC(enum("star.core$true"))]),
      block(boolType,[iLdC(enum("star.core$false"))]),Opts,Ctx,Rp);
    valis (Code,[Stk..,boolType])
  }

  compExps:(list[crExp],compilerOptions,codeCtx,list[tipe],reports) =>
    either[reports,(list[assemOp],list[tipe])].
  compExps([],_,Ctx,Stk,_)=>either(([],Stk)).
  compExps([El,..Es],Opts,Ctx,Stk,Rp)=> do{
    (ElCde,AStk) <- compExp(El,Opts,Ctx,Stk,Rp);
    (ACde,RStk) <- compExps(Es,Opts,Ctx,AStk,Rp);
    valis (ElCde++ACde,RStk)
  }
  
  compVar:(locn,srcLoc,compilerOptions,codeCtx,list[tipe],reports)=>
    either[reports,(list[assemOp],list[tipe])].
  compVar(Lc,argVar(Off,Tp),_,Ctx,Stk,_) => either(([iLdA(Off)],[Stk..,Tp])).
  compVar(Lc,lclVar(Off,Tp),_,Ctx,Stk,_) => either(([iLdL(Off)],[Stk..,Tp])).
  compVar(Lc,glbVar(Nm,Tp),_,Ctx,Stk,_) => either(([iLdG(Nm)],[Stk..,Tp])).

  compCond:(crExp,codeBlock,codeBlock,compilerOptions,codeCtx,reports) => either[reports,list[assemOp]].
  compCond(crCnj(Lc,L,R),IfT,IfF,Opts,Ctx,Rp) => do{
    Rcde <- compCond(R,IfT,IfF,Opts,Ctx,Rp);
    compCond(L,block(boolType,Rcde),IfF,Opts,Ctx,Rp)
  }
  compCond(crDsj(Lc,L,R),IfT,IfF,Opts,Ctx,Rp) => do{
    Rcde <- compCond(R,IfT,IfF,Opts,Ctx,Rp);
    Lcde <- compCond(L,IfT,block(boolType,Rcde),Opts,Ctx,Rp);
    valis Lcde
  }
  compCond(crNeg(Lc,R),IfT,IfF,Opts,Ctx,Rp) =>
    compCond(R,IfF,IfT,Opts,Ctx,Rp).
  compCond(crCnd(Lc,T,L,R),IfT,IfF,Opts,Ctx,Rp) => do{
    Rcde <- compCond(R,IfT,IfF,Opts,Ctx,Rp);
    Lcde <- compCond(L,IfT,IfF,Opts,Ctx,Rp);
    compCond(T,block(boolType,Lcde),block(boolType,Rcde),Opts,Ctx,Rp)
  }
  compCond(crWhere(Lc,Exp,Cond),IfT,IfF,Opts,Ctx,Rp) =>
    compCond(crCnj(Lc,Exp,Cond),IfT,IfF,Opts,Ctx,Rp).
  compCond(crMatch(Lc,Exp,Ptn),IfT,IfF,Opts,Ctx,Rp) => do{
    (Cde,[sTp]) <- compExp(Exp,Opts,Ctx,[],Rp);
    compPtn(Ptn,IfT,IfF,Opts,Ctx,sTp,Rp)
  }
  compCond(Exp,IfT,IfF,Opts,Ctx,Rp) => do{
    (Cde,Stk) <- compExp(Exp,Opts,Ctx,[],Rp);
    if \+size(Stk)==1 then
      throw reportError(Rp,"expecting a single entry on stack",locOf(Exp))
    else
    valis [Cde..,iIf(IfT,IfF)]
  }

  compPtn:(crExp,codeBlock,codeBlock,compilerOptions,codeCtx,tipe,reports) => either[reports,list[assemOp]].
  compPtn(crVar(Lc,crId(Vr,Tp)),IfT,_,Opts,Ctx,STp,Rp) where Loc ^= locateVar(Vr,Ctx) => do{
    compPtnVar(Lc,Loc,IfT,Opts,Ctx,[],Rp)
  }
  compPtn(crVar(Lc,crId(Vr,Tp)),IfT,_,Opts,Ctx,STp,Rp) => either([iStL(-1),iEnter(IfT)]).

  compPtnVar:(locn,srcLoc,codeBlock,compilerOptions,codeCtx,list[tipe],reports)=>
    either[reports,list[assemOp]].
  compPtnVar(Lc,lclVar(Off,Tp),Nxt,_,Ctx,Stk,_) => either([iStL(Off),iEnter(Nxt)]).
  compPtnVar(Lc,Loc,_,_,_,_,Rp) => other(reportError(Rp,"cannot target var in pattern",Lc)).

  locateVar:(string,codeCtx)=>option[srcLoc].
    locateVar(Nm,codeCtx(Vars,_,_)) => Vars[Nm].

  defineLclVar:(crVar,codeCtx) => (integer,codeCtx).
  defineLclVar(crId(Nm,Tp),codeCtx(Vrs,Lc,Count))=>
    (Count,codeCtx(Vrs[Nm->lclVar(Count,Tp)],Lc,Count+1)).

  changeLoc:(locn,compilerOptions,codeCtx)=>(list[assemOp],codeCtx).
  changeLoc(Lc,_,codeCtx(Vars,Lc0,Dp)) where Lc=!=Lc0 => ([iLine(Lc)],codeCtx(Vars,Lc,Dp)).
  changeLoc(_,_,Ctx)=>([],Ctx).

  implementation hasLoc[codeCtx] => {
    locOf(codeCtx(_,Lc,_))=>Lc.
  }

  ptnVars:(crExp,codeCtx) => codeCtx.
  ptnVars(crVar(_,crId(Nm,Tp)),codeCtx(Vars,CLc,Count)) where _ ^= Vars[Nm] => codeCtx(Vars,CLc,Count).
  ptnVars(crVar(_,crId(Nm,Tp)),codeCtx(Vars,CLc,Count)) => codeCtx(Vars[Nm->lclVar(Count,Tp)],CLc,Count+1).
  ptnVars(crInt(_,_),Ctx) => Ctx.
  ptnVars(crFlot(_,_),Ctx) => Ctx.
  ptnVars(crStrg(_,_),Ctx) => Ctx.
  ptnVars(crLbl(_,_,_,_),Ctx) => Ctx.
  ptnVars(crTerm(_,Op,Els,_),Ctx) => foldRight(ptnVars,ptnVars(Op,Ctx),Els).
  ptnVars(crCall(_,_,_,_),Ctx) => Ctx.
  ptnVars(crECall(_,_,_,_),Ctx) => Ctx.
  ptnVars(crOCall(_,_,_,_),Ctx) => Ctx.
  ptnVars(crRecord(_,_,Els,_),Ctx) => foldRight(((_,El),X)=>ptnVars(El,X),Ctx,Els).
  ptnVars(crDte(_,_,_,_),Ctx) => Ctx.
  ptnVars(crTplDte(_,_,_,_),Ctx) => Ctx.
  ptnVars(crCnj(_,L,R),Ctx) => ptnVars(L,ptnVars(R,Ctx)).
  ptnVars(crDsj(_,L,R),Ctx) => mergeCtx(ptnVars(L,Ctx),ptnVars(R,Ctx),Ctx).
  ptnVars(crNeg(_,R),Ctx) => Ctx.
  ptnVars(crCnd(Lc,T,L,R),Ctx) => mergeCtx(ptnVars(crCnj(Lc,T,L),Ctx),ptnVars(R,Ctx),Ctx).
  ptnVars(crLtt(_,_,_,_),Ctx) => Ctx.
  ptnVars(crCase(_,_,_,_,_),Ctx) => Ctx.
  ptnVars(crAbort(_,_,_),Ctx) => Ctx.
  ptnVars(crWhere(_,P,C),Ctx) => ptnVars(C,ptnVars(P,Ctx)).
  ptnVars(crMatch(_,P,_),Ctx) => ptnVars(P,Ctx).

  argVars:(list[crVar],codeCtx,integer) => codeCtx.
  argVars([],Ctx,_)=>Ctx.
  argVars([crId(Nm,Tp),..As],codeCtx(Vars,CLc,Count),Arg) =>
    argVars(As,codeCtx(Vars[Nm->argVar(Arg,Tp)],CLc,Count+1),Arg+1).
  argVars([_,..As],Ctx,Arg) => argVars(As,Ctx,Arg+1).

  mergeCtx:(codeCtx,codeCtx,codeCtx)=>codeCtx.
  mergeCtx(codeCtx(LV,_,_),codeCtx(RV,_,_),Base) => let{
    mergeVar:(string,srcLoc,codeCtx) => codeCtx.
    mergeVar(Nm,_,Vrs) where _ ^= locateVar(Nm,Base) => Vrs.
    margeVar(Nm,_,codeCtx(Vs,Lc,Count)) where lclVar(_,Tp) ^= RV[Nm] => codeCtx(Vs[Nm->lclVar(Count,Tp)],Lc,Count+1).
  } in ixRight(mergeVar,Base,LV).

}
