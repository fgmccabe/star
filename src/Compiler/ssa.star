star.compiler.ssa{
  -- Automatically Generated Assembler -- Do NOT Edit
  import star.
  import star.multi.
  import star.sort.

  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.location.
  import star.compiler.data.
  import star.compiler.meta.
  import star.compiler.types.
  import star.compiler.encode.
  import star.compiler.ltipe.

  public codeSegment ::= .func(termLbl,codePolicy,ltipe,cons[(string,data)],multi[insOp]) |
    .struct(termLbl,tipe,integer) |
    .tipe(tipe,typeRule,map[termLbl,integer]).

  public insOp ::=
    | .iHalt(varNm)
    | .iAbort(data, varNm)
    | .iCall(termLbl, cons[varNm])
    | .iOCall(varNm, cons[varNm])
    | .iEscape(string, cons[varNm])
    | .iTCall(termLbl, cons[varNm])
    | .iTOCall(varNm, cons[varNm])
    | .iRSP(varNm)
    | .iRSX(assemLbl, varNm)
    | .iEntry(cons[varNm], cons[varNm])
    | .iRtn
    | .iRet(varNm)
    | .iXRet(varNm)
    | .iLoop(multi[insOp])
    | .iBlock(cons[varNm], multi[insOp])
    | .iBreak(assemLbl)
    | .iResult(assemLbl, cons[varNm])
    | .iCont(assemLbl)
    | .iIf(assemLbl, varNm)
    | .iIfNot(assemLbl, varNm)
    | .iICase(varNm, multi[insOp])
    | .iCase(varNm, multi[insOp])
    | .iIxCase(varNm, multi[insOp])
    | .iCLbl(termLbl, assemLbl, varNm)
    | .iCInt(data, assemLbl, varNm)
    | .iCChar(data, assemLbl, varNm)
    | .iCFlt(data, assemLbl, varNm)
    | .iCLit(data, assemLbl, varNm)
    | .iMC(varNm, data)
    | .iMv(varNm, varNm)
    | .iLG(string)
    | .iSG(string, varNm)
    | .iSav(varNm)
    | .iLdSav(varNm, assemLbl, varNm)
    | .iTstSav(varNm, varNm)
    | .iStSav(varNm, varNm, varNm)
    | .iCell(varNm, varNm)
    | .iGet(varNm, varNm)
    | .iAssign(varNm, varNm)
    | .iNth(varNm, integer, varNm)
    | .iStNth(varNm, integer, varNm)
    | .iIAdd(varNm, varNm, varNm)
    | .iISub(varNm, varNm, varNm)
    | .iIMul(varNm, varNm, varNm)
    | .iIDiv(assemLbl, varNm, varNm, varNm)
    | .iIMod(assemLbl, varNm, varNm, varNm)
    | .iIAbs(varNm, varNm)
    | .iIEq(varNm, varNm, varNm)
    | .iILt(varNm, varNm, varNm)
    | .iIGe(varNm, varNm, varNm)
    | .iCEq(varNm, varNm, varNm)
    | .iCLt(varNm, varNm, varNm)
    | .iCGe(varNm, varNm, varNm)
    | .iBAnd(varNm, varNm, varNm)
    | .iBOr(varNm, varNm, varNm)
    | .iBXor(varNm, varNm, varNm)
    | .iBLsl(varNm, varNm, varNm)
    | .iBLsr(varNm, varNm, varNm)
    | .iBAsr(varNm, varNm, varNm)
    | .iBNot(varNm, varNm)
    | .iFAdd(varNm, varNm, varNm)
    | .iFSub(varNm, varNm, varNm)
    | .iFMul(varNm, varNm, varNm)
    | .iFDiv(assemLbl, varNm, varNm, varNm)
    | .iFMod(assemLbl, varNm, varNm, varNm)
    | .iFAbs(varNm, varNm)
    | .iFEq(varNm, varNm, varNm)
    | .iFLt(varNm, varNm, varNm)
    | .iFGe(varNm, varNm, varNm)
    | .iAlloc(termLbl, varNm, cons[varNm])
    | .iClosure(termLbl, varNm, varNm)
    | .iBump(varNm)
    | .iDrop(varNm)
    | .iFiber(varNm, varNm)
    | .iSuspend(varNm, varNm)
    | .iResume(varNm, varNm)
    | .iRetire(varNm, varNm)
    | .iUnderflow
    | .iLine(data)
    | .iBind(data, varNm)
    | .iDBug(data)

    | .iLbl(string, insOp).

  public assemLbl ~> string.

  public varNm ~> string.

  public assem:(codeSegment) => data.
  assem(Df) => case Df in {
    | .func(Nm,H,Sig,LSpecs,Ins) => valof{
      funSig = .strg(Sig::string);
      (Lt0,_) = findLit([],.symb(Nm));
      (Lt1,tpIx) = findLit(Lt0,funSig);
      (Ags,Lcs) = findEntryInstruction(Ins);
      LclMap = declareArgs(Ags,declareLocals(Lcs,{}));
      (Code,_,Lts) = assemBlock(Ins,[],0,[],Lt1,LclMap);
       valis mkCons("code",
              [.symb(Nm),encPolicy(H),.intgr(tpIx),
              .intgr(size(Lcs)),litTbl(Lts),mkTpl(Code::cons[data]),
    	  mkTpl(sortVInfo(varInfos(LSpecs,LclMap)))])
    }
    | .struct(Lbl,Tp,Ix) => mkCons("struct",[.symb(Lbl),.strg(encodeSignature(Tp)),.intgr(Ix)])
    | .tipe(Tp,TpRl,Map) => mkCons("type",[.strg(tpName(Tp)),.strg(encodeTpRlSignature(TpRl)),encodeMap(Map)])
  }.

  varInfos:(cons[(string,data)],map[string,integer])=>cons[data].
  varInfos(Lcs,Map) => (try
    (Lcs//(((V,Spec))=>mkTpl([.strg(V),.intgr(_optval(Map[V])),Spec])))
    catch { _ => valof{ reportTrap("Invalid map"); valis []}}
  ).

  sortVInfo:(cons[data])=>cons[data].
  sortVInfo(Els) => sort(Els,((.term(_,[_,.intgr(I1),_]),.term(_,[_,.intgr(I2),_])) => I1<I2)).

  declareArgs:(cons[string],map[string,integer]) => map[string,integer].
  declareArgs(Lst,Mp) => let{.
    declare([],_,Map) => Map.
    declare([Nm,..Vrs],Ax,Map) =>
      declare(Vrs,Ax+1,Map[Nm->Ax]).
  .} in declare(Lst,0,Mp).          -- First arg is 0

  declareLocals:(cons[string],map[string,integer]) => map[string,integer].
  declareLocals(Lcs,Map) => let{.
    decl([],_,Mp) => Mp.
    decl([Vr,..Ls],Ix,Mp) => decl(Ls,Ix-1,Mp[Vr -> Ix]).
  .} in decl(Lcs,-1,Map).             -- First local is -1

  encodeMap(Entries) => mkTpl(ixRight((Lbl,Ix,Lst)=>[mkTpl([.symb(Lbl),.intgr(Ix)]),..Lst],[],Entries)).

  encPolicy(.hardDefinition) => mkTpl([]).
  encPolicy(.softDefinition) => mkTpl([.strg("soft")]).

  lblLevel ~> option[assemLbl].

  private assemBlock:(multi[insOp],multi[data],integer,
                      cons[lblLevel],map[data,integer],map[string,integer]) =>
                                        (multi[data],integer,map[data,integer]).
  assemBlock([],Code,Pc,Lbls,Lts,_Lcx) => (Code,Pc,Lts).
  assemBlock([I,..Ins],SoFar,Pc,Lbs,Lts,Lcx) => valof{
    (Code,Pc1,Lt0) = mnem(I,Pc,Lbs,Lts,Lcx);
    valis assemBlock(Ins,SoFar++Code,Pc1,Lbs,Lt0,Lcx)
  }

  private mnem:(insOp,integer,cons[lblLevel],map[data,integer],map[string,integer]) =>
    (multi[data],integer,map[data,integer]).
  mnem(.iLbl(Lb,I),Pc,Lbls,Lts,Lcs) => mnem(I,Pc,[.some(Lb),..Lbls],Lts,Lcs).
  mnem(.iHalt(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(0),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iAbort(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(1),.intgr(L1),findLocal(V1,Lcs)],Pc+3,Lt1);
  }
  mnem(.iCall(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V0)); 
    valis ([.intgr(2),.intgr(L1),mkTpl(findLocals(V1,Lcs))],Pc+3,Lt1);
  }
  mnem(.iOCall(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(3),findLocal(V0,Lcs),mkTpl(findLocals(V1,Lcs))],Pc+3,Lt0).
  mnem(.iEscape(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(4),.strg(V0),mkTpl(findLocals(V1,Lcs))],Pc+3,Lt0).
  mnem(.iTCall(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V0)); 
    valis ([.intgr(5),.intgr(L1),mkTpl(findLocals(V1,Lcs))],Pc+3,Lt1);
  }
  mnem(.iTOCall(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(6),findLocal(V0,Lcs),mkTpl(findLocals(V1,Lcs))],Pc+3,Lt0).
  mnem(.iRSP(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(7),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iRSX(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(8),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iEntry(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(9),.intgr(size(V0)),.intgr(size(V1))],Pc+3,Lt0).
  mnem(.iRtn, Pc,Lbls,Lt0,Lcs) => ([.intgr(10)],Pc+1,Lt0).
  mnem(.iRet(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(11),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iXRet(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(12),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iLoop(V0), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V0,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(13),mkTpl(A1::cons[data])],Pc+2,Lt1);
  }
  mnem(.iBlock(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V1,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(14),mkTpl(findLocals(V0,Lcs)),mkTpl(A1::cons[data])],Pc+3,Lt1);
  }
  mnem(.iBreak(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(15),.intgr(findLevel(Lbls,V0))],Pc+2,Lt0).
  mnem(.iResult(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(16),.intgr(findLevel(Lbls,V0)),mkTpl(findLocals(V1,Lcs))],Pc+3,Lt0).
  mnem(.iCont(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(17),.intgr(findLevel(Lbls,V0))],Pc+2,Lt0).
  mnem(.iIf(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(18),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iIfNot(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(19),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iICase(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V1,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(20),findLocal(V0,Lcs),mkTpl(A1::cons[data])],Pc+3,Lt1);
  }
  mnem(.iCase(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V1,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(21),findLocal(V0,Lcs),mkTpl(A1::cons[data])],Pc+3,Lt1);
  }
  mnem(.iIxCase(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V1,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(22),findLocal(V0,Lcs),mkTpl(A1::cons[data])],Pc+3,Lt1);
  }
  mnem(.iCLbl(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V0)); 
    valis ([.intgr(23),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iCInt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(24),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iCChar(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(25),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iCFlt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(26),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iCLit(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(27),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iMC(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V1); 
    valis ([.intgr(28),findLocal(V0,Lcs),.intgr(L1)],Pc+3,Lt1);
  }
  mnem(.iMv(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(29),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iLG(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(30),.strg(V0)],Pc+2,Lt0).
  mnem(.iSG(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(31),.strg(V0),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iSav(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(32),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iLdSav(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(33),findLocal(V0,Lcs),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iTstSav(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(34),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iStSav(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(35),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iCell(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(36),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iGet(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(37),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iAssign(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(38),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iNth(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(39),findLocal(V0,Lcs),.intgr(V1),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iStNth(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(40),findLocal(V0,Lcs),.intgr(V1),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iIAdd(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(41),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iISub(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(42),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iIMul(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(43),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iIDiv(V0, V1, V2, V3), Pc,Lbls,Lt0,Lcs) => ([.intgr(44),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs),findLocal(V2,Lcs),findLocal(V3,Lcs)],Pc+5,Lt0).
  mnem(.iIMod(V0, V1, V2, V3), Pc,Lbls,Lt0,Lcs) => ([.intgr(45),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs),findLocal(V2,Lcs),findLocal(V3,Lcs)],Pc+5,Lt0).
  mnem(.iIAbs(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(46),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iIEq(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(47),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iILt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(48),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iIGe(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(49),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iCEq(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(50),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iCLt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(51),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iCGe(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(52),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBAnd(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(53),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBOr(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(54),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBXor(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(55),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBLsl(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(56),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBLsr(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(57),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBAsr(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(58),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBNot(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(59),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iFAdd(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(60),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFSub(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(61),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFMul(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(62),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFDiv(V0, V1, V2, V3), Pc,Lbls,Lt0,Lcs) => ([.intgr(63),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs),findLocal(V2,Lcs),findLocal(V3,Lcs)],Pc+5,Lt0).
  mnem(.iFMod(V0, V1, V2, V3), Pc,Lbls,Lt0,Lcs) => ([.intgr(64),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs),findLocal(V2,Lcs),findLocal(V3,Lcs)],Pc+5,Lt0).
  mnem(.iFAbs(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(65),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iFEq(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(66),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFLt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(67),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFGe(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(68),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iAlloc(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V0)); 
    valis ([.intgr(69),.intgr(L1),findLocal(V1,Lcs),mkTpl(findLocals(V2,Lcs))],Pc+4,Lt1);
  }
  mnem(.iClosure(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V0)); 
    valis ([.intgr(70),.intgr(L1),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iBump(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(71),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iDrop(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(72),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iFiber(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(73),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iSuspend(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(74),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iResume(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(75),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iRetire(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(76),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iUnderflow, Pc,Lbls,Lt0,Lcs) => ([.intgr(77)],Pc+1,Lt0).
  mnem(.iLine(V0), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(78),.intgr(L1)],Pc+2,Lt1);
  }
  mnem(.iBind(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(79),.intgr(L1),findLocal(V1,Lcs)],Pc+3,Lt1);
  }
  mnem(.iDBug(V0), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(80),.intgr(L1)],Pc+2,Lt1);
  }

  mnem(I,Pc,Lbls,Lts,Lcs) => valof{
    reportTrap("Cannot assemble instruction $(I)");
    valis ([],Pc,Lts)
  }.

  findLit:(map[data,integer],data) => (map[data,integer],integer).
  findLit(Lts,T) where O ?= Lts[T] => (Lts,O).
  findLit(Lts,T) where O .= size(Lts) => (Lts[T->O],O).

  findLocal:(string,map[string,integer])=> data.
  findLocal(Nm,Lcs) => .intgr(try ? Lcs[Nm] catch { _ => unreachable }).

  findLocals:(cons[string],map[string,integer]) => cons[data].
  findLocals(Vrs,Lcs) => (Vrs//((V)=> findLocal(V,Lcs))).

  findLevel:(cons[lblLevel],assemLbl) => integer.
  findLevel(Lbs,Lb) => let{.
    findLvl([],_) => unreachable.
    findLvl([.some(LL),..Ls], Lvl) => (LL==Lb ?? Lvl || findLvl(Ls,Lvl)).
    findLvl([.none,..Ls],Lvl) => findLvl(Ls,Lvl+1).
  .} in findLvl(Lbs,0).

  litTbl:(map[data,integer]) => data.
  litTbl(Lts) => mkTpl(sort(Lts::cons[(data,integer)],((T1,Ix1), (T2,Ix2)) => Ix1<Ix2)//fst).

  showMap:(string,cons[(string,data)])=>string.
  showMap(_,[]) => "".
  showMap(Msg,Map) => "#(Msg)\: #(interleave(Map//(((Nm,_))=>Nm),", ")*)\n".

  public implementation display[codeSegment] => {
    disp(.func(Nm,_,Tp,Lcs,Ins)) => "fun $(Nm)\:$(Tp)\n"++showMap("locals",Lcs)++showMnem(Ins).
    disp(.struct(Lbl,Tp,Ix)) => "struct $(Lbl)\:$(Tp) @ $(Ix)".
    disp(.tipe(_Tp,TpRl,Map)) => "type $(TpRl), map = $(Map)".
  }

  public implementation display[insOp] => {
    disp(Op) => showIns(Op,0,0).0.
  }

  showMnem:(multi[insOp]) => string.
  showMnem(Ops) => showBlock(Ops,0,0).0.

  showBlock:(multi[insOp],integer,integer) => (string,integer).
  showBlock(Ins,Pc,Sps) => valof{
    (Blocks,Pcx) = showCode(Ins,Pc,Sps);
    valis ("\n"++interleave(Blocks,"\n")*,Pcx)
  }

  showCode:(multi[insOp],integer,integer) => (cons[string],integer).
  showCode([],Pc,_) => ([],Pc).
  showCode([Ins,..Cde],Pc,Sps) => valof{
    (Itxt,Pc1) = showIns(Ins,Pc,Sps);
    (Rest,Pcx) = showCode(Cde,Pc1,Sps);
    valis ([Itxt,..Rest],Pcx)
  }

  showIns:(insOp,integer,integer) => (string,integer).
  showIns(.iLbl(Lb,I),Pc,Sps) => valof{
    (Text,Pcx) = showIns(I,Pc,Sps);
    Prefix = "#(Lb):\n";
    valis (Prefix++Text,Pcx)
  }
  showIns(.iHalt(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))Halt #(V0)",Pc1+1)
  }
  showIns(.iAbort(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Abort $(V0), #(V1)",Pc2+1)
  }
  showIns(.iCall(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+size(V1)+1;
    valis ("#(showPc(Pc0,Sps))Call $(V0), #(showLocals(V1))",Pc2+1)
  }
  showIns(.iOCall(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+size(V1)+1;
    valis ("#(showPc(Pc0,Sps))OCall #(V0), #(showLocals(V1))",Pc2+1)
  }
  showIns(.iEscape(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+size(V1)+1;
    valis ("#(showPc(Pc0,Sps))Escape #(V0), #(showLocals(V1))",Pc2+1)
  }
  showIns(.iTCall(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+size(V1)+1;
    valis ("#(showPc(Pc0,Sps))TCall $(V0), #(showLocals(V1))",Pc2+1)
  }
  showIns(.iTOCall(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+size(V1)+1;
    valis ("#(showPc(Pc0,Sps))TOCall #(V0), #(showLocals(V1))",Pc2+1)
  }
  showIns(.iRSP(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))RSP #(V0)",Pc1+1)
  }
  showIns(.iRSX(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))RSX #(V0), #(V1)",Pc2+1)
  }
  showIns(.iEntry(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Entry $(V0), $(V1)",Pc2+1)
  }
  showIns(.iRtn, Pc0, Sps) => valof{
    valis ("#(showPc(Pc0,Sps))Rtn",Pc0+1)
  }
  showIns(.iRet(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))Ret #(V0)",Pc1+1)
  }
  showIns(.iXRet(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))XRet #(V0)",Pc1+1)
  }
  showIns(.iLoop(V0), Pc0, Sps) => valof{
    (InsTxt,Pc1) = showBlock(V0,Pc0,Sps+2);
    valis ("#(showPc(Pc0,Sps))Loop #(InsTxt)",Pc1+1)
  }
  showIns(.iBlock(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+size(V0)+1;
    (InsTxt,Pc2) = showBlock(V1,Pc1,Sps+2);
    valis ("#(showPc(Pc0,Sps))Block #(showLocals(V0)), #(InsTxt)",Pc2+1)
  }
  showIns(.iBreak(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))Break #(V0)",Pc1+1)
  }
  showIns(.iResult(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+size(V1)+1;
    valis ("#(showPc(Pc0,Sps))Result #(V0), #(showLocals(V1))",Pc2+1)
  }
  showIns(.iCont(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))Cont #(V0)",Pc1+1)
  }
  showIns(.iIf(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))If #(V0), #(V1)",Pc2+1)
  }
  showIns(.iIfNot(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))IfNot #(V0), #(V1)",Pc2+1)
  }
  showIns(.iICase(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    (InsTxt,Pc2) = showBlock(V1,Pc1,Sps+2);
    valis ("#(showPc(Pc0,Sps))ICase #(V0), #(InsTxt)",Pc2+1)
  }
  showIns(.iCase(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    (InsTxt,Pc2) = showBlock(V1,Pc1,Sps+2);
    valis ("#(showPc(Pc0,Sps))Case #(V0), #(InsTxt)",Pc2+1)
  }
  showIns(.iIxCase(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    (InsTxt,Pc2) = showBlock(V1,Pc1,Sps+2);
    valis ("#(showPc(Pc0,Sps))IxCase #(V0), #(InsTxt)",Pc2+1)
  }
  showIns(.iCLbl(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))CLbl $(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCInt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))CInt $(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCChar(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))CChar $(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCFlt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))CFlt $(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCLit(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))CLit $(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iMC(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))MC #(V0), $(V1)",Pc2+1)
  }
  showIns(.iMv(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Mv #(V0), #(V1)",Pc2+1)
  }
  showIns(.iLG(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))LG #(V0)",Pc1+1)
  }
  showIns(.iSG(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))SG #(V0), #(V1)",Pc2+1)
  }
  showIns(.iSav(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))Sav #(V0)",Pc1+1)
  }
  showIns(.iLdSav(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))LdSav #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iTstSav(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))TstSav #(V0), #(V1)",Pc2+1)
  }
  showIns(.iStSav(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))StSav #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCell(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Cell #(V0), #(V1)",Pc2+1)
  }
  showIns(.iGet(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Get #(V0), #(V1)",Pc2+1)
  }
  showIns(.iAssign(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Assign #(V0), #(V1)",Pc2+1)
  }
  showIns(.iNth(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))Nth #(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iStNth(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))StNth #(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iIAdd(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))IAdd #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iISub(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))ISub #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iIMul(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))IMul #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iIDiv(V0, V1, V2, V3), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    Pc4 = Pc3+1;
    valis ("#(showPc(Pc0,Sps))IDiv #(V0), #(V1), #(V2), #(V3)",Pc4+1)
  }
  showIns(.iIMod(V0, V1, V2, V3), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    Pc4 = Pc3+1;
    valis ("#(showPc(Pc0,Sps))IMod #(V0), #(V1), #(V2), #(V3)",Pc4+1)
  }
  showIns(.iIAbs(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))IAbs #(V0), #(V1)",Pc2+1)
  }
  showIns(.iIEq(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))IEq #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iILt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))ILt #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iIGe(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))IGe #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCEq(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))CEq #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCLt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))CLt #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCGe(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))CGe #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBAnd(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))BAnd #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBOr(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))BOr #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBXor(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))BXor #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBLsl(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))BLsl #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBLsr(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))BLsr #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBAsr(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))BAsr #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBNot(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))BNot #(V0), #(V1)",Pc2+1)
  }
  showIns(.iFAdd(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))FAdd #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFSub(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))FSub #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFMul(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))FMul #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFDiv(V0, V1, V2, V3), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    Pc4 = Pc3+1;
    valis ("#(showPc(Pc0,Sps))FDiv #(V0), #(V1), #(V2), #(V3)",Pc4+1)
  }
  showIns(.iFMod(V0, V1, V2, V3), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    Pc4 = Pc3+1;
    valis ("#(showPc(Pc0,Sps))FMod #(V0), #(V1), #(V2), #(V3)",Pc4+1)
  }
  showIns(.iFAbs(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))FAbs #(V0), #(V1)",Pc2+1)
  }
  showIns(.iFEq(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))FEq #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFLt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))FLt #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFGe(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))FGe #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iAlloc(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+size(V2)+1;
    valis ("#(showPc(Pc0,Sps))Alloc $(V0), #(V1), #(showLocals(V2))",Pc3+1)
  }
  showIns(.iClosure(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("#(showPc(Pc0,Sps))Closure $(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBump(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))Bump #(V0)",Pc1+1)
  }
  showIns(.iDrop(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))Drop #(V0)",Pc1+1)
  }
  showIns(.iFiber(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Fiber #(V0), #(V1)",Pc2+1)
  }
  showIns(.iSuspend(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Suspend #(V0), #(V1)",Pc2+1)
  }
  showIns(.iResume(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Resume #(V0), #(V1)",Pc2+1)
  }
  showIns(.iRetire(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Retire #(V0), #(V1)",Pc2+1)
  }
  showIns(.iUnderflow, Pc0, Sps) => valof{
    valis ("#(showPc(Pc0,Sps))Underflow",Pc0+1)
  }
  showIns(.iLine(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))Line $(V0)",Pc1+1)
  }
  showIns(.iBind(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("#(showPc(Pc0,Sps))Bind $(V0), #(V1)",Pc2+1)
  }
  showIns(.iDBug(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("#(showPc(Pc0,Sps))DBug $(V0)",Pc1+1)
  }


  showPc:(integer, integer) => string.
  showPc(Pc,Sps) => "$(Pc):    9;\:#(spaces(Sps))".

  spaces:(integer)=>string.
  spaces(Ln) => let{.
    sp(0) => [].
    sp(N) => [` `,..sp(N-1)].
  .} in _implode(sp(Ln)).

  showLocals:(cons[string]) => string.
  showLocals(Lcls) => "(#(interleave(Lcls,", ")*))".

  bumpPc:(cons[integer]) => cons[integer].
  bumpPc([Pc,..Rest]) => [Pc+1,..Rest].

  findEntryInstruction(Ins) => (Rslt ?= hasEntryInstruction(Ins) ?? Rslt || unreachable).

  public validateCode:(codeSegment){}.
  validateCode(Code){
    case Code in {
    | .func(Nm,_,Tp,Lcs,Ins) do {
        try{
          if (A,L) ?= hasEntryInstruction(Ins) then{
            validBlock(Ins, foldLeft(((N,Ls)=>Ls[N->.inited]),{ N->.notInited | N in L}, A),[])
        } else{
            validBlock(Ins, [], [])
          }
        } catch {
          .exception(Msg) do {
            reportTrap("Problem in code for $(Nm)\: #(Msg)")
         }
       }
     }
   | _ do {}
   }
  }

  varState ::= .inited | .notInited | .phiVar.

  implementation display[varState] => {
    disp(.inited) => "inited".
    disp(.notInited) => "not inited".
    disp(.phiVar) => "phi".
  }

  validBlock:(multi[insOp],map[varNm,varState],set[string])=>map[varNm,varState] throws exception.
  validBlock([],Lcls,Lbls) => Lcls.
  validBlock([I,..Ins],Lcls,Lbls) =>
    validBlock(Ins,validIns(I,Lcls,Lbls),Lbls).

  validIns:(insOp,map[varNm,varState],set[string])=>map[varNm,varState] throws exception.
  validIns(.iLbl(Lb,I),Lcls,Lbls) =>
    validIns(I,Lcls,Lbls\+Lb).
  validIns(.iHalt(V0), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Halt' not inited");
    valis Lcls0
  }
  validIns(.iAbort(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'Abort' not inited");
    valis Lcls0
  }
  validIns(.iCall(V0, V1), Lcls0, Lbls) => valof{
  if ~ {? Vv in V1 *> varInited(Lcls0,Vv) ?} then     throw .exception("Var $(V1) in 'Call' not inited");
    valis Lcls0
  }
  validIns(.iOCall(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'OCall' not inited");
  if ~ {? Vv in V1 *> varInited(Lcls0,Vv) ?} then     throw .exception("Var $(V1) in 'OCall' not inited");
    valis Lcls0
  }
  validIns(.iEscape(V0, V1), Lcls0, Lbls) => valof{
 if ~isEscape(V0) then   throw .exception("Unknown escape #(V0)");
  if ~ {? Vv in V1 *> varInited(Lcls0,Vv) ?} then     throw .exception("Var $(V1) in 'Escape' not inited");
    valis Lcls0
  }
  validIns(.iTCall(V0, V1), Lcls0, Lbls) => valof{
  if ~ {? Vv in V1 *> varInited(Lcls0,Vv) ?} then     throw .exception("Var $(V1) in 'TCall' not inited");
    valis Lcls0
  }
  validIns(.iTOCall(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'TOCall' not inited");
  if ~ {? Vv in V1 *> varInited(Lcls0,Vv) ?} then     throw .exception("Var $(V1) in 'TOCall' not inited");
    valis Lcls0
  }
  validIns(.iRSP(V0), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'RSP' already inited");
   Lcls1 = markInited(Lcls0,V0);
    valis Lcls1
  }
  validIns(.iRSX(V0, V1), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
   if varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'RSX' already inited");
   Lcls1 = markInited(Lcls0,V1);
    valis Lcls1
  }
  validIns(.iEntry(V0, V1), Lcls0, Lbls) => valof{
    valis Lcls0
  }
  validIns(.iRtn, Lcls0, Lbls) => valof{
    valis Lcls0
  }
  validIns(.iRet(V0), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Ret' not inited");
    valis Lcls0
  }
  validIns(.iXRet(V0), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'XRet' not inited");
    valis Lcls0
  }
  validIns(.iLoop(V0), Lcls0, Lbls) => valof{
    validBlock(V0,Lcls0,Lbls);
    valis Lcls0
  }
  validIns(.iBlock(V0, V1), Lcls0, Lbls) => valof{
   Lcls1 = foldRight(((V,Ls)=>Ls[V->.phiVar]),Lcls0,V0);
   Lcls2 = foldRight(((V,Ls)=>Ls[V->.inited]),Lcls0,V0);
    validBlock(V1,Lcls1,Lbls);
    valis Lcls2
  }
  validIns(.iBreak(V0), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
    valis Lcls0
  }
  validIns(.iResult(V0, V1), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
  if ~ {? Vv in V1 *> varInited(Lcls0,Vv) ?} then     throw .exception("Var $(V1) in 'Result' not inited");
    valis Lcls0
  }
  validIns(.iCont(V0), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
    valis Lcls0
  }
  validIns(.iIf(V0, V1), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
   if ~varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'If' not inited");
    valis Lcls0
  }
  validIns(.iIfNot(V0, V1), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
   if ~varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'IfNot' not inited");
    valis Lcls0
  }
  validIns(.iICase(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'ICase' not inited");
    validBlock(V1,Lcls0,Lbls);
    valis Lcls0
  }
  validIns(.iCase(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Case' not inited");
    validBlock(V1,Lcls0,Lbls);
    valis Lcls0
  }
  validIns(.iIxCase(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'IxCase' not inited");
    validBlock(V1,Lcls0,Lbls);
    valis Lcls0
  }
  validIns(.iCLbl(V0, V1, V2), Lcls0, Lbls) => valof{
 if ~ V1 .<. Lbls then
   throw .exception("Label #(V1) not in scope");
   if ~varInited(Lcls0, V2) then
     throw .exception("Var #(V2) in 'CLbl' not inited");
    valis Lcls0
  }
  validIns(.iCInt(V0, V1, V2), Lcls0, Lbls) => valof{
 if ~ V1 .<. Lbls then
   throw .exception("Label #(V1) not in scope");
   if ~varInited(Lcls0, V2) then
     throw .exception("Var #(V2) in 'CInt' not inited");
    valis Lcls0
  }
  validIns(.iCChar(V0, V1, V2), Lcls0, Lbls) => valof{
 if ~ V1 .<. Lbls then
   throw .exception("Label #(V1) not in scope");
   if ~varInited(Lcls0, V2) then
     throw .exception("Var #(V2) in 'CChar' not inited");
    valis Lcls0
  }
  validIns(.iCFlt(V0, V1, V2), Lcls0, Lbls) => valof{
 if ~ V1 .<. Lbls then
   throw .exception("Label #(V1) not in scope");
   if ~varInited(Lcls0, V2) then
     throw .exception("Var #(V2) in 'CFlt' not inited");
    valis Lcls0
  }
  validIns(.iCLit(V0, V1, V2), Lcls0, Lbls) => valof{
 if ~ V1 .<. Lbls then
   throw .exception("Label #(V1) not in scope");
   if ~varInited(Lcls0, V2) then
     throw .exception("Var #(V2) in 'CLit' not inited");
    valis Lcls0
  }
  validIns(.iMC(V0, V1), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'MC' already inited");
   Lcls1 = markInited(Lcls0,V0);
    valis Lcls1
  }
  validIns(.iMv(V0, V1), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Mv' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'Mv' not inited");
    valis Lcls1
  }
  validIns(.iLG(V0), Lcls0, Lbls) => valof{
    valis Lcls0
  }
  validIns(.iSG(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'SG' not inited");
    valis Lcls0
  }
  validIns(.iSav(V0), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Sav' already inited");
   Lcls1 = markInited(Lcls0,V0);
    valis Lcls1
  }
  validIns(.iLdSav(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'LdSav' already inited");
   Lcls1 = markInited(Lcls0,V0);
 if ~ V1 .<. Lbls then
   throw .exception("Label #(V1) not in scope");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'LdSav' not inited");
    valis Lcls1
  }
  validIns(.iTstSav(V0, V1), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'TstSav' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'TstSav' not inited");
    valis Lcls1
  }
  validIns(.iStSav(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'StSav' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'StSav' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'StSav' not inited");
    valis Lcls1
  }
  validIns(.iCell(V0, V1), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Cell' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'Cell' not inited");
    valis Lcls1
  }
  validIns(.iGet(V0, V1), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Get' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'Get' not inited");
    valis Lcls1
  }
  validIns(.iAssign(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Assign' not inited");
   if ~varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'Assign' not inited");
    valis Lcls0
  }
  validIns(.iNth(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Nth' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'Nth' not inited");
    valis Lcls1
  }
  validIns(.iStNth(V0, V1, V2), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'StNth' not inited");
   if ~varInited(Lcls0, V2) then
     throw .exception("Var #(V2) in 'StNth' not inited");
    valis Lcls0
  }
  validIns(.iIAdd(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'IAdd' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'IAdd' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'IAdd' not inited");
    valis Lcls1
  }
  validIns(.iISub(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'ISub' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'ISub' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'ISub' not inited");
    valis Lcls1
  }
  validIns(.iIMul(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'IMul' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'IMul' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'IMul' not inited");
    valis Lcls1
  }
  validIns(.iIDiv(V0, V1, V2, V3), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
   if varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'IDiv' already inited");
   Lcls1 = markInited(Lcls0,V1);
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'IDiv' not inited");
   if ~varInited(Lcls1, V3) then
     throw .exception("Var #(V3) in 'IDiv' not inited");
    valis Lcls1
  }
  validIns(.iIMod(V0, V1, V2, V3), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
   if varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'IMod' already inited");
   Lcls1 = markInited(Lcls0,V1);
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'IMod' not inited");
   if ~varInited(Lcls1, V3) then
     throw .exception("Var #(V3) in 'IMod' not inited");
    valis Lcls1
  }
  validIns(.iIAbs(V0, V1), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'IAbs' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'IAbs' not inited");
    valis Lcls1
  }
  validIns(.iIEq(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'IEq' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'IEq' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'IEq' not inited");
    valis Lcls1
  }
  validIns(.iILt(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'ILt' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'ILt' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'ILt' not inited");
    valis Lcls1
  }
  validIns(.iIGe(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'IGe' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'IGe' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'IGe' not inited");
    valis Lcls1
  }
  validIns(.iCEq(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'CEq' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'CEq' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'CEq' not inited");
    valis Lcls1
  }
  validIns(.iCLt(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'CLt' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'CLt' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'CLt' not inited");
    valis Lcls1
  }
  validIns(.iCGe(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'CGe' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'CGe' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'CGe' not inited");
    valis Lcls1
  }
  validIns(.iBAnd(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'BAnd' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'BAnd' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'BAnd' not inited");
    valis Lcls1
  }
  validIns(.iBOr(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'BOr' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'BOr' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'BOr' not inited");
    valis Lcls1
  }
  validIns(.iBXor(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'BXor' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'BXor' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'BXor' not inited");
    valis Lcls1
  }
  validIns(.iBLsl(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'BLsl' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'BLsl' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'BLsl' not inited");
    valis Lcls1
  }
  validIns(.iBLsr(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'BLsr' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'BLsr' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'BLsr' not inited");
    valis Lcls1
  }
  validIns(.iBAsr(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'BAsr' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'BAsr' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'BAsr' not inited");
    valis Lcls1
  }
  validIns(.iBNot(V0, V1), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'BNot' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'BNot' not inited");
    valis Lcls1
  }
  validIns(.iFAdd(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'FAdd' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'FAdd' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'FAdd' not inited");
    valis Lcls1
  }
  validIns(.iFSub(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'FSub' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'FSub' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'FSub' not inited");
    valis Lcls1
  }
  validIns(.iFMul(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'FMul' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'FMul' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'FMul' not inited");
    valis Lcls1
  }
  validIns(.iFDiv(V0, V1, V2, V3), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
   if varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'FDiv' already inited");
   Lcls1 = markInited(Lcls0,V1);
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'FDiv' not inited");
   if ~varInited(Lcls1, V3) then
     throw .exception("Var #(V3) in 'FDiv' not inited");
    valis Lcls1
  }
  validIns(.iFMod(V0, V1, V2, V3), Lcls0, Lbls) => valof{
 if ~ V0 .<. Lbls then
   throw .exception("Label #(V0) not in scope");
   if varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'FMod' already inited");
   Lcls1 = markInited(Lcls0,V1);
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'FMod' not inited");
   if ~varInited(Lcls1, V3) then
     throw .exception("Var #(V3) in 'FMod' not inited");
    valis Lcls1
  }
  validIns(.iFAbs(V0, V1), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'FAbs' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'FAbs' not inited");
    valis Lcls1
  }
  validIns(.iFEq(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'FEq' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'FEq' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'FEq' not inited");
    valis Lcls1
  }
  validIns(.iFLt(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'FLt' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'FLt' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'FLt' not inited");
    valis Lcls1
  }
  validIns(.iFGe(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'FGe' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'FGe' not inited");
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'FGe' not inited");
    valis Lcls1
  }
  validIns(.iAlloc(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'Alloc' already inited");
   Lcls1 = markInited(Lcls0,V1);
  if ~ {? Vv in V2 *> varInited(Lcls1,Vv) ?} then     throw .exception("Var $(V2) in 'Alloc' not inited");
    valis Lcls1
  }
  validIns(.iClosure(V0, V1, V2), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'Closure' already inited");
   Lcls1 = markInited(Lcls0,V1);
   if ~varInited(Lcls1, V2) then
     throw .exception("Var #(V2) in 'Closure' not inited");
    valis Lcls1
  }
  validIns(.iBump(V0), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Bump' not inited");
    valis Lcls0
  }
  validIns(.iDrop(V0), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Drop' not inited");
    valis Lcls0
  }
  validIns(.iFiber(V0, V1), Lcls0, Lbls) => valof{
   if varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Fiber' already inited");
   Lcls1 = markInited(Lcls0,V0);
   if ~varInited(Lcls1, V1) then
     throw .exception("Var #(V1) in 'Fiber' not inited");
    valis Lcls1
  }
  validIns(.iSuspend(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Suspend' not inited");
   if ~varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'Suspend' not inited");
    valis Lcls0
  }
  validIns(.iResume(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Resume' not inited");
   if ~varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'Resume' not inited");
    valis Lcls0
  }
  validIns(.iRetire(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V0) then
     throw .exception("Var #(V0) in 'Retire' not inited");
   if ~varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'Retire' not inited");
    valis Lcls0
  }
  validIns(.iUnderflow, Lcls0, Lbls) => valof{
    valis Lcls0
  }
  validIns(.iLine(V0), Lcls0, Lbls) => valof{
    valis Lcls0
  }
  validIns(.iBind(V0, V1), Lcls0, Lbls) => valof{
   if ~varInited(Lcls0, V1) then
     throw .exception("Var #(V1) in 'Bind' not inited");
    valis Lcls0
  }
  validIns(.iDBug(V0), Lcls0, Lbls) => valof{
    valis Lcls0
  }


  varInited(Lcls,Vn) => .inited ?= Lcls[Vn].

  markInited(Lcls,Vn) => Lcls[Vn->.inited].

  hasEntryInstruction([.iEntry(A,L),.._]) => .some((A,L)).
  hasEntryInstruction([_,..Ins]) => hasEntryInstruction(Ins).
  hasEntryInstruction([]) => .none.

  public opcodeHash = 1457260803800190732.
}
