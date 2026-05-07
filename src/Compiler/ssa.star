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
  declareArgs(Lst,Map) => let{.
    declare([],_,Map) => Map.
    declare([Nm,..Vrs],Ax,Map) =>
      declare(Vrs,Ax+1,Map[Nm->Ax]).
  .} in declare(Lst,0,Map).          -- First arg is 0

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
  showMap(Msg,Map) => "#(Msg)\: #((Map//((Nm,_))=>Nm++"\n")*)".

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
    valis (interleave(Blocks,"\n")*,Pcx)
  }

  showCode:(multi[insOp],integer,integer) => (cons[string],integer).
  showCode([],Pc,_) => ([],Pc).
  showCode([Ins,..Cde],Pc,Sps) => valof{
    (Itxt,Pc1) = showIns(Ins,Pc,Sps);
    (Rest,Pcx) = showCode(Cde,Pc1,Sps);
    valis ([spaces(Sps),Itxt,..Rest],Pcx)
  }

  showIns:(insOp,integer,integer) => (string,integer).
  showIns(.iLbl(Lb,I),Pc,Sps) => valof{
    (Text,Pcx) = showIns(I,Pc,Sps);
    valis ("#(Lb): #(Text)",Pcx)
  }
  showIns(.iHalt(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("Halt #(V0)",Pc1+1)
  }
  showIns(.iAbort(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Abort $(V0), #(V1)",Pc2+1)
  }
  showIns(.iCall(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Call $(V0), $(V1)",Pc2+1)
  }
  showIns(.iOCall(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("OCall #(V0), $(V1)",Pc2+1)
  }
  showIns(.iEscape(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Escape #(V0), $(V1)",Pc2+1)
  }
  showIns(.iTCall(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("TCall $(V0), $(V1)",Pc2+1)
  }
  showIns(.iTOCall(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("TOCall #(V0), $(V1)",Pc2+1)
  }
  showIns(.iRSP(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("RSP #(V0)",Pc1+1)
  }
  showIns(.iRSX(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("RSX $(V0), #(V1)",Pc2+1)
  }
  showIns(.iEntry(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Entry $(V0), $(V1)",Pc2+1)
  }
  showIns(.iRtn, Pc0, Sps) => valof{
    valis ("Rtn",Pc0+1)
  }
  showIns(.iRet(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("Ret #(V0)",Pc1+1)
  }
  showIns(.iXRet(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("XRet #(V0)",Pc1+1)
  }
  showIns(.iLoop(V0), Pc0, Sps) => valof{
    (InsTxt,Pc1) = showBlock(V0,Pc0,Sps+2);
    valis ("Loop #(InsTxt)",Pc1+1)
  }
  showIns(.iBlock(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    (InsTxt,Pc2) = showBlock(V1,Pc1,Sps+2);
    valis ("Block $(V0), #(InsTxt)",Pc2+1)
  }
  showIns(.iBreak(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("Break $(V0)",Pc1+1)
  }
  showIns(.iResult(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Result $(V0), $(V1)",Pc2+1)
  }
  showIns(.iCont(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("Cont $(V0)",Pc1+1)
  }
  showIns(.iIf(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("If $(V0), #(V1)",Pc2+1)
  }
  showIns(.iIfNot(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("IfNot $(V0), #(V1)",Pc2+1)
  }
  showIns(.iICase(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    (InsTxt,Pc2) = showBlock(V1,Pc1,Sps+2);
    valis ("ICase #(V0), #(InsTxt)",Pc2+1)
  }
  showIns(.iCase(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    (InsTxt,Pc2) = showBlock(V1,Pc1,Sps+2);
    valis ("Case #(V0), #(InsTxt)",Pc2+1)
  }
  showIns(.iIxCase(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    (InsTxt,Pc2) = showBlock(V1,Pc1,Sps+2);
    valis ("IxCase #(V0), #(InsTxt)",Pc2+1)
  }
  showIns(.iCLbl(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("CLbl $(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iCInt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("CInt $(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iCChar(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("CChar $(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iCFlt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("CFlt $(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iCLit(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("CLit $(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iMC(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("MC #(V0), $(V1)",Pc2+1)
  }
  showIns(.iMv(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Mv #(V0), #(V1)",Pc2+1)
  }
  showIns(.iLG(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("LG #(V0)",Pc1+1)
  }
  showIns(.iSG(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("SG #(V0), #(V1)",Pc2+1)
  }
  showIns(.iSav(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("Sav #(V0)",Pc1+1)
  }
  showIns(.iLdSav(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("LdSav #(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iTstSav(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("TstSav #(V0), #(V1)",Pc2+1)
  }
  showIns(.iStSav(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("StSav #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCell(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Cell #(V0), #(V1)",Pc2+1)
  }
  showIns(.iGet(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Get #(V0), #(V1)",Pc2+1)
  }
  showIns(.iAssign(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Assign #(V0), #(V1)",Pc2+1)
  }
  showIns(.iNth(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("Nth #(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iStNth(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("StNth #(V0), $(V1), #(V2)",Pc3+1)
  }
  showIns(.iIAdd(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("IAdd #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iISub(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("ISub #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iIMul(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("IMul #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iIDiv(V0, V1, V2, V3), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    Pc4 = Pc3+1;
    valis ("IDiv $(V0), #(V1), #(V2), #(V3)",Pc4+1)
  }
  showIns(.iIMod(V0, V1, V2, V3), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    Pc4 = Pc3+1;
    valis ("IMod $(V0), #(V1), #(V2), #(V3)",Pc4+1)
  }
  showIns(.iIAbs(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("IAbs #(V0), #(V1)",Pc2+1)
  }
  showIns(.iIEq(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("IEq #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iILt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("ILt #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iIGe(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("IGe #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCEq(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("CEq #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCLt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("CLt #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iCGe(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("CGe #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBAnd(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("BAnd #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBOr(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("BOr #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBXor(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("BXor #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBLsl(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("BLsl #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBLsr(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("BLsr #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBAsr(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("BAsr #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBNot(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("BNot #(V0), #(V1)",Pc2+1)
  }
  showIns(.iFAdd(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("FAdd #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFSub(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("FSub #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFMul(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("FMul #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFDiv(V0, V1, V2, V3), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    Pc4 = Pc3+1;
    valis ("FDiv $(V0), #(V1), #(V2), #(V3)",Pc4+1)
  }
  showIns(.iFMod(V0, V1, V2, V3), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    Pc4 = Pc3+1;
    valis ("FMod $(V0), #(V1), #(V2), #(V3)",Pc4+1)
  }
  showIns(.iFAbs(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("FAbs #(V0), #(V1)",Pc2+1)
  }
  showIns(.iFEq(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("FEq #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFLt(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("FLt #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iFGe(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("FGe #(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iAlloc(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("Alloc $(V0), #(V1), $(V2)",Pc3+1)
  }
  showIns(.iClosure(V0, V1, V2), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    Pc3 = Pc2+1;
    valis ("Closure $(V0), #(V1), #(V2)",Pc3+1)
  }
  showIns(.iBump(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("Bump #(V0)",Pc1+1)
  }
  showIns(.iDrop(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("Drop #(V0)",Pc1+1)
  }
  showIns(.iFiber(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Fiber #(V0), #(V1)",Pc2+1)
  }
  showIns(.iSuspend(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Suspend #(V0), #(V1)",Pc2+1)
  }
  showIns(.iResume(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Resume #(V0), #(V1)",Pc2+1)
  }
  showIns(.iRetire(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Retire #(V0), #(V1)",Pc2+1)
  }
  showIns(.iUnderflow, Pc0, Sps) => valof{
    valis ("Underflow",Pc0+1)
  }
  showIns(.iLine(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("Line $(V0)",Pc1+1)
  }
  showIns(.iBind(V0, V1), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    Pc2 = Pc1+1;
    valis ("Bind $(V0), #(V1)",Pc2+1)
  }
  showIns(.iDBug(V0), Pc0, Sps) => valof{
    Pc1 = Pc0+1;
    valis ("DBug $(V0)",Pc1+1)
  }


  showPc:(integer, integer) => string.
  showPc(Pc,Sps) => "$(Pc)\:#(spaces(Sps))".

  spaces:(integer)=>string.
  spaces(Ln) => let{.
    sp(0) => [].
    sp(N) => [` `,..sp(N-1)].
  .} in _implode(sp(Ln)).

  bumpPc:(cons[integer]) => cons[integer].
  bumpPc([Pc,..Rest]) => [Pc+1,..Rest].

  findEntryInstruction([.iEntry(A,L),.._]) => (A,L).
  findEntryInstruction([_,..Ins]) => findEntryInstruction(Ins).

  public opcodeHash = 211094525106623888.
}
