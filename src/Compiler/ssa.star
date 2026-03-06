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

  public codeSegment ::= .func(termLbl,codePolicy,ltipe,cons[(string,data)],cons[(string,data)],multi[insOp]) |
    .struct(termLbl,tipe,integer) |
    .tipe(tipe,typeRule,map[termLbl,integer]).

  public insOp ::=
    | .iHalt(varNm)
    | .iAbort(data, varNm)
    | .iCall(termLbl, cons[varNm])
    | .iOCall(integer, varNm, cons[varNm])
    | .iEscape(string, cons[varNm])
    | .iXCall(termLbl, assemLbl, cons[varNm])
    | .iXOCall(integer, assemLbl, varNm, cons[varNm])
    | .iXEscape(string, assemLbl, cons[varNm])
    | .iTCall(termLbl, cons[varNm])
    | .iTOCall(integer, varNm, cons[varNm])
    | .iXEntry(cons[varNm])
    | .iEntry(cons[varNm])
    | .iRet(varNm)
    | .iXRet(varNm)
    | .iBlock(multi[insOp])
    | .iValof(varNm, multi[insOp])
    | .iBreak(assemLbl)
    | .iResult(assemLbl, varNm)
    | .iLoop(assemLbl)
    | .iIf(assemLbl, varNm)
    | .iIfNot(assemLbl, varNm)
    | .iCLbl(termLbl, assemLbl, varNm)
    | .iCInt(data, assemLbl, varNm)
    | .iCChar(data, assemLbl, varNm)
    | .iCFlt(data, assemLbl, varNm)
    | .iCLit(data, assemLbl, varNm)
    | .iICase(varNm, multi[insOp])
    | .iCase(varNm, multi[insOp])
    | .iIxCase(varNm, multi[insOp])
    | .iMvV(varNm)
    | .iMvC(varNm, data)
    | .iMv(varNm, varNm)
    | .iMvR(varNm)
    | .iMvG(varNm, string)
    | .iStG(string, varNm)
    | .iSav(varNm)
    | .iLdSav(varNm, assemLbl, varNm)
    | .iTstSav(varNm, varNm)
    | .iStSav(varNm, varNm)
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
    | .iAlloc(varNm, termLbl, cons[varNm])
    | .iClosure(varNm, termLbl, varNm)
    | .iFiber(varNm, varNm)
    | .iSuspend(varNm, varNm, varNm)
    | .iResume(varNm, varNm, varNm)
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
    | .func(Nm,H,Sig,Ags,Lcs,Ins) => valof{
      funSig = .strg(Sig::string);
      (Lt0,_) = findLit([],.symb(Nm));
      (Lt1,tpIx) = findLit(Lt0,funSig);
      LclMap = declareArgs(Ags,declareLocals(Lcs,{}));
      (Code,_,Lts) = assemBlock(Ins,[],0,[],Lt1,LclMap);
       valis mkCons("code",
              [.symb(Nm),encPolicy(H),.intgr(tpIx),
              .intgr(size(Lcs)),litTbl(Lts),mkTpl(Code::cons[data]),
    	  mkTpl(sortVInfo(varInfos(Ags,LclMap)++varInfos(Lcs,LclMap)))])
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

  declareLocals:(cons[(string,data)],map[string,integer]) => map[string,integer].
  declareLocals(Lcs,Map) => let{.
    decl([],_,Mp) => Mp.
    decl([(Vr,_),..Ls],Ix,Mp) => decl(Ls,Ix-1,Mp[Vr -> Ix]).
  .} in decl(Lcs,-1,Map).             -- First local is -1

  declareArgs:(cons[(string,data)],map[string,integer]) => map[string,integer].
  declareArgs(Lcs,Map) => let{.
    decl([],_,Mp) => Mp.
    decl([(Vr,_),..Ls],Ix,Mp) => decl(Ls,Ix+1,Mp[Vr -> Ix]).
  .} in decl(Lcs,0,Map).             -- First arg is 0

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
  mnem(.iOCall(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(3),.intgr(V0),findLocal(V1,Lcs),mkTpl(findLocals(V2,Lcs))],Pc+4,Lt0).
  mnem(.iEscape(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(4),.strg(V0),mkTpl(findLocals(V1,Lcs))],Pc+3,Lt0).
  mnem(.iXCall(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V0)); 
    valis ([.intgr(5),.intgr(L1),.intgr(findLevel(Lbls,V1)),mkTpl(findLocals(V2,Lcs))],Pc+4,Lt1);
  }
  mnem(.iXOCall(V0, V1, V2, V3), Pc,Lbls,Lt0,Lcs) => ([.intgr(6),.intgr(V0),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs),mkTpl(findLocals(V3,Lcs))],Pc+5,Lt0).
  mnem(.iXEscape(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(7),.strg(V0),.intgr(findLevel(Lbls,V1)),mkTpl(findLocals(V2,Lcs))],Pc+4,Lt0).
  mnem(.iTCall(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V0)); 
    valis ([.intgr(8),.intgr(L1),mkTpl(findLocals(V1,Lcs))],Pc+3,Lt1);
  }
  mnem(.iTOCall(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(9),.intgr(V0),findLocal(V1,Lcs),mkTpl(findLocals(V2,Lcs))],Pc+4,Lt0).
  mnem(.iXEntry(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(10),mkTpl(findLocals(V0,Lcs))],Pc+2,Lt0).
  mnem(.iEntry(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(11),mkTpl(findLocals(V0,Lcs))],Pc+2,Lt0).
  mnem(.iRet(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(12),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iXRet(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(13),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iBlock(V0), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V0,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(14),mkTpl(A1::cons[data])],Pc+2,Lt1);
  }
  mnem(.iValof(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V1,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(15),findLocal(V0,Lcs),mkTpl(A1::cons[data])],Pc+3,Lt1);
  }
  mnem(.iBreak(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(16),.intgr(findLevel(Lbls,V0))],Pc+2,Lt0).
  mnem(.iResult(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(17),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iLoop(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(18),.intgr(findLevel(Lbls,V0))],Pc+2,Lt0).
  mnem(.iIf(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(19),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iIfNot(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(20),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iCLbl(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V0)); 
    valis ([.intgr(21),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iCInt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(22),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iCChar(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(23),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iCFlt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(24),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iCLit(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(25),.intgr(L1),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iICase(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V1,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(26),findLocal(V0,Lcs),mkTpl(A1::cons[data])],Pc+3,Lt1);
  }
  mnem(.iCase(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V1,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(27),findLocal(V0,Lcs),mkTpl(A1::cons[data])],Pc+3,Lt1);
  }
  mnem(.iIxCase(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (A1, _, Lt1) = assemBlock(V1,[],Pc+1,[.none,..Lbls],Lt0,Lcs); 
    valis ([.intgr(28),findLocal(V0,Lcs),mkTpl(A1::cons[data])],Pc+3,Lt1);
  }
  mnem(.iMvV(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(29),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iMvC(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V1); 
    valis ([.intgr(30),findLocal(V0,Lcs),.intgr(L1)],Pc+3,Lt1);
  }
  mnem(.iMv(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(31),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iMvR(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(32),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iMvG(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(33),findLocal(V0,Lcs),.strg(V1)],Pc+3,Lt0).
  mnem(.iStG(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(34),.strg(V0),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iSav(V0), Pc,Lbls,Lt0,Lcs) => ([.intgr(35),findLocal(V0,Lcs)],Pc+2,Lt0).
  mnem(.iLdSav(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(36),findLocal(V0,Lcs),.intgr(findLevel(Lbls,V1)),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iTstSav(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(37),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iStSav(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(38),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iCell(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(39),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iGet(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(40),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iAssign(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(41),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iNth(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(42),findLocal(V0,Lcs),.intgr(V1),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iStNth(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(43),findLocal(V0,Lcs),.intgr(V1),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iIAdd(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(44),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iISub(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(45),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iIMul(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(46),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iIDiv(V0, V1, V2, V3), Pc,Lbls,Lt0,Lcs) => ([.intgr(47),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs),findLocal(V2,Lcs),findLocal(V3,Lcs)],Pc+5,Lt0).
  mnem(.iIMod(V0, V1, V2, V3), Pc,Lbls,Lt0,Lcs) => ([.intgr(48),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs),findLocal(V2,Lcs),findLocal(V3,Lcs)],Pc+5,Lt0).
  mnem(.iIAbs(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(49),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iIEq(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(50),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iILt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(51),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iIGe(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(52),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iCEq(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(53),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iCLt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(54),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iCGe(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(55),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBAnd(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(56),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBOr(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(57),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBXor(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(58),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBLsl(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(59),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBLsr(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(60),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBAsr(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(61),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iBNot(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(62),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iFAdd(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(63),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFSub(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(64),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFMul(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(65),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFDiv(V0, V1, V2, V3), Pc,Lbls,Lt0,Lcs) => ([.intgr(66),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs),findLocal(V2,Lcs),findLocal(V3,Lcs)],Pc+5,Lt0).
  mnem(.iFMod(V0, V1, V2, V3), Pc,Lbls,Lt0,Lcs) => ([.intgr(67),.intgr(findLevel(Lbls,V0)),findLocal(V1,Lcs),findLocal(V2,Lcs),findLocal(V3,Lcs)],Pc+5,Lt0).
  mnem(.iFAbs(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(68),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iFEq(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(69),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFLt(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(70),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iFGe(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(71),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iAlloc(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V1)); 
    valis ([.intgr(72),findLocal(V0,Lcs),.intgr(L1),mkTpl(findLocals(V2,Lcs))],Pc+4,Lt1);
  }
  mnem(.iClosure(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,.symb(V1)); 
    valis ([.intgr(73),findLocal(V0,Lcs),.intgr(L1),findLocal(V2,Lcs)],Pc+4,Lt1);
  }
  mnem(.iFiber(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(74),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iSuspend(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(75),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iResume(V0, V1, V2), Pc,Lbls,Lt0,Lcs) => ([.intgr(76),findLocal(V0,Lcs),findLocal(V1,Lcs),findLocal(V2,Lcs)],Pc+4,Lt0).
  mnem(.iRetire(V0, V1), Pc,Lbls,Lt0,Lcs) => ([.intgr(77),findLocal(V0,Lcs),findLocal(V1,Lcs)],Pc+3,Lt0).
  mnem(.iUnderflow, Pc,Lbls,Lt0,Lcs) => ([.intgr(78)],Pc+1,Lt0).
  mnem(.iLine(V0), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(79),.intgr(L1)],Pc+2,Lt1);
  }
  mnem(.iBind(V0, V1), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(80),.intgr(L1),findLocal(V1,Lcs)],Pc+3,Lt1);
  }
  mnem(.iDBug(V0), Pc,Lbls,Lt0,Lcs) => valof {
    (Lt1, L1) = findLit(Lt0,V0); 
    valis ([.intgr(81),.intgr(L1)],Pc+2,Lt1);
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
    disp(.func(Nm,_,Tp,Ags,Lcs,Ins)) => "fun $(Nm)\:$(Tp)\n"++showMap("args",Ags)++showMap("locals",Lcs)++showMnem(Ins).
    disp(.struct(Lbl,Tp,Ix)) => "struct $(Lbl)\:$(Tp) @ $(Ix)".
    disp(.tipe(_Tp,TpRl,Map)) => "type $(TpRl), map = $(Map)".
  }

  public implementation display[insOp] => {
    disp(Op) => showIns(Op,[]).
  }

  showMnem:(multi[insOp]) => string.
  showMnem(Ops) => showBlock(Ops,[0]).

  showBlock:(multi[insOp],cons[integer]) => string.
  showBlock(Ins,Pc) => interleave(showCode(Ins,[0,..Pc]),"\n")*.

  showCode:(multi[insOp],cons[integer]) => cons[string].
  showCode([],_) => [].
  showCode([Ins,..Cde],Pc) => ["#(showPc(Pc))\: #(showIns(Ins,Pc))",..showCode(Cde,bumpPc(Pc))].

  showIns:(insOp,cons[integer]) => string.
  showIns(.iLbl(Lb,I),Pc) => "#(Lb):  #(showIns(I,Pc))".
  showIns(.iHalt(V0), Pc) => "Halt #(V0)".
  showIns(.iAbort(V0, V1), Pc) => "Abort $(V0) #(V1)".
  showIns(.iCall(V0, V1), Pc) => "Call $(V0) $(V1)".
  showIns(.iOCall(V0, V1, V2), Pc) => "OCall $(V0) #(V1) $(V2)".
  showIns(.iEscape(V0, V1), Pc) => "Escape #(V0) $(V1)".
  showIns(.iXCall(V0, V1, V2), Pc) => "XCall $(V0) $(V1) $(V2)".
  showIns(.iXOCall(V0, V1, V2, V3), Pc) => "XOCall $(V0) $(V1) #(V2) $(V3)".
  showIns(.iXEscape(V0, V1, V2), Pc) => "XEscape #(V0) $(V1) $(V2)".
  showIns(.iTCall(V0, V1), Pc) => "TCall $(V0) $(V1)".
  showIns(.iTOCall(V0, V1, V2), Pc) => "TOCall $(V0) #(V1) $(V2)".
  showIns(.iXEntry(V0), Pc) => "XEntry $(V0)".
  showIns(.iEntry(V0), Pc) => "Entry $(V0)".
  showIns(.iRet(V0), Pc) => "Ret #(V0)".
  showIns(.iXRet(V0), Pc) => "XRet #(V0)".
  showIns(.iBlock(V0), Pc) => "Block #(showBlock(V0,[0,..Pc]))".
  showIns(.iValof(V0, V1), Pc) => "Valof #(V0) #(showBlock(V1,[0,..Pc]))".
  showIns(.iBreak(V0), Pc) => "Break $(V0)".
  showIns(.iResult(V0, V1), Pc) => "Result $(V0) #(V1)".
  showIns(.iLoop(V0), Pc) => "Loop $(V0)".
  showIns(.iIf(V0, V1), Pc) => "If $(V0) #(V1)".
  showIns(.iIfNot(V0, V1), Pc) => "IfNot $(V0) #(V1)".
  showIns(.iCLbl(V0, V1, V2), Pc) => "CLbl $(V0) $(V1) #(V2)".
  showIns(.iCInt(V0, V1, V2), Pc) => "CInt $(V0) $(V1) #(V2)".
  showIns(.iCChar(V0, V1, V2), Pc) => "CChar $(V0) $(V1) #(V2)".
  showIns(.iCFlt(V0, V1, V2), Pc) => "CFlt $(V0) $(V1) #(V2)".
  showIns(.iCLit(V0, V1, V2), Pc) => "CLit $(V0) $(V1) #(V2)".
  showIns(.iICase(V0, V1), Pc) => "ICase #(V0) #(showBlock(V1,[0,..Pc]))".
  showIns(.iCase(V0, V1), Pc) => "Case #(V0) #(showBlock(V1,[0,..Pc]))".
  showIns(.iIxCase(V0, V1), Pc) => "IxCase #(V0) #(showBlock(V1,[0,..Pc]))".
  showIns(.iMvV(V0), Pc) => "MvV #(V0)".
  showIns(.iMvC(V0, V1), Pc) => "MvC #(V0) $(V1)".
  showIns(.iMv(V0, V1), Pc) => "Mv #(V0) #(V1)".
  showIns(.iMvR(V0), Pc) => "MvR #(V0)".
  showIns(.iMvG(V0, V1), Pc) => "MvG #(V0) #(V1)".
  showIns(.iStG(V0, V1), Pc) => "StG #(V0) #(V1)".
  showIns(.iSav(V0), Pc) => "Sav #(V0)".
  showIns(.iLdSav(V0, V1, V2), Pc) => "LdSav #(V0) $(V1) #(V2)".
  showIns(.iTstSav(V0, V1), Pc) => "TstSav #(V0) #(V1)".
  showIns(.iStSav(V0, V1), Pc) => "StSav #(V0) #(V1)".
  showIns(.iCell(V0, V1), Pc) => "Cell #(V0) #(V1)".
  showIns(.iGet(V0, V1), Pc) => "Get #(V0) #(V1)".
  showIns(.iAssign(V0, V1), Pc) => "Assign #(V0) #(V1)".
  showIns(.iNth(V0, V1, V2), Pc) => "Nth #(V0) $(V1) #(V2)".
  showIns(.iStNth(V0, V1, V2), Pc) => "StNth #(V0) $(V1) #(V2)".
  showIns(.iIAdd(V0, V1, V2), Pc) => "IAdd #(V0) #(V1) #(V2)".
  showIns(.iISub(V0, V1, V2), Pc) => "ISub #(V0) #(V1) #(V2)".
  showIns(.iIMul(V0, V1, V2), Pc) => "IMul #(V0) #(V1) #(V2)".
  showIns(.iIDiv(V0, V1, V2, V3), Pc) => "IDiv $(V0) #(V1) #(V2) #(V3)".
  showIns(.iIMod(V0, V1, V2, V3), Pc) => "IMod $(V0) #(V1) #(V2) #(V3)".
  showIns(.iIAbs(V0, V1), Pc) => "IAbs #(V0) #(V1)".
  showIns(.iIEq(V0, V1, V2), Pc) => "IEq #(V0) #(V1) #(V2)".
  showIns(.iILt(V0, V1, V2), Pc) => "ILt #(V0) #(V1) #(V2)".
  showIns(.iIGe(V0, V1, V2), Pc) => "IGe #(V0) #(V1) #(V2)".
  showIns(.iCEq(V0, V1, V2), Pc) => "CEq #(V0) #(V1) #(V2)".
  showIns(.iCLt(V0, V1, V2), Pc) => "CLt #(V0) #(V1) #(V2)".
  showIns(.iCGe(V0, V1, V2), Pc) => "CGe #(V0) #(V1) #(V2)".
  showIns(.iBAnd(V0, V1, V2), Pc) => "BAnd #(V0) #(V1) #(V2)".
  showIns(.iBOr(V0, V1, V2), Pc) => "BOr #(V0) #(V1) #(V2)".
  showIns(.iBXor(V0, V1, V2), Pc) => "BXor #(V0) #(V1) #(V2)".
  showIns(.iBLsl(V0, V1, V2), Pc) => "BLsl #(V0) #(V1) #(V2)".
  showIns(.iBLsr(V0, V1, V2), Pc) => "BLsr #(V0) #(V1) #(V2)".
  showIns(.iBAsr(V0, V1, V2), Pc) => "BAsr #(V0) #(V1) #(V2)".
  showIns(.iBNot(V0, V1), Pc) => "BNot #(V0) #(V1)".
  showIns(.iFAdd(V0, V1, V2), Pc) => "FAdd #(V0) #(V1) #(V2)".
  showIns(.iFSub(V0, V1, V2), Pc) => "FSub #(V0) #(V1) #(V2)".
  showIns(.iFMul(V0, V1, V2), Pc) => "FMul #(V0) #(V1) #(V2)".
  showIns(.iFDiv(V0, V1, V2, V3), Pc) => "FDiv $(V0) #(V1) #(V2) #(V3)".
  showIns(.iFMod(V0, V1, V2, V3), Pc) => "FMod $(V0) #(V1) #(V2) #(V3)".
  showIns(.iFAbs(V0, V1), Pc) => "FAbs #(V0) #(V1)".
  showIns(.iFEq(V0, V1, V2), Pc) => "FEq #(V0) #(V1) #(V2)".
  showIns(.iFLt(V0, V1, V2), Pc) => "FLt #(V0) #(V1) #(V2)".
  showIns(.iFGe(V0, V1, V2), Pc) => "FGe #(V0) #(V1) #(V2)".
  showIns(.iAlloc(V0, V1, V2), Pc) => "Alloc #(V0) $(V1) $(V2)".
  showIns(.iClosure(V0, V1, V2), Pc) => "Closure #(V0) $(V1) #(V2)".
  showIns(.iFiber(V0, V1), Pc) => "Fiber #(V0) #(V1)".
  showIns(.iSuspend(V0, V1, V2), Pc) => "Suspend #(V0) #(V1) #(V2)".
  showIns(.iResume(V0, V1, V2), Pc) => "Resume #(V0) #(V1) #(V2)".
  showIns(.iRetire(V0, V1), Pc) => "Retire #(V0) #(V1)".
  showIns(.iUnderflow, Pc) => "Underflow".
  showIns(.iLine(V0), Pc) => "Line $(V0)".
  showIns(.iBind(V0, V1), Pc) => "Bind $(V0) #(V1)".
  showIns(.iDBug(V0), Pc) => "dBug $(V0)".


  showPc:(cons[integer]) => string.
  showPc(Pcs) => "#(spaces(size(Pcs)))#(interleave(Pcs//disp,".")*)".

  spaces:(integer)=>string.
  spaces(Ln) => let{.
    sp(0) => [].
    sp(N) => [` `,..sp(N-1)].
  .} in _implode(sp(Ln)).

  bumpPc:(cons[integer]) => cons[integer].
  bumpPc([Pc,..Rest]) => [Pc+1,..Rest].

  public opcodeHash = 694396771253745882.
}
