star.compiler.assem{
  -- Automatically Generated Assembler -- Do NOT Edit
  import star.
  import star.multi.
  import star.sort.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.
  import star.compiler.ltipe.

  public codePolicy ::= hardDefinition | softDefinition.

  public implementation display[codePolicy] => {.
    disp(hardDefinition) => ss("hard").
    disp(softDefinition) => ss("soft").
  .}

  public codeSegment ::= func(termLbl,codePolicy,tipe,cons[assemOp]) |
    global(termLbl,tipe,cons[assemOp]) |
    struct(termLbl,tipe,integer) |
    tipe(tipe,cons[(termLbl,tipe,integer)]).

  public assemOp ::=
    iHalt(integer) |
    .iAbort |
    iCall(termLbl) |
    iOCall(integer) |
    iEscape(string) |
    iTCall(termLbl) |
    iTOCall(integer) |
    .iRet |
    iJmp(assemLbl) |
    .iDrop |
    iDropTo(integer) |
    .iDup |
    iRst(integer) |
    .iSwap |
    iTag(termLbl) |
    .iPrompt |
    .iCut |
    .iResume |
    .iTResume |
    .iUnderflow |
    .iLdV |
    iLdC(term) |
    iLdA(integer) |
    iLdL(integer) |
    iStL(integer) |
    iStV(integer) |
    iTL(integer) |
    iStA(integer) |
    iLdG(string) |
    iStG(string) |
    iTG(string) |
    .iCell |
    .iGet |
    .iAssign |
    iCLbl(termLbl,assemLbl) |
    iCmpVd(integer) |
    iNth(integer) |
    iStNth(integer) |
    iIf(assemLbl) |
    iIfNot(assemLbl) |
    iCase(integer) |
    iIndxJmp(integer) |
    iUnpack(termLbl,assemLbl) |
    .iIAdd |
    .iISub |
    .iIMul |
    .iIDiv |
    .iIMod |
    .iIAbs |
    .iIEq |
    .iILt |
    .iIGe |
    iICmp(integer) |
    .iBAnd |
    .iBOr |
    .iBXor |
    .iBLsl |
    .iBLsr |
    .iBAsr |
    .iBNot |
    .iFAdd |
    .iFSub |
    .iFMul |
    .iFDiv |
    .iFMod |
    .iFAbs |
    .iFEq |
    .iFLt |
    .iFGe |
    iFCmp(assemLbl) |
    iAlloc(termLbl) |
    iAlTpl(termLbl) |
    iCmp(assemLbl) |
    iComp(integer) |
    iFrame(ltipe) |
    idLine(term) |
    .idBug |
    .idBreak |

    iLbl(assemLbl) |
    iLocal(string,string,string,integer) |
    iLine(term).

  public assemLbl ::= al(string).

  public assem:(codeSegment) => term.
  assem(func(Nm,H,Sig,Ins)) where
    (Lt0,_) .= findLit([],symb(Nm)) &&
    (_,Lbls) .= genLblTbl(Ins,0,[]) &&
    (Code,Lts,Lns,Lcs,_,Max) .= assemBlock(Ins,Lbls,Lt0,[],[],0,0,[]) =>
    mkCons("func",
      [symb(Nm),encPolicy(H),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code::cons[term]),litTbl(Lts),mkTpl([Lcl|Lcl in Lcs]),
            mkTpl(sortLines(Lns))]).
  assem(global(Nm,Sig,Ins)) where
    (Lt0,_) .= findLit([],symb(Nm)) &&
    (_,Lbls) .= genLblTbl(Ins,0,[]) &&
    (Code,Lts,Lns,Lcs,_,Max) .= assemBlock(Ins,Lbls,Lt0,[],[],0,0,[]) =>
    mkCons("global",
       [symb(Nm),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code::cons[term]),litTbl(Lts),mkTpl({Lcl|Lcl in Lcs}),
            mkTpl(sortLines(Lns))]).
  assem(struct(Lbl,Tp,Ix)) =>
    mkCons("struct",[symb(Lbl),strg(encodeSignature(Tp)),intgr(Ix)]).

  encPolicy(hardDefinition) => mkTpl([]).
  encPolicy(softDefinition) => mkTpl([strg("soft")]).

  private assemBlock:(cons[assemOp],map[string,integer],map[term,integer],map[term,integer],
                      set[term],integer,integer,cons[integer]) =>
                                        (multi[term],map[term,integer],map[term,integer],set[term],integer,integer).
  assemBlock(Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (End,_).=genLblTbl(Code,Pc,[])
    => mnem(Code,[],Lbls,Lts,Lns,Lcs,Pc,MxLcl,[End,..Ends]).

  private mnem:(cons[assemOp],multi[term],map[string,integer],map[term,integer],map[term,integer],set[term],integer,integer,cons[integer]) =>
    (multi[term],map[term,integer],map[term,integer],set[term],integer,integer).
  mnem([],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,_) => (Code,Lts,Lns,Lcs,Pc,MxLcl).
  mnem([iLbl(_),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends).
  mnem([iLocal(Nm,Frm,End,Off),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where
    F ^= Lbls[Frm] &&
    T ^= Lbls[End] =>
    mnem(Ins,Code,Lbls,Lts,Lns,Lcs\+mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)]),Pc,MxLcl,Ends).
  mnem([iLine(Lc),..Ins],Code,Lbs,Lts,Lns,Lcs,Pc,MxLcl,Ends) =>
        mnem([idLine(Lc),..Ins],Code,Lbs,Lts,Lns[mkTpl([Lc,intgr(Pc)])->Pc],Lcs,Pc,MxLcl,Ends).
  mnem([iHalt(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(0),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iAbort,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(1)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(2),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iOCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(3),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iEscape(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(4),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iTCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(5),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iTOCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(6),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iRet,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(7)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iJmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(8),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iDrop,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(9)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iDropTo(V),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(10),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iDup,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(11)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iRst(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(12),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iSwap,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(13)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iTag(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(14),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iPrompt,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(15)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iCut,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(16)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iResume,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(17)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iTResume,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(18)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iUnderflow,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(19)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iLdV,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(20)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iLdC(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Code++[intgr(21),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iLdA(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(22),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iLdL(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(23),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iStL(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(24),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iStV(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(25),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iTL(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(26),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iStA(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(27),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iLdG(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(28),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iStG(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(29),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iTG(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(30),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iCell,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(31)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iGet,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(32)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iAssign,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(33)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iCLbl(U,al(V)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(34),intgr(LtNo),intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).
  mnem([iCmpVd(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(35),intgr(findEnd(Ends,U)-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iNth(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(36),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iStNth(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(37),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iIf(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(38),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iIfNot(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(39),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iCase(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(40),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iIndxJmp(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(41),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iUnpack(U,al(V)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(42),intgr(LtNo),intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).
  mnem([.iIAdd,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(43)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iISub,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(44)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIMul,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(45)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIDiv,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(46)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIMod,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(47)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIAbs,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(48)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIEq,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(49)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iILt,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(50)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIGe,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(51)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iICmp(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(52),intgr(findEnd(Ends,U)-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iBAnd,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(53)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBOr,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(54)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBXor,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(55)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBLsl,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(56)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBLsr,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(57)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBAsr,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(58)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBNot,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(59)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFAdd,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(60)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFSub,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(61)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFMul,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(62)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFDiv,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(63)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFMod,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(64)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFAbs,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(65)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFEq,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(66)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFLt,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(67)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFGe,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(68)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iFCmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(69),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iAlloc(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(70),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iAlTpl(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(71),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iCmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(72),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iComp(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(73),intgr(findEnd(Ends,U)-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iFrame(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,strg(U::string)) => mnem(Ins,Code++[intgr(74),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([idLine(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Code++[intgr(75),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.idBug,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(76)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.idBreak,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(77)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).


  genLblTbl:(cons[assemOp],integer,map[string,integer]) => (integer,map[string,integer]).
  genLblTbl([],Pc,Lbls) => (Pc,Lbls).
  genLblTbl([iLbl(al(Lbl)),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls[Lbl->Pc]).
  genLblTbl([iLocal(_,_,_,_),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls).
  genLblTbl([iLine(T),..Ins],Pc,Lbs) => genLblTbl([idLine(T),..Ins],Pc,Lbs).
  genLblTbl([iHalt(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iAbort,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iEscape(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTOCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iRet,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iJmp(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iDrop,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iDropTo(B),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iDup,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iRst(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iSwap,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iTag(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iPrompt,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iCut,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iResume,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iTResume,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iUnderflow,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iLdV,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iLdC(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdA(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdL(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStL(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStV(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTL(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStA(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdG(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStG(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTG(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iCell,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iGet,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iAssign,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iCLbl(A,al(B)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+5,Lbls).
  genLblTbl([iCmpVd(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iNth(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStNth(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iIf(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iIfNot(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCase(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iIndxJmp(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iUnpack(A,al(B)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+5,Lbls).
  genLblTbl([.iIAdd,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iISub,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIMul,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIDiv,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIMod,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIAbs,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIEq,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iILt,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIGe,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iICmp(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iBAnd,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBOr,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBXor,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBLsl,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBLsr,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBAsr,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBNot,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFAdd,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFSub,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFMul,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFDiv,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFMod,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFAbs,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFEq,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFLt,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFGe,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iFCmp(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iAlloc(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iAlTpl(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCmp(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iComp(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iFrame(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([idLine(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.idBug,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.idBreak,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).


  findEnd:(cons[integer],integer)=>integer.
  findEnd([E,.._],0) => E.
  findEnd([_,..Ends],Lvl) => findEnd(Ends,Lvl-1).

  findLit:(map[term,integer],term) => (map[term,integer],integer).
  findLit(Lts,T) where O ^= Lts[T] => (Lts,O).
  findLit(Lts,T) where O .= size(Lts) => (Lts[T->O],O).

  litTbl:(map[term,integer]) => term.
  litTbl(Lts) => mkTpl(sort(Lts::cons[keyval[term,integer]],((T1->Ix1), (T2->Ix2)) => Ix1<Ix2)//(K->_)=>K).

  sortLines:(map[term,integer]) => cons[term].
  sortLines(Lns) => (sort(Lns::cons[keyval[term,integer]],compLine)//(K->_)=>K).

  compLine:(keyval[term,integer],keyval[term,integer])=>boolean.
  compLine(T1->P1,T2->P2) => P1<P2.

  public implementation display[assemLbl] => {.
    disp(al(L)) => ss(L)
  .}

  public implementation display[codeSegment] => {.
    disp(func(Nm,H,Sig,Ins)) => ssSeq([disp(H),ss(" "),disp(Nm),ss(":"),disp(Sig),ss("\n"),..(showMnem(Ins,0)::cons[ss])]).
    disp(global(Nm,Sig,Ins)) => ssSeq([ss("global "),disp(Nm),ss(":"),disp(Sig),ss("\n"),..(showMnem(Ins,0)::cons[ss])]).
  .}

  public implementation display[assemOp] => {.
    disp(Op) => ssSeq(showMnem([Op],0)::cons[ss]).
  .}

  showMnem:(cons[assemOp],integer) => multi[ss].
  showMnem([],_) => .null.
  showMnem([iLbl(al(Lb)),..Ins],Pc) => single(ssSeq([ss(Lb),ss(":\n")]))++showMnem(Ins,Pc).
  showMnem([iLocal(Nm,Frm,End,_Off),..Ins],Pc) => single(ssSeq([ss(Nm),ss("::"),disp(Frm),ss("-"),disp(End),ss("\n")]))++showMnem(Ins,Pc).
  showMnem([iHalt(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Halt"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iAbort,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Abort"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iCall(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Call"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iOCall(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("OCall"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iEscape(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Escape"),ss(" "),ss(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iTCall(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("TCall"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iTOCall(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("TOCall"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iRet,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Ret"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iJmp(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Jmp"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iDrop,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Drop"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iDropTo(V),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("DropTo"),ss(" "),disp(V),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iDup,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Dup"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iRst(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Rst"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iSwap,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Swap"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iTag(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Tag"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iPrompt,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Prompt"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iCut,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Cut"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iResume,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Resume"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iTResume,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("TResume"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iUnderflow,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Underflow"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iLdV,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdV"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iLdC(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdC"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iLdA(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdA"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iLdL(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdL"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStL(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StL"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStV(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StV"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iTL(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("TL"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStA(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StA"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iLdG(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdG"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStG(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StG"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iTG(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("TG"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iCell,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Cell"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iGet,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Get"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iAssign,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Assign"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iCLbl(U,al(V)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("CLbl"),ss(" "),disp(U),ss(" "),disp(V),ss("\n")])) ++ showMnem(Ins,Pc+5).
  showMnem([iCmpVd(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("CmpVd"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iNth(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Nth"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStNth(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StNth"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iIf(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("If"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iIfNot(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IfNot"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iCase(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Case"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iIndxJmp(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IndxJmp"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iUnpack(U,al(V)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Unpack"),ss(" "),disp(U),ss(" "),disp(V),ss("\n")])) ++ showMnem(Ins,Pc+5).
  showMnem([.iIAdd,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IAdd"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iISub,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("ISub"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIMul,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IMul"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIDiv,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IDiv"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIMod,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IMod"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIAbs,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IAbs"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIEq,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IEq"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iILt,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("ILt"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIGe,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IGe"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iICmp(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("ICmp"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iBAnd,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("BAnd"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iBOr,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("BOr"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iBXor,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("BXor"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iBLsl,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("BLsl"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iBLsr,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("BLsr"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iBAsr,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("BAsr"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iBNot,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("BNot"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iFAdd,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FAdd"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iFSub,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FSub"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iFMul,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FMul"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iFDiv,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FDiv"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iFMod,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FMod"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iFAbs,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FAbs"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iFEq,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FEq"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iFLt,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FLt"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iFGe,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FGe"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iFCmp(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("FCmp"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iAlloc(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Alloc"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iAlTpl(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("AlTpl"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iCmp(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Cmp"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iComp(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Comp"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iFrame(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Frame"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([idLine(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("dLine"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.idBug,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("dBug"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.idBreak,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("dBreak"),ss("\n")])) ++ showMnem(Ins,Pc+1).

}
