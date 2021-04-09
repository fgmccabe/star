star.compiler.assem{
  -- Automatically Generated Assembler -- Do NOT Edit
  import star.
  import star.multi.
  import star.sort.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.
  import star.compiler.ltipe.


  public codeSegment ::= method(termLbl,tipe,cons[assemOp]) |
    global(termLbl,tipe,cons[assemOp]) |
    struct(termLbl,integer,tipe,tipe) |
    tipe(tipe,cons[(termLbl,tipe,integer)]).

  public assemOp ::=
    iHalt(integer) |
    iCall(termLbl) |
    iOCall(integer) |
    iEscape(string) |
    iTail(termLbl) |
    iOTail(integer) |
    .iRet |
    iJmp(assemLbl) |
    .iDrop |
    .iDup |
    iRst(integer) |
    .iPrompt |
    .iCut |
    .iRestore |
    .iLdV |
    iLdG(string) |
    iLdC(term) |
    iLdA(integer) |
    iLdL(integer) |
    iStL(integer) |
    iStV(integer) |
    iTL(integer) |
    iStA(integer) |
    iStG(string) |
    iTG(string) |
    iCLbl(term,assemLbl) |
    iCmpVd(integer) |
    iNth(integer) |
    iStNth(integer) |
    iGet(termLbl) |
    iSet(termLbl) |
    iIf(integer) |
    iIfNot(integer) |
    iCase(integer) |
    iIndxJmp(integer) |
    iUnpack(term,assemLbl) |
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
    iThrow(assemLbl) |
    iUnwind(assemLbl) |
    idLine(term) |
    .idBug |
    .idBreak |

    iLbl(assemLbl) |
    iLocal(string,string,string,integer) |
    iLine(term).

  public assemLbl ::= al(string).

  public assem:(codeSegment) => term.
  assem(method(Nm,Sig,Ins)) where
    (Lt0,_) .= findLit([],symb(Nm)) &&
    (_,Lbls) .= genLblTbl(Ins,0,[]) &&
    (Code,Lts,Lns,Lcs,_,Max) .= assemBlock(Ins,Lbls,Lt0,[],[],0,0,[]) =>
    term(tLbl("method",7),
      [symb(Nm),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code::cons[term]),litTbl(Lts),mkTpl({Lcl|Lcl in Lcs}),
            mkTpl(sortLines(Lns))]).
  assem(global(Nm,Sig,Ins)) where
    (Lt0,_) .= findLit([],symb(Nm)) &&
    (_,Lbls) .= genLblTbl(Ins,0,[]) &&
    (Code,Lts,Lns,Lcs,_,Max) .= assemBlock(Ins,Lbls,Lt0,[],[],0,0,[]) =>
    term(tLbl("global",7),
       [symb(Nm),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code::cons[term]),litTbl(Lts),mkTpl({Lcl|Lcl in Lcs}),
            mkTpl(sortLines(Lns))]).
  assem(struct(Lbl,Ix,Tp,Flds)) =>
    term(tLbl("struct",3),[symb(Lbl),intgr(Ix),strg(encodeSignature(Tp)),strg(encodeSignature(Flds))]).

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
  mnem([iCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(1),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iOCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(2),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iEscape(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(3),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iTail(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(4),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iOTail(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(5),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iRet,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(6)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iJmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(7),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iDrop,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(8)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iDup,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(9)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iRst(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(10),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iPrompt,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(11)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iCut,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(12)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iRestore,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(13)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iLdV,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(14)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iLdG(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(15),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iLdC(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Code++[intgr(16),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iLdA(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(17),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iLdL(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(18),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iStL(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(19),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iStV(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(20),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iTL(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(21),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iStA(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(22),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iStG(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(23),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iTG(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(24),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iCLbl(U,al(V)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(25),intgr(LtNo),intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).
  mnem([iCmpVd(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(26),intgr(findEnd(Ends,U)-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iNth(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(27),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iStNth(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(28),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iGet(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(29),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iSet(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(30),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iIf(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(31),intgr(findEnd(Ends,U)-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iIfNot(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(32),intgr(findEnd(Ends,U)-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iCase(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(33),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iIndxJmp(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(34),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iUnpack(U,al(V)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(35),intgr(LtNo),intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).
  mnem([.iIAdd,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(36)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iISub,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(37)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIMul,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(38)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIDiv,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(39)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIMod,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(40)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIAbs,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(41)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIEq,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(42)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iILt,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(43)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIGe,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(44)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iICmp(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(45),intgr(findEnd(Ends,U)-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iBAnd,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(46)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBOr,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(47)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBXor,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(48)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBLsl,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(49)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBLsr,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(50)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBAsr,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(51)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBNot,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(52)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFAdd,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(53)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFSub,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(54)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFMul,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(55)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFDiv,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(56)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFMod,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(57)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFAbs,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(58)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFEq,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(59)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFLt,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(60)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFGe,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(61)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iFCmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(62),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iAlloc(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(63),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iAlTpl(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(64),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iCmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(65),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iComp(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(66),intgr(findEnd(Ends,U)-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iFrame(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,strg(U::string)) => mnem(Ins,Code++[intgr(67),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iThrow(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(68),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iUnwind(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(69),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([idLine(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Code++[intgr(70),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.idBug,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(71)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.idBreak,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(72)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).


  genLblTbl:(cons[assemOp],integer,map[string,integer]) => (integer,map[string,integer]).
  genLblTbl([],Pc,Lbls) => (Pc,Lbls).
  genLblTbl([iLbl(al(Lbl)),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls[Lbl->Pc]).
  genLblTbl([iLocal(_,_,_,_),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls).
  genLblTbl([iLine(T),..Ins],Pc,Lbs) => genLblTbl([idLine(T),..Ins],Pc,Lbs).
  genLblTbl([iHalt(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iEscape(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTail(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOTail(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iRet,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iJmp(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iDrop,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iDup,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iRst(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iPrompt,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iCut,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iRestore,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iLdV,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iLdG(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdC(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdA(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdL(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStL(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStV(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTL(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStA(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStG(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTG(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCLbl(A,al(B)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+5,Lbls).
  genLblTbl([iCmpVd(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iNth(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStNth(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iGet(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iSet(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iIf(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iIfNot(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
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
  genLblTbl([iThrow(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iUnwind(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
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
    disp(method(Nm,Sig,Ins)) => ssSeq([disp(Nm),ss(":"),disp(Sig),ss("\n"),..(showMnem(Ins,0)::cons[ss])]).
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
  showMnem([iCall(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Call"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iOCall(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("OCall"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iEscape(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Escape"),ss(" "),ss(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iTail(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Tail"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iOTail(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("OTail"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iRet,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Ret"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iJmp(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Jmp"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iDrop,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Drop"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iDup,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Dup"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iRst(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Rst"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iPrompt,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Prompt"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iCut,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Cut"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iRestore,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Restore"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iLdV,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdV"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iLdG(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdG"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iLdC(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdC"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iLdA(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdA"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iLdL(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("LdL"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStL(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StL"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStV(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StV"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iTL(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("TL"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStA(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StA"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStG(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StG"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iTG(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("TG"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iCLbl(U,al(V)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("CLbl"),ss(" "),disp(U),ss(" "),disp(V),ss("\n")])) ++ showMnem(Ins,Pc+5).
  showMnem([iCmpVd(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("CmpVd"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iNth(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Nth"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStNth(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StNth"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iGet(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Get"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iSet(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Set"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iIf(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("If"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iIfNot(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IfNot"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
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
  showMnem([iThrow(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Throw"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iUnwind(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Unwind"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([idLine(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("dLine"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.idBug,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("dBug"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.idBreak,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("dBreak"),ss("\n")])) ++ showMnem(Ins,Pc+1).

}
