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
    struct(termLbl,tipe,tipe).

  public assemOp ::=
    .iHalt |
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
    iCLbl(assemLbl) |
    iCV(assemLbl) |
    iNth(integer) |
    iStNth(integer) |
    iGet(termLbl) |
    iSet(termLbl) |
    iCase(integer) |
    .iIAdd |
    .iISub |
    .iIMul |
    .iIDiv |
    .iIMod |
    .iIAbs |
    .iIEq |
    .iILt |
    .iIGe |
    iICmp(assemLbl) |
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
    iUnpack(termLbl,assemLbl) |
    iCmp(assemLbl) |
    iFrame(term) |
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
    (Code,Lts,Lns,Max,Lcs) .= mnem(Ins,genLblTbl(Ins,0,[]),Lt0,[],[],0,0,[]) =>
    term(tLbl("method",7),
      [symb(Nm),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code),litTbl(Lts),mkTpl(Lcs),
            mkTpl(sortLines(Lns))]).
  assem(global(Nm,Sig,Ins)) where
    (Lt0,_) .= findLit([],symb(Nm)) &&
    (Code,Lts,Lns,Max,Lcs) .= mnem(Ins,genLblTbl(Ins,0,[]),Lt0,[],[],0,0,[]) =>
    term(tLbl("global",7),
       [symb(Nm),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code),litTbl(Lts),mkTpl(Lcs),
            mkTpl(sortLines(Lns))]).
  assem(struct(Lbl,Tp,Flds)) =>
    term(tLbl("struct",3),[symb(Lbl),strg(encodeSignature(Tp)),strg(encodeSignature(Flds))]).

  private mnem:(cons[assemOp],map[string,integer],map[term,integer],map[term,integer],set[term],integer,integer,cons[term]) =>
    (cons[term],map[term,integer],map[term,integer],integer,cons[term]).
  mnem([],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => (reverse(Code),Lts,Lns,MxLcl,{Lcl|Lcl in Lcs}).
  mnem([iLbl(_),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code).
  mnem([iLocal(Nm,Frm,End,Off),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where
    F ^= Lbls[Frm] &&
    T ^= Lbls[End] =>
    mnem(Ins,Lbls,Lts,Lns,Lcs\+mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)]),Pc,MxLcl,Code).
  mnem([iLine(Lc),..Ins],Lbs,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem([idLine(Lc),..Ins],Lbs,Lts,Lns[mkTpl([Lc,intgr(Pc)])->Pc],Lcs,Pc,MxLcl,Code).
  mnem([.iHalt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(0),..Code]).
  mnem([iCall(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(1),..Code]).
  mnem([iOCall(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(U),intgr(2),..Code]).
  mnem([iEscape(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[strg(U),intgr(3),..Code]).
  mnem([iTail(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(4),..Code]).
  mnem([iOTail(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(U),intgr(5),..Code]).
  mnem([.iRet,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(6),..Code]).
  mnem([iJmp(al(U)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[U] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(7),..Code]).
  mnem([.iDrop,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(8),..Code]).
  mnem([.iDup,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(9),..Code]).
  mnem([iRst(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(U),intgr(10),..Code]).
  mnem([.iLdV,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(11),..Code]).
  mnem([iLdG(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[strg(U),intgr(12),..Code]).
  mnem([iLdC(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(13),..Code]).
  mnem([iLdA(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(U),intgr(14),..Code]).
  mnem([iLdL(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),[intgr(U),intgr(15),..Code]).
  mnem([iStL(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),[intgr(U),intgr(16),..Code]).
  mnem([iStV(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),[intgr(U),intgr(17),..Code]).
  mnem([iTL(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),[intgr(U),intgr(18),..Code]).
  mnem([iStA(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(U),intgr(19),..Code]).
  mnem([iStG(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[strg(U),intgr(20),..Code]).
  mnem([iTG(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[strg(U),intgr(21),..Code]).
  mnem([iCLbl(al(U)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[U] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(22),..Code]).
  mnem([iCV(al(U)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[U] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(23),..Code]).
  mnem([iNth(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(U),intgr(24),..Code]).
  mnem([iStNth(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(U),intgr(25),..Code]).
  mnem([iGet(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(26),..Code]).
  mnem([iSet(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(27),..Code]).
  mnem([iCase(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(U),intgr(28),..Code]).
  mnem([.iIAdd,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(29),..Code]).
  mnem([.iISub,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(30),..Code]).
  mnem([.iIMul,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(31),..Code]).
  mnem([.iIDiv,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(32),..Code]).
  mnem([.iIMod,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(33),..Code]).
  mnem([.iIAbs,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(34),..Code]).
  mnem([.iIEq,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(35),..Code]).
  mnem([.iILt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(36),..Code]).
  mnem([.iIGe,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(37),..Code]).
  mnem([iICmp(al(U)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[U] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(38),..Code]).
  mnem([.iBAnd,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(39),..Code]).
  mnem([.iBOr,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(40),..Code]).
  mnem([.iBXor,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(41),..Code]).
  mnem([.iBLsl,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(42),..Code]).
  mnem([.iBLsr,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(43),..Code]).
  mnem([.iBAsr,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(44),..Code]).
  mnem([.iBNot,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(45),..Code]).
  mnem([.iFAdd,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(46),..Code]).
  mnem([.iFSub,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(47),..Code]).
  mnem([.iFMul,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(48),..Code]).
  mnem([.iFDiv,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(49),..Code]).
  mnem([.iFMod,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(50),..Code]).
  mnem([.iFAbs,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(51),..Code]).
  mnem([.iFEq,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(52),..Code]).
  mnem([.iFLt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(53),..Code]).
  mnem([.iFGe,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(54),..Code]).
  mnem([iFCmp(al(U)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[U] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(55),..Code]).
  mnem([iAlloc(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(56),..Code]).
  mnem([iAlTpl(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(57),..Code]).
  mnem([iUnpack(U,al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,symb(U)) && Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,[intgr(Tgt-Pc-5),intgr(LtNo),intgr(58),..Code]).
  mnem([iCmp(al(U)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[U] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(59),..Code]).
  mnem([iFrame(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(60),..Code]).
  mnem([iThrow(al(U)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[U] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(61),..Code]).
  mnem([iUnwind(al(U)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[U] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(62),..Code]).
  mnem([idLine(U),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(63),..Code]).
  mnem([.idBug,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(64),..Code]).
  mnem([.idBreak,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(65),..Code]).


  genLblTbl:(cons[assemOp],integer,map[string,integer]) => map[string,integer].
  genLblTbl([],_,Lbls) => Lbls.
  genLblTbl([iLbl(al(Lbl)),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls[Lbl->Pc]).
  genLblTbl([iLocal(_,_,_,_),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls).
  genLblTbl([iLine(T),..Ins],Pc,Lbs) => genLblTbl([idLine(T),..Ins],Pc,Lbs).
  genLblTbl([.iHalt,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iCall(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOCall(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iEscape(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTail(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOTail(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iRet,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iJmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iDrop,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iDup,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iRst(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iLdV,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iLdG(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdC(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdA(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdL(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStL(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStV(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTL(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStA(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStG(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTG(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCLbl(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCV(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iNth(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStNth(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iGet(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iSet(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCase(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iIAdd,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iISub,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIMul,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIDiv,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIMod,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIAbs,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIEq,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iILt,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIGe,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iICmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iBAnd,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBOr,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBXor,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBLsl,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBLsr,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBAsr,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBNot,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFAdd,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFSub,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFMul,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFDiv,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFMod,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFAbs,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFEq,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFLt,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFGe,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iFCmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iAlloc(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iAlTpl(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iUnpack(_,_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+5,Lbls).
  genLblTbl([iCmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iFrame(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iThrow(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iUnwind(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([idLine(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.idBug,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.idBreak,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).


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
  showMnem([.iHalt,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Halt"),ss("\n")])) ++ showMnem(Ins,Pc+1).
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
  showMnem([iCLbl(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("CLbl"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iCV(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("CV"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iNth(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Nth"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iStNth(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("StNth"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iGet(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Get"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iSet(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Set"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iCase(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Case"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.iIAdd,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IAdd"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iISub,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("ISub"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIMul,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IMul"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIDiv,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IDiv"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIMod,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IMod"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIAbs,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IAbs"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIEq,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IEq"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iILt,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("ILt"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.iIGe,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("IGe"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([iICmp(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("ICmp"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
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
  showMnem([iUnpack(U,al(V)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Unpack"),ss(" "),disp(U),ss(" "),disp(V),ss("\n")])) ++ showMnem(Ins,Pc+5).
  showMnem([iCmp(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Cmp"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iFrame(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Frame"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iThrow(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Throw"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([iUnwind(al(U)),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("Unwind"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([idLine(U),..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("dLine"),ss(" "),disp(U),ss("\n")])) ++ showMnem(Ins,Pc+3).
  showMnem([.idBug,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("dBug"),ss("\n")])) ++ showMnem(Ins,Pc+1).
  showMnem([.idBreak,..Ins],Pc) => single(ssSeq([disp(Pc),ss(":"),ss("dBreak"),ss("\n")])) ++ showMnem(Ins,Pc+1).

}
