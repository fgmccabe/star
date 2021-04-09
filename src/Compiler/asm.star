star.compiler.asm{
  -- Automatically Generated Assembler -- Do NOT Edit
  import star.
  import star.multi.
  import star.sort.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.
  import star.compiler.ltipe.


  public codeSegment ::= method(termLbl,tipe,assemOp) |
    global(termLbl,tipe,assemOp) |
    struct(termLbl,tipe,tipe).

  public assemOp ::=
    iBlock(ltipe,cons[assemOp]) |
    .iHalt |
      iCall(termLbl) |
      iOCall(integer) |
      iEscape(string) |
      iTail(termLbl) |
      iOTail(integer) |
      .iRet |
      iBrk(integer) |
      .iDrop |
      .iDup |
      iRst(integer) |
      .iLdV |
      iLdG(string) |
      iLdC(term) |
      iLet(ltipe,integer,assemOp) |
      iLdA(integer) |
      iLdL(integer) |
      iStL(integer) |
      iStV(integer) |
      iTL(integer) |
      iStA(integer) |
      iStG(string) |
      iTG(string) |
      iCLbl(assemOp,assemOp) |
      iCV(assemOp,assemOp) |
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
      iICmp(assemOp,assemOp) |
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
      iFCmp(assemOp,assemOp) |
      iAlloc(termLbl) |
      iAlTpl(termLbl) |
      iUnpack(termLbl,assemOp,assemOp) |
      iCmp(assemOp,assemOp) |
      iFrame(term) |
      idLine(term) |
      .idBug |
      .idBreak |
      
      iLocal(string,string,string,integer) |
      iLine(term).
  
  litTbl:(map[term,integer]) => term.
  litTbl(Lts) => mkTpl(sort(Lts::cons[keyval[term,integer]],((T1->Ix1), (T2->Ix2)) => Ix1<Ix2)//(K->_)=>K).

  sortLines:(map[term,integer]) => cons[term].
  sortLines(Lns) => (sort(Lns::cons[keyval[term,integer]],compLine)//(K->_)=>K).

  compLine:(keyval[term,integer],keyval[term,integer])=>boolean.
  compLine(T1->P1,T2->P2) => P1<P2.

  public implementation display[codeSegment] => {.
    disp(method(Nm,Sig,Ins)) => ssSeq([disp(Nm),ss(":"),disp(Sig),ss("\n"),
	showMnem(Ins,"")]).
    disp(global(Nm,Sig,Ins)) => ssSeq([ss("global "),disp(Nm),ss(":"),disp(Sig),ss("\n"),
	showMnem(Ins,"")]).
  .}

  public implementation display[assemOp] => {.
    disp(Op) => showMnem(Op,"")
  .}

  showMnem:(assemOp,string) => ss.
  showMnem(iBlock(Tp,Code),Off) =>
    ssSeq([ss(Off),ss("block:"),disp(Tp),ss("\n"),
	ssSeq(interleave(Code//(Ins)=>showMnem(Ins,Off++"  "),ss("\n"))),ss("\n"),
	ss(Off),ss("end")]).
  showMnem(iLocal(Nm,Frm,End,_Off),Off) => ssSeq([ss(Off),ss(Nm),ss("::"),disp(Frm),ss("-"),disp(End)]).
  showMnem(.iHalt,Off) => ssSeq([ss(Off),ss("Halt")]).
  showMnem(iCall(U),Off) => ssSeq([ss(Off),ss("Call"),ss(" "),disp(U)]).
  showMnem(iOCall(U),Off) => ssSeq([ss(Off),ss("OCall"),ss(" "),disp(U)]).
  showMnem(iEscape(U),Off) => ssSeq([ss(Off),ss("Escape"),ss(" "),ss(U)]).
  showMnem(iTail(U),Off) => ssSeq([ss(Off),ss("Tail"),ss(" "),disp(U)]).
  showMnem(iOTail(U),Off) => ssSeq([ss(Off),ss("OTail"),ss(" "),disp(U)]).
  showMnem(.iRet,Off) => ssSeq([ss(Off),ss("Ret")]).
  showMnem(iBrk(Cnt),Off) => ssSeq([ss(Off),ss("Brk"),ss(" "),disp(Cnt)]).
  showMnem(.iDrop,Off) => ssSeq([ss(Off),ss("Drop")]).
  showMnem(.iDup,Off) => ssSeq([ss(Off),ss("Dup")]).
  showMnem(iRst(U),Off) => ssSeq([ss(Off),ss("Rst"),ss(" "),disp(U)]).
  showMnem(.iLdV,Off) => ssSeq([ss(Off),ss("LdV")]).
  showMnem(iLdG(U),Off) => ssSeq([ss(Off),ss("LdG"),ss(" "),disp(U)]).
  showMnem(iLdC(U),Off) => ssSeq([ss(Off),ss("LdC"),ss(" "),disp(U)]).
  showMnem(iLdA(U),Off) => ssSeq([ss(Off),ss("LdA"),ss(" "),disp(U)]).
  showMnem(iLdL(U),Off) => ssSeq([ss(Off),ss("LdL"),ss(" "),disp(U)]).
  showMnem(iStL(U),Off) => ssSeq([ss(Off),ss("StL"),ss(" "),disp(U)]).
  showMnem(iStV(U),Off) => ssSeq([ss(Off),ss("StV"),ss(" "),disp(U)]).
  showMnem(iTL(U),Off) => ssSeq([ss(Off),ss("TL"),ss(" "),disp(U)]).
  showMnem(iStA(U),Off) => ssSeq([ss(Off),ss("StA"),ss(" "),disp(U)]).
  showMnem(iStG(U),Off) => ssSeq([ss(Off),ss("StG"),ss(" "),disp(U)]).
  showMnem(iTG(U),Off) => ssSeq([ss(Off),ss("TG"),ss(" "),disp(U)]).
  showMnem(iCLbl(Th,El),Off) => ssSeq([ss(Off),ss("CLbl\n"),
      showMnem(Th,Off++"  "),ss("\n"),showMnem(El,Off++"  ")]).
  showMnem(iCV(Th,El),Off) => ssSeq([ss(Off),ss("CV\n"),
      showMnem(Th,Off++"  "),ss("\n"),showMnem(El,Off++"  ")]).
  showMnem(iNth(U),Off) => ssSeq([ss(Off),ss("Nth"),ss(" "),disp(U)]).
  showMnem(iStNth(U),Off) => ssSeq([ss(Off),ss("StNth"),ss(" "),disp(U)]).
  showMnem(iGet(U),Off) => ssSeq([ss(Off),ss("Get"),ss(" "),disp(U)]).
  showMnem(iSet(U),Off) => ssSeq([ss(Off),ss("Set"),ss(" "),disp(U)]).
  showMnem(iCase(U),Off) => ssSeq([ss(Off),ss("Case"),ss(" "),disp(U)]).
  showMnem(.iIAdd,Off) => ssSeq([ss(Off),ss("IAdd")]).
  showMnem(.iISub,Off) => ssSeq([ss(Off),ss("ISub")]).
  showMnem(.iIMul,Off) => ssSeq([ss(Off),ss("IMul")]).
  showMnem(.iIDiv,Off) => ssSeq([ss(Off),ss("IDiv")]).
  showMnem(.iIMod,Off) => ssSeq([ss(Off),ss("IMod")]).
  showMnem(.iIAbs,Off) => ssSeq([ss(Off),ss("IAbs")]).
  showMnem(.iIEq,Off) => ssSeq([ss(Off),ss("IEq")]).
  showMnem(.iILt,Off) => ssSeq([ss(Off),ss("ILt")]).
  showMnem(.iIGe,Off) => ssSeq([ss(Off),ss("IGe")]).
  showMnem(iICmp(Th,El),Off) => ssSeq([ss(Off),ss("ICmp"),
      showMnem(Th,Off++"  "),ss("\n"),showMnem(El,Off++"  ")]).
  showMnem(.iBAnd,Off) => ssSeq([ss(Off),ss("BAnd")]).
  showMnem(.iBOr,Off) => ssSeq([ss(Off),ss("BOr")]).
  showMnem(.iBXor,Off) => ssSeq([ss(Off),ss("BXor")]).
  showMnem(.iBLsl,Off) => ssSeq([ss(Off),ss("BLsl")]).
  showMnem(.iBLsr,Off) => ssSeq([ss(Off),ss("BLsr")]).
  showMnem(.iBAsr,Off) => ssSeq([ss(Off),ss("BAsr")]).
  showMnem(.iBNot,Off) => ssSeq([ss(Off),ss("BNot")]).
  showMnem(.iFAdd,Off) => ssSeq([ss(Off),ss("FAdd")]).
  showMnem(.iFSub,Off) => ssSeq([ss(Off),ss("FSub")]).
  showMnem(.iFMul,Off) => ssSeq([ss(Off),ss("FMul")]).
  showMnem(.iFDiv,Off) => ssSeq([ss(Off),ss("FDiv")]).
  showMnem(.iFMod,Off) => ssSeq([ss(Off),ss("FMod")]).
  showMnem(.iFAbs,Off) => ssSeq([ss(Off),ss("FAbs")]).
  showMnem(.iFEq,Off) => ssSeq([ss(Off),ss("FEq")]).
  showMnem(.iFLt,Off) => ssSeq([ss(Off),ss("FLt")]).
  showMnem(.iFGe,Off) => ssSeq([ss(Off),ss("FGe")]).
  showMnem(iFCmp(Th,El),Off) => ssSeq([ss(Off),ss("FCmp"),
      showMnem(Th,Off++"  "),ss("\n"),showMnem(El,Off++"  ")]).
  showMnem(iAlloc(U),Off) => ssSeq([ss(Off),ss("Alloc"),ss(" "),disp(U)]).
  showMnem(iAlTpl(U),Off) => ssSeq([ss(Off),ss("AlTpl"),ss(" "),disp(U)]).
  showMnem(iUnpack(U,Th,El),Off) => ssSeq([ss(Off),ss("Unpack"),ss(" "),disp(U),
	showMnem(Th,Off++"  "),ss("\n"),showMnem(El,Off++"  ")]).
  showMnem(iCmp(Th,El),Off) => ssSeq([ss(Off),ss("Cmp"),ss(" "),
	showMnem(Th,Off++"  "),ss("\n"),showMnem(El,Off++"  ")]).
  showMnem(iFrame(U),Off) => ssSeq([ss(Off),ss("Frame"),ss(" "),disp(U)]).
  showMnem(idLine(U),Off) => ssSeq([ss(Off),ss("dLine"),ss(" "),disp(U)]).
  showMnem(.idBug,Off) => ssSeq([ss(Off),ss("dBug")]).
  showMnem(.idBreak,Off) => ssSeq([ss(Off),ss("dBreak")]).
}
