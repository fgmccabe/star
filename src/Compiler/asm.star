star.compiler.asm{
  import star.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.

  public codeSegment ::= codeSeg(termLbl,tipe,codeBlock) |
    struct(termLbl,tipe,tipe).

  public codeBlock ::= block(tipe,list[assemOp]) .

  public assemOp ::=
    iHalt |
      iCall(termLbl) |
      iOCall(integer) |
      iEscape(string) |
      iTail(termLbl) |
      iOTail(integer) |
      iRet |
      iDrop |
      iDup |
      iRst(integer) |
      iLdG(string) |
      iLdC(term) |
      iLdA(integer) |
      iLdL(integer) |
      iStL(integer) |
      iStV(integer) |
      iTL(string) |
      iStA(integer) |
      iCLbl(string) |
      iNth(integer) |
      iStNth(integer) |
      iGetFld(string) |
      iGet(termLbl) |
      iSet(termLbl) |
      iCase(list[codeBlock],codeBlock) |
      iAlloc(termLbl) |
      iCmp(term) |
      iIf(codeBlock,codeBlock) |
      iLet(string,tipe,codeBlock) |
      iEnter(codeBlock) |
      iFrame(tipe) |
      idBug |
      idBreak |
      iLine(locn).

  public implementation display[codeSegment] => {.
    disp(C) => dispCode(C,"")
  .}

  public implementation display[codeBlock] => {.
    disp(C) => dispBlock(C,"").
  .}

  public implementation display[assemOp] => {.
    disp(C) => dispAssem(C,"").
  .}

  dispCode(codeSeg(Lbl,Tp,Block),Off) => ssSeq([ss(Off),disp(Lbl),ss(":"),disp(Tp),ss("\n"),dispBlock(Block,Off)]).

  dispAssem:(assemOp,string) => ss.
  dispAssem(iHalt,Off) => ssSeq([ss("Halt")]).
  dispAssem(iCall(Lbl),Off) => ssSeq([ss("Call "),disp(Lbl)]).
  dispAssem(iOCall(Lcl),Off) => ssSeq([ss("OCall "),disp(Lcl)]).
  dispAssem(iEscape(Nm),Off) => ssSeq([ss("Escape "),disp(Nm)]).
  dispAssem(iTail(Lbl),Off) => ssSeq([ss("Tail "),disp(Lbl)]).
  dispAssem(iOTail(Ar),Off) => ssSeq([ss("Tail "),disp(Ar)]).
  dispAssem(iRet,Off) => ssSeq([ss("Ret")]).
  dispAssem(iDrop,Off) => ssSeq([ss("Drop")]).
  dispAssem(iDup,Off) => ssSeq([ss("Dup")]).
  dispAssem(iRst(Lvl),Off) => ssSeq([ss("Rst "),disp(Lvl)]).
  dispAssem(iLdG(Nm),Off) => ssSeq([ss("LdG "),disp(Nm)]).
  dispAssem(iLdC(T),Off) => ssSeq([ss("LdC "),disp(T)]).
  dispAssem(iLdA(V),Off) => ssSeq([ss("LdA "),disp(V)]).
  dispAssem(iLdL(V),Off) => ssSeq([ss("LdL "),disp(V)]).
  dispAssem(iStL(V),Off) => ssSeq([ss("StL "),disp(V)]).
  dispAssem(iTL(V),Off) => ssSeq([ss("TL "),disp(V)]).
  dispAssem(iCLbl(T),Off) => ssSeq([ss("CLbl "),disp(T)]).
  dispAssem(iNth(O),Off) => ssSeq([ss("Nth "),disp(O)]).
  dispAssem(iStNth(O),Off) => ssSeq([ss("StNth "),disp(O)]).
  dispAssem(iGetFld(Nm),Off) => ssSeq([ss("GetFld "),disp(Nm)]).
  dispAssem(iGet(T),Off) => ssSeq([ss("Get "),disp(T)]).
  dispAssem(iSet(T),Off) => ssSeq([ss("Set "),disp(T)]).
  dispAssem(iCase(Cases,Deflt),Off) => ssSeq([ss("Case "),dispCases(Cases,Off++"  "),
      ss("\n"),ss(Off),ss("} else "),dispBlock(Deflt,Off)]).
  dispAssem(iAlloc(Lbl),Off) => ssSeq([ss("Alloc "),disp(Lbl)]).
  dispAssem(iCmp(T),Off) => ssSeq([ss("Cmp "),disp(T)]).
  dispAssem(iIf(Th,El),Off) => ssSeq([ss("If "),
      dispBlock(Th,Off),
      ss("\n"),ss(Off),ss("else"),
      dispBlock(El,Off)]).
  dispAssem(iEnter(B),Off) => ssSeq([ss("Enter "),dispBlock(B,Off)]).
  dispAssem(iFrame(Tp),Off) => ssSeq([ss("Frame "),disp(Tp)]).
  dispAssem(idBug,Off) => ssSeq([ss("dBug")]).
  dispAssem(idBreak,Off) => ssSeq([ss("dBreak")]).
  dispAssem(iLine(Lc),Off) => ssSeq([ss("iLine "),disp(Lc)]).

  dispBlock(block(Tp,Code),Off) where Off2.=Off++"  "=> ssSeq([ss("{"),disp(Tp),ss("\n"),ss(Off2),
      ssSeq(interleave(Code//(A)=>dispAssem(A,Off2),ss("\n"++Off2))),
      ss("\n"),ss(Off),ss("}")]).

  dispCases(Blocks,Off) => ssSeq([ss("<"),ss("\n"),ss(Off),
      ssSeq(interleave(Blocks//(A)=>dispBlock(A,Off),ss("\n"++Off))),
      ss(">")]).
}
