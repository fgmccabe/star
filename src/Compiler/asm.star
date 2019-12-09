star.compiler.asm{
  import star.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.

  public codeSegment ::= method(termLbl,tipe,codeBlock) |
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
      idLine(locn) |
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

  dispCode(method(Lbl,Tp,Block),Off) => ssSeq([ss(Off),disp(Lbl),ss(":"),disp(Tp),ss("\n"),dispBlock(Block,Off++"  ")]).

  dispAssem:(assemOp,string) => ss.
  dispAssem(iHalt,Off) => ssSeq([ss(Off),ss("Halt\n")]).
  dispAssem(iCall(Lbl),Off) => ssSeq([ss(Off),ss("Call "),disp(Lbl),ss("\n")]).
  dispAssem(iOCall(Lcl),Off) => ssSeq([ss(Off),ss("OCall "),disp(Lcl),ss("\n")]).
  dispAssem(iEscape(Nm),Off) => ssSeq([ss(Off),ss("Escape "),disp(Nm),ss("\n")]).
  dispAssem(iTail(Lbl),Off) => ssSeq([ss(Off),ss("Tail "),disp(Lbl),ss("\n")]).
  dispAssem(iOTail(Ar),Off) => ssSeq([ss(Off),ss("Tail "),disp(Ar),ss("\n")]).
  dispAssem(iRet,Off) => ssSeq([ss(Off),ss("Ret"),ss("\n")]).
  dispAssem(iDrop,Off) => ssSeq([ss(Off),ss("Drop"),ss("\n")]).
  dispAssem(iDup,Off) => ssSeq([ss(Off),ss("Dup"),ss("\n")]).
  dispAssem(iRst(Lvl),Off) => ssSeq([ss(Off),ss("Rst "),disp(Lvl),ss("\n")]).
  dispAssem(iLdG(Nm),Off) => ssSeq([ss(Off),ss("LdG "),disp(Nm),ss("\n")]).
  dispAssem(iLdC(T),Off) => ssSeq([ss(Off),ss("LdC "),disp(T),ss("\n")]).
  dispAssem(iLdA(V),Off) => ssSeq([ss(Off),ss("LdA "),disp(V),ss("\n")]).
  dispAssem(iLdL(V),Off) => ssSeq([ss(Off),ss("LdL "),disp(V),ss("\n")]).
  dispAssem(iStL(V),Off) => ssSeq([ss(Off),ss("StL "),disp(V),ss("\n")]).
  dispAssem(iTL(V),Off) => ssSeq([ss(Off),ss("TL "),disp(V),ss("\n")]).
  dispAssem(iCLbl(T),Off) => ssSeq([ss(Off),ss("CLbl "),disp(T),ss("\n")]).
  dispAssem(iNth(O),Off) => ssSeq([ss(Off),ss("Nth "),disp(O),ss("\n")]).
  dispAssem(iStNth(O),Off) => ssSeq([ss(Off),ss("StNth "),disp(O),ss("\n")]).
  dispAssem(iGetFld(Nm),Off) => ssSeq([ss(Off),ss("GetFld "),disp(Nm),ss("\n")]).
  dispAssem(iGet(T),Off) => ssSeq([ss(Off),ss("Get "),disp(T),ss("\n")]).
  dispAssem(iSet(T),Off) => ssSeq([ss(Off),ss("Set "),disp(T),ss("\n")]).
  dispAssem(iCase(Cases,Deflt),Off) => ssSeq([ss(Off),ss("Case "),ss("{\n"),dispCases(Cases,Off++"  "),
      ss("\n"),ss(Off),ss("} else "),dispBlock(Deflt,Off++" ")]).
  dispAssem(iAlloc(Lbl),Off) => ssSeq([ss(Off),ss("Alloc "),disp(Lbl),ss("\n")]).
  dispAssem(iCmp(T),Off) => ssSeq([ss(Off),ss("Cmp "),disp(T),ss("\n")]).
  dispAssem(iIf(Th,El),Off) => ssSeq([ss(Off),ss("If\n"),
      dispBlock(Th,Off++"  "),
      ss(Off),ss(" else"),
      dispBlock(El,Off++"  "),
      ss("\n")]).
  dispAssem(iEnter(B),Off) => ssSeq([ss(Off),ss("Enter "),dispBlock(B,Off++"  "),ss("\n")]).
  dispAssem(iFrame(Tp),Off) => ssSeq([ss(Off),ss("Frame "),disp(Tp),ss("\n")]).
  dispAssem(idBug,Off) => ssSeq([ss(Off),ss("dBug"),ss("\n")]).
  dispAssem(idBreak,Off) => ssSeq([ss(Off),ss("dBreak"),ss("\n")]).
  dispAssem(iLine(Lc),Off) => ssSeq([ss(Off),ss("iLine "),disp(Lc),ss("\n")]).

  dispBlock(block(Tp,Code),Off) => ssSeq([ss("{"),disp(Tp),ss("\n"),
      ssSeq(interleave(Code//(A)=>dispAssem(A,Off),ss("\n"))),
      ss("}")]).

  dispCases(Blocks,Off) => ssSeq([ss("<"),ss("\n"),
      ssSeq(interleave(Blocks//(A)=>dispBlock(A,Off),ss("\n"))),
      ss(">")]).
}
