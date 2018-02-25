/* Automatically generated, do not edit */

:- module(assemble,[assem/2]).
:- use_module(misc).
:- use_module(terms).
:- use_module(encode).

assem([method(Nm,Sig)|Ins],MTpl) :-
    genLblTbl(Ins,0,[],Lbs),
    mnem(Ins,Lbs,[],Lts,[],Lcs,0,Cde),
    mkInsTpl(Cde,Code),
    mkLitTpl(Lts,LtTpl),
    mkTpl(Lcs,LcsTpl),
    mkTpl([Nm,strg(Sig),Code,LtTpl,LcsTpl],MTpl).

mnem([],_,Lt,Lt,Lc,Lc,_,[]).
mnem([iLbl(_)|Ins],Lbs,Lt,Lts,Lc,Lcx,Pc,Code) :- mnem(Ins,Lbs,Lt,Lts,Lc,Lcx,Pc,Code).
mnem([iLocal(Nm,Frm,End,Off)|Ins],Lbs,Lt,Lts,Lc,Lcx,Pc,Code) :-
    findLbl(Frm,Lbs,F),
    findLbl(End,Lbs,T),
    mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)],Entry),
    mnem(Ins,Lbs,Lt,Lts,[Entry|Lc],Lcx,Pc,Code).
mnem([iHalt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[0|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(1,LtNo)|M]) :- Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iOCall|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[2|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iEscape(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(3,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iTail(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(4,LtNo)|M]) :- Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iOTail|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[5|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iEnter(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(6,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iRet|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[7|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iJmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(8,Off)|M]) :- Pc1 is Pc+1,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iDrop|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[9|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iDup|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[10|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iPull(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(11,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iRot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(12,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iLdC(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(13,LtNo)|M]) :- Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iLdA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(14,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iLdL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(15,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iStL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(16,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iTL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(17,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iStA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(18,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iCLbl(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(19,Off)|M]) :- Pc1 is Pc+1,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(20,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iStNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(21,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iCase(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(22,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iAlloc(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(23,LtNo)|M]) :- Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(24,Off)|M]) :- Pc1 is Pc+1,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iBf(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(25,Off)|M]) :- Pc1 is Pc+1,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iBt(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(26,Off)|M]) :- Pc1 is Pc+1,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iCas(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(27,Off)|M]) :- Pc1 is Pc+1,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iRais(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(28,LtNo)|M]) :- Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iFrame(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(29,V)|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iLine(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[(30,LtNo)|M]) :- Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).

genLblTbl([],_,Lbls,Lbls).
genLblTbl([iLbl(Lbl)|Ins],Pc,Lbls,Lbx) :- genLblTbl(Ins,Pc,[(Lbl,Pc)|Lbls],Lbx).
genLblTbl([_|Ins],Pc,Lb,Lbx) :- Pc1 is Pc+1, genLblTbl(Ins,Pc1,Lb,Lbx).

findLbl(L,Lbs,Tgt) :- is_member((L,Tgt),Lbs),!.

pcGap(Pc,Tgt,Off) :- Off is Tgt-Pc-1.

findLit(Lits,V,LtNo,Lits) :- is_member((V,LtNo),Lits),!.
findLit(Lits,V,LtNo,[(V,LtNo)|Lits]) :- length(Lits,LtNo).

mkLitTpl(Lits,Tpl) :-
    reverse(Lits,RLit),
    project0(RLit,Els),
    mkTpl(Els,Tpl).

mkInsTpl(Is,Tpl) :-
    map(Is,assemble:mkIns,Ins),
    mkTpl(Ins,Tpl).

mkIns((O,A),Tpl) :-
    wrap(A,WA),
    mkTpl([intgr(O),WA],Tpl).
mkIns(O,intgr(O)) :- number(O).

wrap(O,intgr(O)) :- number(O).
wrap(S,strg(S)) :- string(S).

