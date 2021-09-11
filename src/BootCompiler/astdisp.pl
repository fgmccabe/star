:-module(astdisp,[dispAst/4,dispAst/1,dispAstTerm/3,ast2String/3]).
:- use_module(abstract).
:- use_module(operators).
:- use_module(misc).
:- use_module(display).
:- use_module(wff).

dispAstTerm(Msg,Term,Pr) :- write(Msg), display(Term,Pr), nl().

dispAst(Term) :- dispAst(Term,2000).
dispAst(Term,Pr) :- dispAst(Term,Pr,Chrs,[]), string_chars(Res,Chrs), writeln(Res).

ast2String(Lc,A,Trm) :-
  dispAst(A,2000,Chrs,[]),
  string_chars(Txt,Chrs),
  mkSSChars(Lc,Txt,Trm).

dispAst(name(_,Nm),_,O,E) :- appStr(Nm,O,E).
dispAst(integer(_,Nm),_,O,E) :- number_chars(Nm,Chrs), concat(Chrs,E,O).
dispAst(float(_,Nm),_,O,E) :- number_chars(Nm,Chrs), concat(Chrs,E,O).
dispAst(chars(_,S),_,O,Ox) :- appStr("0",O,O1),appQuoted(S,"\"",O1,Ox).
dispAst(tuple(_,Nm,A),_,O,E) :- bracket(Nm,Left,Right,Sep,Pr),
    appStr(Left,O,O1),
    writeEls(A,Pr,Sep,O1,O2),
    appStr(Right,O2,E).
dispAst(Trm,_,O,Ox) :-
  isSSTrm(Trm),
  appStr("\"",O,O1),
  deInterpolate(Trm,O1,O2),!,
  appStr("\"",O2,Ox).
dispAst(app(_,name(_,Nm),tuple(_,"()",[A])),Pr,O,E) :-
  prefixOp(Nm,OpPr, RightPr),!,
  openParen(Pr,OpPr,O,O1),
  appStr(Nm,O1,O2),
  appStr(" ",O2,O3),
  dispAst(A,RightPr,O3,O4),
  closeParen(Pr,OpPr,O4,E).
dispAst(app(_,name(_,Nm),tuple(_,"()",[A])),Pr,O,E) :-
  postfixOp(Nm,LeftPr,OpPr),!,
  openParen(Pr,OpPr,O,O1),
  dispAst(A,LeftPr,O1,O2),
  appStr(" ",O2,O3),
  appStr(Nm,O3,O4),
  closeParen(Pr,OpPr,O4,E).
dispAst(app(_,name(_,Nm),tuple(_,"()",[A,B])),Pr,O,E) :-
  infixOp(Nm,LeftPr,OpPr, RightPr),!,
  openParen(Pr,OpPr,O,O1),
  dispAst(A,LeftPr,O1,O2),
  appStr(" ",O2,O3),
  appStr(Nm,O3,O4),
  appStr(" ",O4,O5),
  dispAst(B,RightPr,O5,O6),
  closeParen(Pr,OpPr,O6,E).
dispAst(app(_,Op,A),_,O,E) :- dispAst(Op,0,O,O1), dispAst(A,0,O1,E).

isSSTrm(T) :-
  isUnary(T,_,"chrs_",_).
isSSTrm(T) :-
  isBinary(T,_,"pair_",L,R),
  isSSTrm(L),
  isSSTrm(R).
isSSTrm(T) :-
  isUnary(T,_,"disp",_).
isSSTrm(T) :-
  isBinary(T,_,"frmt",_,_).

deInterpolate(T,O,Ox) :-
  isSSChars(T,_,Txt),
  string_chars(Txt,Chars),
  quoteConcat('\"',Chars,O,Ox).
deInterpolate(T,O,Ox) :-
  isUnary(T,_,"chrs_",Trm),!,
  appStr("#",O,O1),
  dispAst(Trm,1000,O1,Ox).
deInterpolate(T,O,Ox) :-
  isBinary(T,_,"pair_",L,R),
  deInterpolate(L,O,O1),
  deInterpolate(R,O1,Ox).
deInterpolate(T,O,Ox) :-
  isUnary(T,_,"disp",Trm),
  appStr("$",O,O1),
  dispAst(Trm,1000,O1,Ox).
deInterpolate(T,O,Ox) :-
  isBinary(T,_,"frmt",Trm,Fmt),
  isChars(Fmt,FmtTxt),
  appStr("$",O,O1),
  dispAst(Trm,1000,O1,O2),
  appStr(":",O2,O3),
  appStr(FmtTxt,O3,O4),
  appStr(";",O4,Ox).

writeEls([],_,_,O,O) :- !.
writeEls([H|M],Pr,Sep,O,E) :- dispAst(H,Pr,O,O1), writeMoreEls(M,Pr,Sep,O1,E).

writeMoreEls([],_,_,O,O) :- !.
writeMoreEls([H|M],Pr,Sep,O,E) :- appStr(Sep,O,O1), dispAst(H,Pr,O1,O2), writeMoreEls(M,Pr,Sep,O2,E).

openParen(Pr,OpPr,O,E) :- OpPr > Pr,!, appStr("(",O,E).
openParen(_,_,O,O).

closeParen(Pr,OpPr,O,E) :- OpPr > Pr, appStr(")",O,E).
closeParen(_,_,O,O).

ssAst(_,name(_,Nm),id(Nm)) :- !.
ssAst(_,integer(_,Ix),ix(Ix)) :- !.
ssAst(_,float(_,Dx),fx(Dx)) :- !.
ssAst(_,chars(_,S),sq([ss(""""),ss(S),ss("""")])) :- !.
ssAst(_,tuple(_,Nm,A),sq([ss(Left),iv(ss(Sep),AA),ss(Right)])) :-
  bracket(Nm,Left,Right,Sep,Pr),
  map(A,astdisp:ssAst(Pr),AA).
ssAst(Pr,app(_,name(_,Nm),tuple(_,"()",[A])),sq([Par,id(Nm),ss(" "),AA,En])) :-
  prefixOp(Nm,OpPr, RightPr),!,
  openPar(Pr,OpPr,Par),
  ssAst(RightPr,A,AA),
  closePar(Pr,OpPr,En).
ssAst(Pr,app(_,name(_,Nm),tuple(_,"()",[A])),sq([Par,AA,ss(" "),id(Nm),En])) :-
  postfixOp(Nm,LeftPr,OpPr),!,
  openPar(Pr,OpPr,Par),
  ssAst(LeftPr,A,AA),
  closePar(Pr,OpPr,En).
ssAst(Pr,app(_,name(_,Nm),tuple(_,"()",[A,B])),
      sq([Par,AA,ss(" "),id(Nm),ss(" "),BB,En])) :-
  infixOp(Nm,LeftPr,OpPr, RightPr),!,
  openPar(Pr,OpPr,Par),
  ssAst(LeftPr,A,AA),
  ssAst(RightPr,B,BB),
  closePar(Pr,OpPr,En).
ssAst(_,app(_,Op,A),sq([OO,AA])) :-
    ssAst(0,Op,OO),
    ssAst(0,A,AA).

openPar(Pr,OpPr,ss("(")) :- OpPr > Pr,!.
openPar(_,_,ss("")).

closePar(Pr,OpPr,ss(")")) :- OpPr > Pr.
closePar(_,_,ss("")).
