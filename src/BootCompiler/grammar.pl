:- module(grammar,[parse/3]).

:- use_module(operators).
:- use_module(abstract).
:- use_module(location).
:- use_module(errors).
:- use_module(lexer).
:- use_module(misc).
:- use_module(wff).

parse(Tks,T,RTks) :-
    term(Tks,2000,T,Tks1,Lst),!,
    checkTerminator(Lst,Tks1,RTks).

term(Tks,Pr,T,Toks,Lst) :-
    termLeft(Tks,Pr,Left,LftPr,Tks1,LLst),!,
    termRight(Tks1,Pr,LftPr,Left,T,Toks,LLst,Lst).

termLeft([idTok(Id,Lc),rgtTok("()",Lcy)|Toks],_,name(Lc,Id),0,[rgtTok("()",Lcy)|Toks],id).
termLeft([idTok(Id,Lcx)|Tks],Pr,Left,LftPr,Toks,Lst) :-
    prefixOp(Id,LftPr,OpRightPr),
    \+ lookAhead(rgtTok("()",_),Tks),
    LftPr =< Pr, !,
    term(Tks,OpRightPr,Arg,Toks,Lst),
    locOfAst(Arg,Lcy),
    mergeLoc(Lcx,Lcy,Lc),
    unary(Lc,Id,Arg,Left).
termLeft(Tks,_,T,0,Toks,Lst) :- term0(Tks,T,Toks,Lst).

termRight([idTok(Id,_)|Tks],Pr,LPr,Left,T,Toks,_,Lst) :-
  infixOp(Id,InfOpL,InfOpPr,InfOpR), InfOpPr =< Pr, LPr =< InfOpL,
  postfixOp(Id,PostOpL,PostOpPr), PostOpPr =< Pr, LPr =< PostOpL,
  legalNextRight(Tks,InfOpR), !,
  term(Tks,InfOpR, Right, Tks1,LLst),
  locOfAst(Left,Lcx),
  locOfAst(Right,Lcy),
  mergeLoc(Lcx,Lcy,Lc),
  binary(Lc,Id,Left,Right,NewLeft),
  termRight(Tks1,Pr,InfOpPr,NewLeft,T,Toks,LLst,Lst).
termRight([idTok(Id,Lcy)|Tks],Pr,LPr,Left,T,Toks,_,Lst) :-
  postfixOp(Id,PostOpL,PostOpPr), PostOpPr =< Pr, LPr =< PostOpL,
  \+legalNextRight(Tks,PostOpPr),!,
  locOfAst(Left,Lcx),
  mergeLoc(Lcx,Lcy,Lc),
  unary(Lc,Id,Left,NewLeft),
  termRight(Tks,Pr,PostOpPr,NewLeft,T,Toks,id,Lst).
termRight([idTok(Id,_)|Tks],Pr,LPr,Left,T,Toks,_,Lst) :-
  infixOp(Id,InfOpL,InfOpPr,InfOpR), InfOpPr =< Pr, LPr =< InfOpL,
  term(Tks,InfOpR, Right, Tks1,LLst),
  locOfAst(Left,Lcx),
  locOfAst(Right,Lcy),
  mergeLoc(Lcx,Lcy,Lc),
  binary(Lc,Id,Left,Right,NewLeft),
  termRight(Tks1,Pr,InfOpPr,NewLeft,T,Toks,LLst,Lst).
termRight(Toks,_,_,Left,Left,Toks,Lst,Lst).

legalNextRight([idTok(I,_)|_],Pr) :- ( prefixOp(I,PPr,_), PPr=<Pr ; \+ isOperator(I)) , !.
legalNextRight([idQTok(_,_)|_],_).
legalNextRight([lftTok(_,_)|_],_).
legalNextRight([stringTok(_,_)|_],_).
legalNextRight([charTok(_,_)|_],_).
legalNextRight([integerTok(_,_)|_],_).
legalNextRight([bigintTok(_,_)|_],_).
legalNextRight([floatTok(_,_)|_],_).

term0([stringTok(St,Lc)|Toks],Str,Toks,id) :-
  interpolateString(St,Lc,Str).
term0([charTok(Cp,Lc)|Toks],char(Lc,Cp),Toks,id).
term0([integerTok(In,Lc)|Toks],integer(Lc,In),Toks,id).
term0([bigintTok(In,Lc)|Toks],bigint(Lc,In),Toks,id).
term0([floatTok(Fl,Lc)|Toks],float(Lc,Fl),Toks,id).
term0([lftTok("{}",Lc0),rgtTok("{}",Lc2)|Toks],tuple(Lc,"{}",[]),Toks,rbrce) :- !,
  mergeLoc(Lc0,Lc2,Lc).
term0([lftTok("{}",Lcx)|Tks],tuple(Lc,"{}",Seq),Toks,rbrce) :- 
  terms(Tks,rgtTok("{}",_),Tks2,Seq),!,
  checkToken(Tks2,Toks,rgtTok("{}",Lcy),Lcy,"missing close brace, got %s, left brace at %s",[Lcx]),
  mergeLoc(Lcx,Lcy,Lc).
term0([lftTok("{..}",Lc0),rgtTok("{..}",Lc2)|Toks],tuple(Lc,"{}",[]),Toks,rbrce) :-!,
  mergeLoc(Lc0,Lc2,Lc).
term0([lftTok("{..}",Lcx)|Tks],tuple(Lc,"{..}",Seq),Toks,rbrce) :-!,
  terms(Tks,rgtTok("{..}",_),Tks2,Seq),!,
  checkToken(Tks2,Toks,rgtTok("{..}",Lcy),Lcy,"missing close brace, got %s, left brace at %s",[Lcx]),
  mergeLoc(Lcx,Lcy,Lc).
term0([lftTok(Bkt,Lc0),rgtTok(Bkt,Lc2)|Toks],Term,Toks,rbrce) :-
  mergeLoc(Lc0,Lc2,Lc),!,
  emptyBkt(Lc,Bkt,Term).
term0(Tks,T,Toks,Lst) :- term00(Tks,Op,RTks,LLst), termArgs(RTks,Op,T,Toks,LLst,Lst).

emptyBkt(Lc,"[]",tuple(Lc,"[]",[])).
emptyBkt(Lc,"()",tuple(Lc,"()",[])).

term00([idTok(I,Lc)|Toks],name(Lc,I),Toks,id) :-
      (isOperator(I), \+lookAhead(rgtTok("()",_),Toks), !, reportError("unexpected operator: '%s'",[I],Lc);
      true).
term00([idQTok(I,Lc)|Toks],qnme(Lc,I),Toks,id).
term00([lftTok("()",Lc0),rgtTok("()",Lc2)|Toks],tuple(Lc,"()",[]),Toks,rpar) :-
  mergeLoc(Lc0,Lc2,Lc).
term00([lftTok("()",Lcx)|Tks],T,Toks,rpar) :-
  term(Tks,2000,Seq,Tks2,_),
  checkToken(Tks2,Toks,rgtTok("()",Lcy),Lcy,"missing close parenthesis, got %s, left paren at %s",[Lcx]),
  mergeLoc(Lcx,Lcy,Lc),
  tupleize(Seq,Lc,"()",T).
term00([lftTok("[]",Lc0),rgtTok("[]",Lc2)|Toks],tuple(Lc,"[]",[]),Toks,rbra) :-
  mergeLoc(Lc0,Lc2,Lc).
term00([lftTok("[]",Lcx)|Tks],T,Toks,rbra) :-
  term(Tks,2000,Seq,Tks2,_),
  checkToken(Tks2,Toks,rgtTok("[]",Lcy),Lcy,"missing close bracket, got %s, left bracket at %s",[Lcx]),
  mergeLoc(Lcx,Lcy,Lc),
  tupleize(Seq,Lc,"[]",T).
term00([lftTok(Bkt,Lc0),rgtTok(Bkt,Lc2)|Toks],name(Lc,Bkt),Toks,rbra) :-
  mergeLoc(Lc0,Lc2,Lc).
term00([lftTok(Bkt,Lcx)|Tks],T,Toks,rbra) :- % all other brackets go here
  term(Tks,2000,Cont,Tks2,_),
  checkToken(Tks2,Toks,rgtTok(Bkt,Lcy),Lcy,"missing close bracket, got %s, left bracket at %s",[Lcx]),
  mergeLoc(Lcx,Lcy,Lc),
  unary(Lc,Bkt,Cont,T).

termArgs([],T,T,[],Lst,Lst).
termArgs([lftTok("()",_),rgtTok("()",Lcy)|Tks],Op,T,Toks,_,Lst) :-
    locOfAst(Op,Lcx),
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"()",[]),NOP),
    termArgs(Tks,NOP,T,Toks,rpar,Lst).
termArgs([lftTok("()",_)|Tks],Op,T,Toks,_,Lst) :-
    locOfAst(Op,Lcx),
    term(Tks,2000,Seq,Tks2,LLst),
    checkToken(Tks2,Tks3,rgtTok("()",Lcy),Lcy,"missing close parenthesis, got %s, left paren at %s",[Lcx]),
    mergeLoc(Lcx,Lcy,Lc),
    tupleize(Seq,Lc,"()",Args),
    apply(Lc,Op,Args,NOP),
    termArgs(Tks3,NOP,T,Toks,LLst,Lst).
termArgs([lftTok("[]",_),rgtTok("[]",Lcy)|Tks],Op,T,Toks,_,Lst) :-
    locOfAst(Op,Lcx),
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"[]",[]),NOP),
    termArgs(Tks,NOP,T,Toks,rbra,Lst).
termArgs([lftTok("[]",_)|Tks],Op,T,Toks,_,Lst) :-
    locOfAst(Op,Lcx),
    term(Tks,2000,Seq,Tks2,_),
    checkToken(Tks2,Tks3,rgtTok("[]",Lcy),Lcy,"missing close bracket, got %s, left bracket at %s",[Lcx]),
    mergeLoc(Lcx,Lcy,Lc),
    tupleize(Seq,Lc,"[]",Args),
    apply(Lc,Op,Args,NOP),
    termArgs(Tks3,NOP,T,Toks,rbra,Lst).
termArgs([lftTok("{}",_),rgtTok("{}",Lcy)|Tks],Op,T,Tks,_,rbrce) :-
    locOfAst(Op,Lcx),
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"{}",[]),T).
termArgs([lftTok("{}",Lcl)|Tks],Op,T,Toks,_,rbrce) :-
    locOfAst(Op,Lcx),
    terms(Tks,rgtTok("{}",_),Tks2,Seq),
    checkToken(Tks2,Toks,rgtTok("{}",Lcy),Lcy,"missing close brace, got %s, left brace at %s",[Lcl]),
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"{}",Seq),T).
termArgs([lftTok("{..}",_),rgtTok("{..}",Lcy)|Tks],Op,T,Tks,_,rbrce) :-
    locOfAst(Op,Lcx),
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"{..}",[]),T).
termArgs([lftTok("{..}",_)|Tks],Op,T,Toks,_,rbrce) :-
    locOfAst(Op,Lcx),
    terms(Tks,rgtTok("{..}",_),Tks2,Seq),
    checkToken(Tks2,Toks,rgtTok("{..}",Lcy),Lcy,"missing close brace, got %s, left brace at %s",[Lcx]),
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"{..}",Seq),T).
termArgs([idTok(".",_),idTok(Fld,LcF)|Tks],Op,T,Toks,_,Lst) :-
    locOfAst(Op,Lcx),
    mergeLoc(Lcx,LcF,Lc),
    binary(Lc,".",Op,name(LcF,Fld),NOP),
    termArgs(Tks,NOP,T,Toks,id,Lst).
termArgs(Toks,T,T,Toks,Lst,Lst).

terms([],_,[],[]).
terms([Match|Toks],Match,[Match|Toks],[]) :- !.
terms(Tks,Match,Toks,[T|R]) :-
    parse(Tks,T,Tks2),
    terms(Tks2,Match,Toks,R).

tupleize(T,Lc,Op,tuple(Lc,Op,Els)) :-
  deComma(T,Els).

lookAhead(Tk,[Tk|_]).

/* an interpolated string becomes
   _multicat(cons(S1,cons(S2,....nil)))
   if it has multiple segments
*/

interpolateString(Els,Lc,Term) :-
  length(Els,Ln),Ln>1,
  handleInterpolations(Els,Lc,Cons),
  unary(Lc,"_str_multicat",Cons,Term).
interpolateString([],Lc,string(Lc,"")).
interpolateString([El],Lc,Term) :-
  handleInterpolation(El,Lc,Term).

handleInterpolations([],Lc,Nil) :-
  mkEnum(Lc,"nil",Nil).
handleInterpolations([El|Els],Lc,Cons) :-
  handleInterpolation(El,Lc,H),
  handleInterpolations(Els,Lc,T),
  binary(Lc,"cons",H,T,Cons).

handleInterpolation(segment(Str,Lc),_,string(Lc,Str)) :-!.
handleInterpolation(interpolate(Text,"",Lc),_,Disp) :-
  subTokenize(Lc,Text,Toks),
  term(Toks,2000,Term,TksX,_),
  unary(Lc,"disp",Term,Disp),
  ( TksX = [] ; lookAhead(ATk,TksX),locOf(ATk,ALc),reportError("extra tokens in string interpolation",[],ALc)).
handleInterpolation(interpolate(Text,Fmt,Lc),_,Disp) :-
  subTokenize(Lc,Text,Toks),
  term(Toks,2000,Term,TksX,_),
  binary(Lc,"frmt",Term,string(Lc,Fmt),Disp),
  ( TksX = [] ; lookAhead(ATk,TksX),locOf(ATk,ALc),reportError("extra tokens in string interpolation",[],ALc)).
handleInterpolation(coerce(Text,Lc),_,Disp) :-
  subTokenize(Lc,Text,Toks),
  term(Toks,2000,Disp,TksX,_),
  ( TksX = [] ; lookAhead(ATk,TksX),locOf(ATk,ALc),reportError("extra tokens in string interpolation",[],ALc)).
  
checkToken([Tk|Toks],Toks,Tk,_,_,_) :- !.
checkToken([Tk|Toks],Toks,_,Lc,Msg,Extra) :- locOfToken(Tk,Lc), reportError(Msg,[Tk|Extra],Lc).
checkToken([],[],Tk,_,Msg,Extra) :- reportError(Msg,[Tk|Extra],missing).

checkTerminator(_,[],[]).
checkTerminator(_,Toks,Toks) :- Toks = [rgtTok("{}",_)|_].
checkTerminator(_,Toks,Toks) :- Toks = [rgtTok("{..}",_)|_].
checkTerminator(_,[termTok(_)|Toks],Toks) .
checkTerminator(rbrce,Toks,Toks).
checkTerminator(_,[Tk|Tks],RTks) :-
  locOfToken(Tk,Lc),
  reportError("missing terminator, got %s",[Tk],Lc),
  scanForTerminator(Tks,RTks).

scanForTerminator([termTok(_)|Tks],Tks).
scanForTerminator([],[]).
scanForTerminator([_|Tk],Tks) :-
  scanForTerminator(Tk,Tks).
