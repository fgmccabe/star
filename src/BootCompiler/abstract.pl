:- module(abstract,[locOfAst/2,isAst/1,
		    nary/4,binary/5,unary/4,zeroary/3,apply/4,isApply/4,
		    isUnary/3,isUnary/4,isBinary/5,isBinaryTerm/4,
		    isTernary/5,ternary/6,isParen/2,deParen/2,
		    roundTerm/4,isRound/4,isRoundTerm/3,isRoundTerm/4,
		    isTuple/2,isTuple/3,isRoundTuple/3,roundTuple/3,
		    isTpl/4,mkTpl/4,
		    braceTerm/4,isBrace/4,isBraceTerm/4,isBraceTuple/3,braceTuple/3,
		    isEmptyBrace/1,
		    qbraceTerm/4,isQBrace/3,isQBraceTerm/4,isQBraceTuple/3,qbraceTuple/3,
		    squareTerm/4,isSquare/3,isSquare/4,
		    isSquareTuple/3,squareTuple/3,isSquareTerm/3,isSquareTerm/4,sqUnary/4,
		    isName/2,isName/3,
		    isIden/1,isIden/2,isIden/3,genIden/2,genIden/3,isString/2,
		    isFloat/3,
		    isString/3,isInteger/3,isConsTerm/4, sameTerm/2,
		    explodeString/2]).
:- use_module(operators).
:- use_module(misc).

apply(Lc,Op,Args,app(Lc,Op,Args)).

isApply(app(Lc,Op,Args),Lc,Op,Args).

isTuple(tuple(_,"()",Args),Args).

isTuple(tuple(Lc,"()",Args),Lc,Args).

isTpl(tuple(Lc,Op,Args),Lc,Op,Args).

mkTpl(Lc,Op,Args,tuple(Lc,Op,Args)).

isRoundTuple(tuple(Lc,"()",Args),Lc,Args).

roundTuple(Lc,Args,tuple(Lc,"()",Args)).

roundTerm(Lc,Op,Args,app(Lc,Op,tuple(Lc,"()",Args))).

isRound(app(Lc,Op,A),Lc,Op,Els) :- isRoundTuple(A,_,Els), \+ isKeyOp(Op).

isKeyOp(name(_,Op)) :- isKeyword(Op).

isRoundTerm(app(_,Op,tuple(_,"()",Args)),Op,Args) :- \+isKeyOp(Op).

isRoundTerm(app(Lc,Op,tuple(_,"()",Args)),Lc,Op,Args) :- \+isKeyOp(Op).

binary(Lc,Op,L,R,app(Lc,name(Lc,Op),tuple(Lc,"()",[L,R]))).

isBinary(app(Lc,name(_,Op),tuple(_,"()",[L,R])),Lc,Op,L,R).

isBinaryTerm(app(_,Op,tuple(_,"()",[L,R])),Op,L,R).

zeroary(Lc,Op,app(Lc,name(Lc,Op),tuple(Lc,"()",[]))).

unary(Lc,Op,L,app(Lc,name(Lc,Op),tuple(Lc,"()",[L]))).

isUnary(app(_,name(_,Op),tuple(_,"()",[L])),Op,L).

isUnary(app(Lc,name(_,Op),tuple(_,"()",[L])),Lc,Op,L).

isTernary(app(_,name(_,Op),tuple(_,"()",[L,M,R])),Op,L,M,R).

ternary(Lc,Op,L,M,R,app(Lc,name(Lc,Op),tuple(Lc,"()",[L,M,R]))).

nary(Lc,Op,Args,app(Lc,Op,tuple(Lc,"()",Args))).

braceTerm(Lc,Op,Els,app(Lc,Op,tuple(Lc,"{}",Els))).

isBrace(app(Lc,name(_,Op),tuple(_,"{}",L)),Lc,Op,L) :- \+ isKeyword(Op).

isBraceTerm(app(Lc,Op,tuple(_,"{}",A)),Lc,Op,A) :- \+isKeyOp(Op).

isBraceTuple(tuple(Lc,"{}",L),Lc,L).

isEmptyBrace(tuple(_,"{}",[])).

braceTuple(Lc,L,tuple(Lc,"{}",L)).

qbraceTerm(Lc,Op,Els,app(Lc,Op,tuple(Lc,"{..}",Els))).

isQBrace(app(_,name(_,Op),tuple(_,"{..}",L)),Op,L) :- \+ isKeyword(Op).

isQBraceTerm(app(Lc,Op,tuple(_,"{..}",A)),Lc,Op,A) :- \+isKeyOp(Op).

isQBraceTuple(tuple(Lc,"{..}",L),Lc,L).

qbraceTuple(Lc,Els,tuple(Lc,"{..}",Els)).

squareTerm(Lc,Op,Els,app(Lc,Op,tuple(Lc,"[]",Els))).

sqUnary(Lc,Nm,Arg,app(Lc,name(Lc,Nm),tuple(Lc,"[]",[Arg]))).

isSquare(app(_,name(_,Op),tuple(_,"[]",L)),Op,L).

isSquare(app(Lc,name(_,Op),tuple(_,"[]",L)),Lc,Op,L).

isSquareTerm(app(_,Op,tuple(_,"[]",L)),Op,L) :- \+isKeyOp(Op).

isSquareTerm(app(Lc,Op,tuple(_,"[]",L)),Lc,Op,L) :- \+isKeyOp(Op).

isSquareTuple(tuple(Lc,"[]",L),Lc,L).

squareTuple(Lc,L,tuple(Lc,"[]",L)).

isConsTerm(Trm,Lc,H,T) :-
  isBinary(Trm,Lc,",..",H,T).

isName(name(_,Nm),Nm).

isName(name(Lc,Nm),Lc,Nm).

isIden(N) :- isIden(N,_).

isIden(name(_,Nm),Nm).
isIden(tuple(_,"()",[name(_,Nm)]),Nm).

isIden(name(Lc,Nm),Lc,Nm).
isIden(tuple(Lc,"()",[name(_,Nm)]),Lc,Nm).

genIden(Lc,name(Lc,Id)) :-
  genstr("_N",Id).

genIden(Lc,Pre,name(Lc,Id)) :-
  genstr(Pre,Id).

isString(string(_,St),St).

isString(string(Lc,Txt),Lc,Txt).

isInteger(integer(Lc,Ix),Lc,Ix).

isFloat(float(Lc,Dx),Lc,Dx).

isAst(A) :- locOfAst(A,_).

locOfAst(name(Lc,_),Lc).
locOfAst(integer(Lc,_),Lc).
locOfAst(float(Lc,_),Lc).
locOfAst(string(Lc,_),Lc).
locOfAst(tuple(Lc,_,_),Lc).
locOfAst(app(Lc,_,_),Lc).
locOfAst(void(Lc),Lc).

sameTerm(name(_,Nm),name(_,Nm)).
sameTerm(integer(_,Ix),integer(_,Ix)).
sameTerm(float(_,Dx),float(_,Dx)).
sameTerm(string(_,S),string(_,S)).
sameTerm(tuple(_,T,A),tuple(_,T,B)) :-
  sameTerms(A,B).
sameTerm(app(_,OA,AA),app(_,OB,BA)) :-
  sameTerm(OA,OB),
  sameTerm(AA,BA).

sameTerms([],[]).
sameTerms([A|L1],[B|L2]) :-
  sameTerm(A,B),
  sameTerms(L1,L2).

explodeString(string(Lc,S),tuple(Lc,"[]",Els)) :-
  string_codes(S,C),
  map(C,abstract:makeInt(Lc),Els).

makeInt(Lc,Ix,integer(Lc,Ix)).

deParen(T,I) :-
  isRoundTuple(T,_,[I]),!.
deParen(T,T).

isParen(T,I) :-
  isRoundTuple(T,_,[I]).
