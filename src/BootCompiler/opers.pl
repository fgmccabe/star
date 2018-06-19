:- module(opers,[parse_operators/2,infixOp/5,prefixOp/4,postfixOp/4,isOperator/2,follows/4,final/3]).

:- use_module(json).
:- use_module(trie).
:- use_module(uri).
:- use_module(misc).
:- use_module(resource).

parse_operators(Path,opTbl(Opers,Follows)) :-
  readFile(Path,Chars),
  phrase(parseJson(J),Chars),
  jsonOperators(J,Opers),
  dict_pairs(Opers,_,Prs),
  emptyTrie(ETr),
  computeTrie(Prs,ETr,Trie),
  computeFollows(Trie,Follows).

jsonOperators(jSeq(Entries),Opers) :-
  jsonOps(Entries,opers{},Opers).

jsonOps([],Ops,Ops) :- !.
jsonOps([jColl(Op)|More],Ops,Opsx) :-
  jsonOp(Op,void,void,void,Ops,Ops1),
  jsonOps(More,Ops1,Opsx).

jsonOp([],Nm,Style,Priorities,Ops,Opsx) :-
  defineOperator(Nm,Style,Priorities,Ops,Opsx),!.
jsonOp([("infixOp",jTxt(Op))|More],_,_,Priorities,Ops,Opsx) :-
  jsonOp(More,Op,infixOp,Priorities,Ops,Opsx).
jsonOp([("prefixOp",jTxt(Op))|More],_,_,Priorities,Ops,Opsx) :-
  jsonOp(More,Op,prefixOp,Priorities,Ops,Opsx).
jsonOp([("postfixOp",jTxt(Op))|More],_,_,Priorities,Ops,Opsx) :-
  jsonOp(More,Op,postfixOp,Priorities,Ops,Opsx).
jsonOp([("priorities",jSeq(Prs))|More],Nm,Style,_,Ops,Opsx) :-
  jsonPriorities(Prs,Priorities),
  jsonOp(More,Nm,Style,Priorities,Ops,Opsx).
jsonOp([("desc",_)|More],Nm,Style,Priorities,Ops,Opsx) :-
  jsonOp(More,Nm,Style,Priorities,Ops,Opsx).
jsonOp([("token",_)|_],_Nm,_Style,_Priorities,Ops,Ops).

jsonPriorities([],[]).
jsonPriorities([jNum(Nm)|More],[Nm|MM]) :-
  jsonPriorities(More,MM).

defineOperator(Nm,Style,Pri,Ops,Opsx) :-
  makeKey(Nm,Ky),
  (get_dict(Ky,Ops,E) -> mergeEntry(E,Style,Pri,E1), put_dict(Ky,Ops,E1,Opsx);
   put_dict(Ky,Ops,oper(Nm,[(Style,Pri)]),Opsx)).

mergeEntry(oper(Nm,Stls),Style,Pri,oper(Nm,NStls)) :-
  replace(Stls,(Style,_),(Style,Pri),NStls).

makeKey(Id,Key) :-
  atom_string(Key,Id).

computeTrie([],Tr,Tr).
computeTrie([_-oper(Nm,_)|M],Tr,Trx) :-
  addToTrie(Nm,Nm,Tr,Tr1),
  computeTrie(M,Tr1,Trx).

computeFollows(Tr,Follows) :-
  walkTrie(Tr,opers:followProc,follows{},Follows).

followProc([],_,_,F,F).
followProc([Ch|Rest],_,Final,F,Fx) :-
  reverse(Rest,Rst),
  atom_chars(Str,Rst),
  reverse([Ch|Rest],Fl),
  atom_chars(Nm,Fl),
  addEntry(Str,Ch,Nm,Final,F,Fx).

addEntry(Prefix,Ch,Nm,Final,F,Fx) :-
  get_dict(Prefix,F,next(Fnl,Sub)),!,
  put_dict(Ch,Sub,Nm,S1),
  put_dict(Prefix,F,next(Fnl,S1),F1),
  markFinal(Nm,Final,F1,Fx).
addEntry(Prefix,Ch,Nm,Final,F,Fx) :-
  put_dict(Ch,nxt{},Nm,S0),
  put_dict(Prefix,F,next(none,S0),F1),
  markFinal(Nm,Final,F1,Fx).

markFinal(Nm,true,F,Fx) :-
  atom_string(Nm,SNm),
  (get_dict(Nm,F,next(_,Sub))->
    put_dict(Nm,F,next(final(SNm),Sub),Fx);
    put_dict(Nm,F,next(final(SNm),nxt{}),Fx)).
markFinal(_,false,F,F).

operator(opTbl(Ops,_),Nm,Styles) :-
  atom_string(Nm,Ky),
  get_dict(Ky,Ops,oper(_,Styles)).

isOperator(Ops,Op) :-
  operator(Ops,Op,_),!.

prefixOp(Ops,Op,P,R) :-
  operator(Ops,Op,Styles),!,
  is_member((prefixOp,[P,R]),Styles),!.

infixOp(Ops,Op,L,P,R) :-
  operator(Ops,Op,Styles),
  is_member((infixOp,[L,P,R]),Styles),!.

postfixOp(Ops,Op,L,P) :-
  operator(Ops,Op,Styles),
  is_member((postfixOp,[P,L]),Styles),!.

follows(opTbl(_,Fls),Pr,Ch,Nx) :-
  get_dict(Pr,Fls,next(_,FF)),
  get_dict(Ch,FF,Nx).

final(opTbl(_,Fls),Ky,Nm) :-
  get_dict(Ky,Fls,next(final(Nm),_)),!.
