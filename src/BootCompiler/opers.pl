:- module(opers,[infixOp/4,prefixOp/3,postfixOp/3,isOperator/1,follows/3]).

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
computeTrie([_-oper(Nm,_)|M],Tr,Flws) :-
  addToTrie(Nm,Nm,Tr,Tr1),
  computeTrie(M,Tr1,Flws).
