:- module(trie, [emptyTrie/1, addToTrie/4, findInTrie/3, foldTrie/4, outTrie/4, dispTrie/2]).

:- use_module(misc).

emptyTrie(trie(void,false,follows{})).

addToTrie(Ky,Value,Tr,Trx) :-
  string_chars(Ky,Chrs),
  add2Tr(Chrs,Value,Tr,Trx).

add2Tr([],Value,trie(_,_,Fl),trie(Value,true,Fl)).
add2Tr([Ch|Rest],Value,trie(Vl,End,Fl),trie(Vl,End,Fl2)) :-
  (get_dict(Ch,Fl,Fls) -> add2Tr(Rest,Value,Fls,F1),put_dict(Ch,Fl,F1,Fl2) ;
   emptyTrie(E),add2Tr(Rest,Value,E,E2), put_dict(Ch,Fl,E2,Fl2)).

findInTrie(Ky,Tr,Value) :-
  string_chars(Ky,Chrs),
  findTrie(Chrs,Tr,Value).

findTrie([],trie(Value,true,_),Value).
findTrie([Ch|Chrs],trie(_,_,Fl),Value) :-
  get_dict(Ch,Fl,Tr),!,
  findTrie(Chrs,Tr,Value).

foldTrie(Tr,Pr,Z,Zx) :-
  fldTrie([],Tr,Pr,Z,Zx).

fldTrie(Prefix,trie(Value,true,Fls),Proc,Z,Zx) :-
  reverse(Prefix,Chrs),
  string_chars(Ky,Chrs),
  call(Proc,Ky,Value,Z,Z1),
  dict_pairs(Fls,_,Pairs),
  rfold(Pairs,trie:procPair(Prefix,Proc),Z1,Zx).
fldTrie(Prefix,trie(_,false,Fls),Proc,Z,Zx) :-
  procTbl(Prefix,Fls,Proc,Z,Zx).

procPair(Prefix,Proc,Ch-Tb,Z,Zx) :-
  fldTrie([Ch|Prefix],Tb,Proc,Z,Zx).

outTrie(Tr,Vp,O,Ox) :-
  appStr("{",O,O1),
  foldTrie(Tr,trie:outEntry(Vp),O1,O2),
  appStr("}",O2,Ox).

outEntry(Vp,Ky,Vl,O,Ox) :-
  appId(Ky,O,O1),
  appStr("-",O1,O2),
  call(Vp,Vl,O2,O3),
  appStr(",",O3,Ox).

dispTrie(Tr,Vp) :-
  outTrie(Tr,Vp,Chrs,[]),
  string_chars(Txt,Chrs),
  writeln(Txt).

walkTrie(Tr,Pr,Z,Zx) :- walkTr([],Tr,Pr,Z,Zx).

walkTr(Prefix,trie(Value,Final,Fls),Proc,Z,Zx) :-
  call(Proc,Prefix,Value,Final,Z,Z1),
  dict_pairs(Fls,_,Pairs),
  rfold(Pairs,trie:walkPair(Prefix,Proc),Z1,Zx).

walkPair(Prefix,Proc,Ch-Inner,Z,Zx) :-
  walkTr([Ch|Prefix],Inner,Proc,Z,Zx).
