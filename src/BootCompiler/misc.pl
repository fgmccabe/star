:-module(misc,[concat/3,flatten/2,segment/3,last/2,reverse/2,revconcat/3,is_member/2,add_mem/3,
        merge/3,intersect/3,subtract/3,replace/4,filter/3,
        collect/4,map/3,rfold/4,project0/2,project1/2,zip/3,
        appStr/3,appInt/3,appFlt/3,appSym/3,appQuoted/4,genstr/2,
        subPath/4,pathSuffix/3,starts_with/2,ends_with/2,
        localName/4,
        listShow/5,
        stringHash/3,hashSixtyFour/2,stringEndsWith/2,
        marker/2,
        same/2,
        interleave/3,concatStrings/2,
        quickSort/3,sortedMerge/4]).

same(X,X).

concat([],X,X).
concat([E|X],Y,[E|Z]) :- concat(X,Y,Z).

flatten([],[]).
flatten([F|R],Z):- concat(F,U,Z), flatten(R,U).

reverse(X,Y) :- reverse(X,[],Y).
reverse([],X,X).
reverse([E|R],X,Y) :- reverse(R,[E|X],Y).

revconcat(X,Y,Z) :-
  reverse(X,Y,Z).

segment(Str,Ch,Segments) :- split_string(Str,Ch,"",Segments).

last([El],El).
last([_|Rest],El) :- last(Rest,El).

is_member(X,[X|_]).
is_member(X,[_|Y]) :- is_member(X,Y).

add_mem(X,L,L) :- is_member(X,L),!.
add_mem(X,L,[X|L]).

merge([],X,X).
merge([E|X],Y,Z) :-
  is_member(E,Y),!,
  merge(X,Y,Z).
merge([E|X],Y,Z) :-
  merge(X,[E|Y],Z).

replace([],_,El,[El]).
replace([E|X],E,Nw,[Nw|X]).
replace([E|X],K,Nw,[E|Y]) :-
  replace(X,K,Nw,Y).

subtract(_,[],[]).
subtract(E,[E|O],O).
subtract(E,[X|I],[X|O]) :-
  subtract(E,I,O).

intersect([],_,[]).
intersect([E|X],Y,[E|Z]) :- is_member(E,Y), intersect(X,Y,Z).
intersect([_|X],Y,Z) :- intersect(X,Y,Z).

interleave([],_,[]).
interleave([F|L],S,[F|M]) :-
  mixin(L,S,M).

mixin([],_,[]).
mixin([F|L],S,[S,F|M]) :- mixin(L,S,M).

project0([],[]).
project0([(E,_)|L],[E|K]) :-
  project0(L,K).

project1([],[]).
project1([(_,E)|L],[E|K]) :-
  project1(L,K).

zip([],[],[]).
zip([E1|L1],[E2|L2],[(E1,E2)|L3]) :-
  zip(L1,L2,L3).

filter([],_,[]) :- !.
filter([E|L],F,[E|M]) :-
  call(F,E),!,
  filter(L,F,M).
filter([_|L],F,M) :-
  filter(L,F,M).

concatStrings(L,S) :-
  concStrings(L,S,"").

concStrings([],O,O).
concStrings([S|L],O,F) :-
  string_concat(F,S,FF),
  concStrings(L,O,FF).

collect([],_,[],[]) :- !.
collect([El|More],T,[El|Ok],Not) :-
  call(T,El),!,
  collect(More,T,Ok,Not).
collect([El|More],T,Ok,[El|Not]) :-
  collect(More,T,Ok,Not).

map([],_,[]).
map([E|L],F,[El|R]) :-
  call(F,E,El),
  map(L,F,R).

rfold([],_,S,S).
rfold([E|L],F,S,Sx) :-
  call(F,E,S,S0),!,
  rfold(L,F,S0,Sx).

appStr(Str,O,E) :- string_chars(Str,Chrs), concat(Chrs,E,O).

appQuoted(Str,Qt,O,E) :- appStr(Qt,O,O1), string_chars(Qt,[Q]),string_chars(Str,Chars), quoteConcat(Q,Chars,O1,O2), appStr(Qt,O2,E).

quoteConcat(_,[],O,O).
quoteConcat(Qt,['\"'|More],['\\','\"'|Out],Ox) :- quoteConcat(Qt,More,Out,Ox).
quoteConcat(Qt,[Qt|More],['\\',Qt|Out],Ox) :- quoteConcat(Qt,More,Out,Ox).
quoteConcat(Qt,['\\'|More],['\\','\\'|Out],Ox) :- quoteConcat(Qt,More,Out,Ox).
quoteConcat(Qt,[C|More],[C|Out],Ox) :- quoteConcat(Qt,More,Out,Ox).

appSym(Sym,O,E) :- atom_chars(Sym,Chrs), concat(Chrs,E,O).

appInt(Ix,O,E) :- number_string(Ix,Str), string_chars(Str,Chrs), concat(Chrs,E,O).

appFlt(Dx,O,Ox) :- number_string(Dx,Str), string_chars(Str,Chrs), concat(Chrs,Ox,O).

subPath(Path,Marker,Suffix,Name) :-
  sub_string(Path,_,_,After,Marker),
  (After=0, string_concat(Path,Suffix,Name) ; string_concat(Path,".",P0),string_concat(P0,Suffix,Name)).
subPath(Path,Marker,Suffix,Name) :-
  string_concat(Path,Marker,P0),
  string_concat(P0,Suffix,Name).

ends_with(String,Tail) :-
  string_concat(_,Tail,String).

starts_with(String,Front) :-
  string_concat(Front,_,String).

pathSuffix(String,Marker,Tail) :-
  split_string(String,Marker,"",[_,Local]),
  split_string(Local,".","",Segs),
  last(Segs,Tail).
pathSuffix(String,_,String).

genstr(Prefix,S) :-
  gensym(Prefix,A),
  atom_string(A,S).

stringHash(H,Str,Hx) :-
  string_codes(Str,Codes),
  hashCodes(Codes,H,Hx).

hashCodes([],H,H).
hashCodes([C|More],H0,Hx) :-
  H1 is 47*H0+C,
  hashCodes(More,H1,Hx).

hashSixtyFour(H0,H) :-
  H is H0 mod (1<<63).

localName(_,Glue,Nm,Nm) :-
  sub_string(Nm,_,_,_,Glue),!.
localName(Pkg,Glue,Nm,LclName) :-
  string_concat(Pkg,Glue,T),
  string_concat(T,Nm,LclName).

marker(type,"*").
marker(value,"@").
marker(class,"#").
marker(conTract,"$").

listShow([],_,_,O,O) :-!.
listShow([E|L],C,Sep,O,Ox) :-
  call(C,E,O,O1),
  listShowMore(L,C,Sep,O1,Ox).

listShowMore([],_,_,O,O).
listShowMore([E|L],C,S,O,Ox) :-
  appStr(S,O,O1),
  call(C,E,O1,O2),
  listShowMore(L,C,S,O2,Ox).

stringEndsWith(S,E) :-
  string_concat(_,E,S).

split([],_,_,[],[]).
split([T|L],Cmp,Ix,[T|L1],L2) :-
  call(Cmp,T,Ix),
  split(L,Cmp,Ix,L1,L2).
split([T|L],Cmp,Ix,L1,[T|L2]) :-
  \+ call(Cmp,T,Ix),
  split(L,Cmp,Ix,L1,L2).

quickSort(L,C,S) :- qSort(L,C,S),!.

qSort([],_,[]).
qSort([T],_,[T]).
qSort([T|L],Cmp,S) :-
  split(L,Cmp,T,L1,L2),
  qSort(L1,Cmp,S1),
  qSort(L2,Cmp,S2),
  concat(S1,[T|S2],S).

sortedMerge([],L,_,L).
sortedMerge(L,[],_,L).
sortedMerge([E1|L1],[E2|L2],C,[E1|Lx]) :-
  call(C,E1,E2),!,
  sortedMerge(L1,[E2|L2],C,Lx).
sortedMerge([E1|L1],[E2|L2],C,[E2|Lx]) :-
  \+call(C,E1,E2),!,
  sortedMerge([E1|L1],L2,C,Lx).
