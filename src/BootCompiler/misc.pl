:-module(misc,[concat/3,flatten/2,segment/3,last/2,reverse/2,revconcat/3,part2/3,
	       is_member/2,add_mem/3,one_of/1,
	       merge/3,intersect/3,subtract/3,replace/4,filter/3,front/3,
	       collect/4,map/3,lfold/4,rfold/4,
	       project0/2,project1/2,project0_3/2,project1_3/2,project2_3/2,
	       zip/3,split_list/4,index_list/3,
	       isIdentifier/1,
	       appChr/3,appStr/3,appStrs/3,appIden/3,appInt/3,appFlt/3,quoteConcat/4,
	       appSym/3,appQuoted/4,
	       appIndx/3,appNl/2,appNwln/3,appMulti/3,
	       genstr/2,str_lt/2,chr_lt/2,
	       subPath/4,pathSuffix/3,starts_with/2,ends_with/2,
	       mangleName/3,mangleName/4,splitLocalName/4,getLocalName/2,localName/3,
	       listShow/5,
	       charHash/3,stringHash/3,hashSixtyFour/2,stringEndsWith/2,
	       marker/2,
	       packageVarName/3,contractName/3,
	       thetaName/3,consName/3,packageTypeName/3,genNewName/3,
	       lambdaLbl/3,
	       same/2,
	       interleave/3,concatStrings/2,
	       sort/3,sortedMerge/4,makeKey/2,
	       nextPrime/2,sieve/2,
	       check_implies/2,verify/2,bin_nop/2,
	       clear_track/0,track/2]).

:- use_module(errors).

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

part2([],[],[]) :-!.
part2([A],[A],[]) :-!.
part2(Ls,L,R) :-
  length(Ls,Ln),
  Ln2 is Ln//2,
  front(Ls,Ln2,L,R).
  

segment(Str,Ch,Segments) :- split_string(Str,Ch,"",Segments).

last([El],El).
last([_|Rest],El) :- last(Rest,El).

is_member(X,[X|_]).
is_member(X,[_|Y]) :- is_member(X,Y).

bin_nop(_,_).

add_mem(X,L,L) :- is_member(X,L),!.
add_mem(X,L,[X|L]).

one_of(C) :- call(C),!.

merge([],X,X).
merge([E|X],Y,Z) :-
  is_member(E,Y),!,
  merge(X,Y,Z).
merge([E|X],Y,Z) :-
  merge(X,[E|Y],Z).

front([],_,[]) :-!.
front(_,0,[]) :-!.
front([H|T],Dp,[H|Rs]) :-
  Dp1 is Dp-1,
  front(T,Dp1,Rs).

front([],_,[],[]).
front(Ls,0,[],Ls).
front([H|T],Cnt,[H|Ts],Ls) :-
  Cn is Cnt-1,
  front(T,Cn,Ts,Ls).

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

project1_3([],[]).
project1_3([(_,E,_)|L],[E|K]) :-
  project1_3(L,K).

project0_3([],[]).
project0_3([(E,_,_)|L],[E|K]) :-
  project0_3(L,K).

project2_3([],[]).
project2_3([(_,_,E)|L],[E|K]) :-
  project2_3(L,K).

zip([],[],[]).
zip([E1|L1],[E2|L2],[(E1,E2)|L3]) :-
  zip(L1,L2,L3).

index_list([],_,[]) :-!.
index_list([E|L],Ix,[(E,Ix)|XL]) :-
  Ix1 is Ix+1,
  index_list(L,Ix1,XL).

split_list(0,L,[],L) :-!.
split_list(Ix,[H|I],[H|R],O) :-
  Ix>0,
  Ix1 is Ix-1,
  split_list(Ix1,I,R,O).

filter([],_,[]) :- !.
filter([E|L],F,[E|M]) :-
  call(F,E),!,
  filter(L,F,M).
filter([_|L],F,M) :-
  filter(L,F,M).

check_implies(X,Y) :-
  \+((call(X),\+call(Y))).

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
  call(F,E,El),!,
  map(L,F,R).

rfold([],_,S,S).
rfold([E|L],F,S,Sx) :-
  call(F,E,S,S0),!,
  rfold(L,F,S0,Sx).

lfold([],_,S,S).
lfold([E|L],F,S,Sx) :-
  lfold(L,F,S,S0),
  call(F,E,S0,Sx),!.

appStr(Str,O,E) :- string_chars(Str,Chrs), concat(Chrs,E,O).

appChr(Cp,[Cp|O],O).

appStrs([],O,O).
appStrs([S|L],O,Ox) :- appStr(S,O,O1), appStrs(L,O1,Ox).

appIden(Nm,O,Ox) :-
  isIdentifier(Nm) ->
    appStr(Nm,O,Ox);
    appQuoted(Nm,"'",O,Ox).

isIdentifier(Nm) :- string_chars(Nm,Chrs), check_implies(is_member(Ch,Chrs),isAlphaNum(Ch)).

isAlphaNum(Ch) :- char_type(Ch,alpha) ; char_type(Ch,alnum) ; Ch='_'.

appQuoted(Str,Qt,O,E) :-
  appStr(Qt,O,O1),
  string_chars(Qt,[Q]),
  string_chars(Str,Chars),
  quoteConcat(Q,Chars,O1,O2),!,
  appStr(Qt,O2,E).

quoteConcat(_,[],O,O).
quoteConcat(Qt,['\"'|More],['\\','\"'|Out],Ox) :- quoteConcat(Qt,More,Out,Ox).
quoteConcat(Qt,[Qt|More],['\\',Qt|Out],Ox) :- quoteConcat(Qt,More,Out,Ox).
quoteConcat(Qt,['\\'|More],['\\','\\'|Out],Ox) :- quoteConcat(Qt,More,Out,Ox).
quoteConcat(Qt,[C|More],[C|Out],Ox) :- quoteConcat(Qt,More,Out,Ox).

appSym(Sym,O,E) :- atom_chars(Sym,Chrs), concat(Chrs,E,O).

appInt(Ix,O,E) :- number_string(Ix,Str), string_chars(Str,Chrs), concat(Chrs,E,O).

appFlt(Dx,O,Ox) :- number_string(Dx,Str), string_chars(Str,Chrs), concat(Chrs,Ox,O).

appIndx(0,O,O) :- !.
appIndx(N,[' '|O],Ox) :-
  N1 is N-1,
  appIndx(N1,O,Ox).

appNl(['\n'|Ox],Ox).

appNwln(Dp,O,Ox) :-
  appNl(O,O1),
  appIndx(Dp,O1,Ox).

appMulti([],Ox,Ox) :- !.
appMulti([C|L],O,Ox) :-
  call(C,O,O1),
  appMulti(L,O1,Ox).

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

chr_lt(CP1,CP2) :-
  atom_codes(CP1,C1),
  atom_codes(CP2,C2),
  codeSmaller(C1,C2),!.

str_lt(S1,S2) :-
     string_codes(S1,C1),
     string_codes(S2,C2),
     codeSmaller(C1,C2),!.

codeSmaller([],[_|_]).
codeSmaller([C|_],[D|_]) :-
     C<D.
codeSmaller([C|L],[C|M]) :-
     codeSmaller(L,M).

stringHash(H,Str,Hx) :-
  string_codes(Str,Codes),
  hashCodes(Codes,H,Hs),
  hashSixtyFour(Hs,Hx).

charHash(H,Cp,Hx) :-
  atom_codes(Cp,Codes),
  hashCodes(Codes,H,Hs),
  hashSixtyFour(Hs,Hx).

hashCodes([],H,H).
hashCodes([C|More],H0,Hx) :-
  H1 is 37*H0+C,
  hashCodes(More,H1,Hx).

hashSixtyFour(H0,H) :-
  H is H0 /\ 9223372036854775807.

mangleName(Pkg,Mrk,Nm,LclName) :-
  marker(Mrk,Glue),!,
  string_concat(Pkg,Glue,T),
  string_concat(T,Nm,LclName).

mangleName(Pref,Mrk,Str) :-
  mangleName(Pref,Mrk,"",Str).

splitLocalName(LclNm,Glue,Pkg,Nm) :-
  sub_string(LclNm,Before,_,After,Glue),
  sub_string(LclNm,0,Before,_,Pkg),
  sub_string(LclNm,_,After,0,Nm),!.

localName(Nm,Tp,Lcl) :-
  marker(Tp,Mrk),!,
  splitLocalName(Nm,Mrk,_,Lcl).

getLocalName(Lcl,Nm) :-
  marker(_,Mrk),
  splitLocalName(Lcl,Mrk,_,Nm),!.
getLocalName(Nm,Nm).

marker(type,"*").
marker(value,"@").
marker(class,"#").
marker(conTract,"$").
marker(over,"!").
marker(package,"#").
marker(field,"°").
marker(closure,"^").

packageVarName(Pkg,Nm,LclName) :-
  mangleName(Pkg,package,Nm,LclName).

thetaName(Path,Nm,LclName) :-
  genNewName(Path,"θ",ThPath),
  mangleName(ThPath,value,Nm,LclName).

lambdaLbl(Prefix,Variant,Nm) :-
  genstr(Variant,V),
  mangleName(Prefix,value,V,Nm).

consName(Path,Nm,ConsNm) :-
  mangleName(Path,class,Nm,ConsNm).

contractName(Path,Nm,ConNm) :-
  marker(conTract,Marker),
  subPath(Path,Marker,Nm,ConNm).

genNewName(Path,Prfx,Name) :-
  genstr(Prfx,Pre),
  mangleName(Path,value,Pre,Name).

packageTypeName(Pkg,Nm,LclName) :-
  mangleName(Pkg,type,Nm,LclName).

listShow([],_,_,O,O) :-!.
listShow([E|L],C,Sep,O,Ox) :-
  call(C,E,O,O1),
  listShowMore(L,C,Sep,O1,Ox).

listShowMore([],_,_,O,O).
listShowMore([E|L],C,S,O,Ox) :-
  call(S,O,O1),
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

makeKey(Id,Key) :-
  atom_string(Key,Id).

sort(L,C,S) :- qSort(L,C,S),!.

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

somePrimes([2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,
  101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,
  197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,
  311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,
  431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,
  557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,
  661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,
  809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,
  937,941,947,953,967,971,977,983,991,997]).

nextPrime(X,Pr) :-
  somePrimes(Prs),
  is_member(Pr,Prs),
  Pr>=X.

sieve([],[]).
sieve([C|I],[C|Pr]) :-
  filterDups(C,I,Nx),
  sieve(Nx,Pr).

filterDups(C,Ps,FPs) :-
  filter(Ps,misc:isntDup(C),FPs).

isntDup(C,N) :-
  N mod C =\= 0.

iota(C,Mx,_,[]) :-
  C>Mx,!.
iota(C,Mx,St,[C|I]) :-
  C1 is C+St,
  iota(C1,Mx,St,I).

verify(C,_) :-
  call(C),!.
verify(C,M) :-
  writef("assertion %w failed: %w\n",[C,M]),
  abort.

clear_track :-
 retractall(trk(_)).

track(Nm,Lc) :-
  clause(trk(Nm),_),!,
  reportMsg("already seen %s",[Nm],Lc),
  abort.
track(Nm,Lc) :-
  reportMsg("track %s",[Nm],Lc),
  assert(trk(Nm)).
