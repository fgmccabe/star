:- module(transUtils,[trCons/3,mergeGoal/4,mergeSeq/4,mergeWhere/4,extraVars/2,thisVar/2,
          lookupVarName/3,lookupFunName/3,lookupPtnName/3,lookupClassName/3,
          definedProgs/2,
          genVar/2,
          pushOpt/3, isOption/2,layerName/2,
          genVars/2]).

:- use_module(misc).
:- use_module(dict).
:- use_module(types).
:- use_module(freshen).
:- use_module(terms).

trCons(Nm,Arity,lbl(Name,Arity)) :-
  integer(Arity),!,
  number_string(Arity,Sz),
  string_concat(Nm,"%",N1),
  string_concat(N1,Sz,Name).
trCons(Nm,Args,lbl(Name,Arity)) :-
  length(Args,Arity),
  number_string(Arity,Sz),
  string_concat(Nm,"%",N1),
  string_concat(N1,Sz,Name).

/*
 * Each element in Layers defines a scope. It is a tuple of the form:
 * lyr(Prefix,Defs:list[(Name,Class),LblRecord,Thvr)
 * Where Prefix is the current prefix
 * Defs is the set of local programs and other names defined in this scope
 * Loc is the file location of the defining label
 * LblRecord is the full form of the label term.
 * Clvr is the variable holding the label
 * Thvr is the variable holding this
 * StreamVr is valid in a grammar body, denotes the stream
 *
 * E.g., in
 * pk{
 *  foo(A){
 *   ...
 *    bar(B){
 *      X = $anon{}
 *   ...
 *    }
 *  }
 * }
 *
 * The inner-most layer will look like:
 *    lyr(pk#foo#bar#anon23,Defs,anon23(Free,bar(B,foo(A))),ThVar)
 * the outermost class layer will look like
 *    lyr(pk#foo,Defs,foo(A),thVar)
 * the package layer will look like
 *    lyr(pk,Defs,'',void)
 */

lookup([],_,_,notInMap).
lookup([lyr(_Prefix,Defns,_Lbl,_ThVr)|_Layers],Nm,Filter,Reslt) :-
  filteredSearch(Defns,Filter,Nm,Reslt),!.
lookup([_|Layers],Nm,Filter,Reslt) :-
  lookup(Layers,Nm,Filter,Reslt).

filteredSearch(Defns,Filter,Nm,Defn) :-
  is_member((Nm,Defn),Defns),
  call(Filter,Defn),!.

lookupVarName(Map,Nm,V) :-
  lookup(Map,Nm,nonType,V).

anyDef(_).

lookupFunName(Map,Nm,V) :-
  lookup(Map,Nm,isFnDef,V).

isFnDef(localFun(_,_,_,_,_)).
isFnDef(moduleFun(_,_,_)).
isFnDef(localClass(_,_,_,_,_)).
isFnDef(moduleCons(_,_,_)).

lookupPtnName(Map,Nm,V) :-
  lookup(Map,Nm,isPtnDef,V).

isPtnDef(localPtn(_,_,_,_,_)).
isPtnDef(modulePtn(_,_,_)).
isPtnDef(localClass(_,_,_,_,_)).
isPtnDef(moduleCons(_,_,_)).

lookupClassName(Map,Nm,V) :-
  lookup(Map,Nm,classDef,V).

classDef(localClass(_,_,_,_,_)).
classDef(moduleCons(_,_,_)).

lookupDefn(Map,Nm,Df) :-
  lookup(Map,Nm,nonType,Df).

nonType(Df) :- Df \= moduleType(_,_,_), Df \= localType(_).

extraVars([lyr(_,_,_,void)|_],[]) :- !.
extraVars([lyr(_,_,_,ThVr)|_],[ThVr]).

thisVar([lyr(_,_,_,ThVr)|_],ThVr) :- ThVr \= void.

definedProgs(Map,Prgs) :-
  definedProgs(Map,[],Prgs).

definedProgs([],Pr,Pr).
definedProgs([lyr(_,Defs,_,_)|Map],Pr,Prx) :-
  definedInDefs(Defs,Pr,Pr0),
  definedProgs(Map,Pr0,Prx).

definedInDefs([],Pr,Pr).
definedInDefs([(Nm,Entry)|Defs],Pr,Prx) :-
  definedP(Nm,Entry,P),!,
  (is_member(P,Pr) -> Pr0=Pr ; Pr0=[P|Pr]),
  definedInDefs(Defs,Pr0,Prx).
definedInDefs([_|Defs],Pr,Prx) :-
  definedInDefs(Defs,Pr,Prx).

definedP(Nm,moduleFun(_,_,Arity),lbl(Nm,Arity)).
definedP(Nm,localFun(_,_,_,_,Arity),lbl(Nm,Arity)).
definedP(Nm,modulePtn(_,_,Arity),lbl(Nm,Arity)).
definedP(Nm,localPtn(_,_,_,_,Arity),lbl(Nm,Arity)).
definedP(Nm,moduleVar(_),lbl(Nm,0)).
definedP(Nm,localVar(_,_,_),lbl(Nm,0)).

mergeGoal(enum("star.core#true"),G,_,G).
mergeGoal(G,enum("star.core#true"),_,G).
mergeGoal(G1,G2,Lc,cnj(Lc,G1,G2)).

mergeWhere(Exp,enum("star.core#true"),_,Exp).
mergeWhere(Exp,G,Lc,whr(Lc,Exp,G)).

mergeSeq(_,L,R,R) :- isUnit(L).
mergeSeq(_,L,R,L) :- isUnit(R).
mergeSeq(Lc,seq(Lc0,L0,R0),R,seq(Lc0,L0,RR)) :-
  mergeSeq(Lc,R0,R,RR).
mergeSeq(Lc,L,R,seq(Lc,L,R)).

pushOpt(Opts,Opt,[Opt|Opts]).

isOption(Opt,Opts) :- is_member(Opt,Opts),!.

layerName([lyr(Nm,_,_,_)|_],Nm).

genVar(Prefix,idnt(V)) :-
  genstr(Prefix,V).

genVars(0,[]).
genVars(K,[V|Rest]) :-
  K>0,
  K1 is K-1,
  genVar("_V",V),
  genVars(K1,Rest).
