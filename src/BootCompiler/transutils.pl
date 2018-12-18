:- module(transutils,[trCons/3,mergeGoal/4,mergeSeq/4,mergeWhere/4,extraVars/2,thisVar/2,
          lookupVarName/3,lookupFunName/3,lookupThetaVar/3,lookupClassName/3,
          definedProgs/2,labelVars/2,
          genVar/2,
          pushOpt/3, isOption/2,layerName/2,dispMap/2,showMap/3,
          genVars/2,
          pullWhere/4,pullWheres/4]).

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

lookupThetaVar(Map,Nm,V) :-
  lookup(Map,Nm,transutils:getThetaVar(V),R), R\=notInMap.

getThetaVar(ThVr,localFun(_,_,_,_,ThVr)).
getThetaVar(ThVr,localClass(_,_,_,_,ThVr)).
getThetaVar(ThVr,labelArg(_,_,ThVr)).

lookupFunName(Map,Nm,V) :-
  lookup(Map,Nm,isFnDef,V).

isFnDef(localFun(_,_,_,_,_)).
isFnDef(moduleFun(_,_,_)).
isFnDef(localClass(_,_,_,_,_)).
isFnDef(moduleCons(_,_,_)).

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
  definedP(Nm,Entry),!,
  (is_member(idnt(Nm),Pr) -> Pr0=Pr ; Pr0=[idnt(Nm)|Pr]),
  definedInDefs(Defs,Pr0,Prx).
definedInDefs([_|Defs],Pr,Prx) :-
  definedInDefs(Defs,Pr,Prx).

definedP(_Nm,moduleFun(_,_,_)).
definedP(_Nm,localFun(_,_,_,_,_)).
definedP(_Nm,moduleVar(_)).
definedP(_Nm,localVar(_,_,_)).

labelVars(Map,Prgs) :-
  lblVars(Map,[],Prgs).

lblVars([],Pr,Pr).
lblVars([lyr(_,Defs,_,ThVr)|Map],Pr,Prx) :-
  (ThVr=void -> Pr=Pr0 ;add_mem(ThVr,Pr,Pr0)),
  labelVarsInDefs(Defs,Pr0,Pr1),
  lblVars(Map,Pr1,Prx).

labelVarsInDefs([],Pr,Pr).
labelVarsInDefs([(_,labelArg(V,_,_ThVr))|Defs],Pr,Prx) :-
  (is_member(V,Pr) -> Pr0=Pr ; Pr0=[V|Pr]),
  labelVarsInDefs(Defs,Pr0,Prx).
labelVarsInDefs([_|Defs],Pr,Prx) :-
  labelVarsInDefs(Defs,Pr,Prx).

mergeGoal(enum("star.core#true"),G,_,G).
mergeGoal(G,enum("star.core#true"),_,G).
mergeGoal(G1,G2,Lc,cnj(Lc,G1,G2)).

mergeWhere(Exp,Cnd,Lc,Rslt) :-
  isCnd(Exp),!,
  mergeGoal(Cnd,Exp,Lc,Rslt).
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

pullWhere(whr(Lc,Val,Cond),G,Value,Gx) :-
  pullWhere(Val,G,Value,G1),
  mergeGoal(Cond,G1,Lc,Gx).
pullWhere(ctpl(Lbl,Args),G,ctpl(Lbl,NArgs),Gx) :-
  pullWheres(Args,NArgs,G,Gx).
pullWhere(Val,G,Val,G).

pullWheres([],[],G,G) :- !.
pullWheres([E|Rest],[Ex|Rx],G,Gx) :-
  pullWhere(E,G,Ex,G1),
  pullWheres(Rest,Rx,G1,Gx).

dispMap(Msg,Map) :-
  appStr(Msg,Chrs,C0),
  showMap(Map,C0,[]),
  string_chars(Txt,Chrs),
  writeln(Txt).

showMap([],Ox,Ox).
showMap([L|Rest],O,Ox) :-
  showLayer(L,O,O1),
  appNl(O1,O2),
  showMap(Rest,O2,Ox).

showLayer(lyr(Nm,Defs,void,void),O,Ox) :-
  appStr("Top Layer: ",O,O1),
  appStr(Nm,O1,O2),
  appNl(O2,O3),
  showLayerDefs(Defs,O3,Ox).
showLayer(lyr(Nm,Defs,FreeTerm,ThVr),O,Ox) :-
  appStr("Layer: ",O,O1),
  appStr(Nm,O1,O2),
  appNl(O2,O3),
  appStr("Free term: ",O3,O4),
  showTerm(FreeTerm,0,O4,O5),
  appStr("«",O5,O6),
  showTerm(ThVr,0,O6,O7),
  appStr("»\n",O7,O8),
  showLayerDefs(Defs,O8,Ox).

showLayerDefs(Defs,O,Ox) :-
  listShow(Defs,transutils:showLayerDef,misc:appNl,O,Ox).

showLayerDef((Nm,moduleFun(LclName,AccessName,Ar)),O,Ox) :-
  appStr("Global Fun ",O,O0),
  appStr(Nm,O0,O0a),
  appStr("=",O0a,O1),
  appStr(LclName,O1,O2),
  appStr("«",O2,O3),
  appStr(AccessName,O3,O4),
  appStr("/",O4,O7),
  appInt(Ar,O7,Ox).
showLayerDef((Nm,localFun(LclName,AccessName,ClosureName,Ar,ThV)),O,Ox) :-
  appStr("Fun ",O,O0),
  appStr(Nm,O0,O0a),
  appStr("=",O0a,O1),
  appStr(LclName,O1,O2),
  appStr("«",O2,O3),
  appStr(AccessName,O3,O4),
  appStr("»:[",O4,O5),
  appStr(ClosureName,O5,O6),
  appStr("@",O6,O6a),
  showTerm(ThV,0,O6a,O6b),
  appStr("]/",O6b,O7),
  appInt(Ar,O7,Ox).
showLayerDef((Nm,localVar(LclName,AccessName,ThV)),O,Ox) :-
  appStr("Var ",O,O0),
  appStr(Nm,O0,O0a),
  appStr("=",O0a,O1),
  appStr(LclName,O1,O2),
  appStr("«",O2,O3),
  appStr(AccessName,O3,O4),
  appStr("»:[",O4,O5),
  showTerm(ThV,0,O5,O6),
  appStr("]",O6,Ox).
showLayerDef((Nm,moduleVar(LclName)),O,Ox) :-
  appStr("Global Var ",O,O0),
  appStr(Nm,O0,O0a),
  appStr("=",O0a,O1),
  appStr(LclName,O1,Ox).
showLayerDef((Nm,labelArg(Field,Ix,ThV)),O,Ox) :-
  appStr("Free Var ",O,O0),
  appStr(Nm,O0,O0a),
  appStr("=",O0a,O1),
  showTerm(Field,0,O1,O2),
  appStr(":",O2,O3),
  showTerm(ThV,0,O3,O4),
  appStr("[",O4,O5),
  appInt(Ix,O5,O6),
  appStr("]",O6,Ox).
showLayerDef((Nm,moduleType(LclName,FullName,Tp)),O,Ox) :-
  appStr("Module type ",O,O1),
  appStr(Nm,O1,O2),
  appStr("=",O2,O3),
  appStr(LclName,O3,O4),
  appStr("«",O4,O5),
  appStr(FullName,O5,O6),
  appStr("»",O6,O7),
  showType(Tp,false,O7,Ox).
showLayerDef((Nm,moduleCons(LclName,FullName,Ar)),O,Ox) :-
  appStr("Module cons ",O,O1),
  appStr(Nm,O1,O2),
  appStr("=",O2,O3),
  appStr(LclName,O3,O4),
  appStr("«",O4,O5),
  appStr(FullName,O5,O6),
  appStr("»/",O6,O7),
  appInt(Ar,O7,Ox).
