:- module(transutils,
	  [trCons/3,mergeSeq/4,mergeWhere/4,extraVars/2,thisVar/2,
	   lookupVar/3,lookupThetaVar/3,lookupTypeIndex/3,
	   lookupType/3,findConsType/3,
	   definedProgs/2,labelVars/2,
	   genVar/2, genVars/2,
	   genVoids/2,
	   pushOpt/3, isOption/2,dispMap/2,
	   pullWhere/4,pullWheres/4]).

:- use_module(canon).
:- use_module(misc).
:- use_module(dict).
:- use_module(types).
:- use_module(freshen).
:- use_module(lterms).
:- use_module(display).

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
 * lyr(Defs:list[(Name,Class),LblRecord,Thvr)
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
 *    lyr(Defs,anon23(Free,bar(B,foo(A))),ThVar)
 * the outermost class layer will look like
 *    lyr(Defs,foo(A),thVar)
 * the package layer will look like
 *    lyr(Defs,'',void)
 */

lookup([],_,_,notInMap).
lookup([Lyr|_Layers],Nm,Filter,Reslt) :-
  call(Filter,Lyr,Nm,Reslt),!.
lookup([_|Layers],Nm,Filter,Reslt) :-
  lookup(Layers,Nm,Filter,Reslt).

lookupVar(Map,Nm,Entry) :-
  makeKey(Nm,Key),
  lookup(Map,Key,transutils:findVar,Entry).

findVar(lyr(VrMap,_,_,_),Key,Entry) :-
  get_dict(Key,VrMap,Entry),!.

lookupType(Map,TpNm,Entry) :-
%  marker(type,TpMrkr),
%  splitLocalName(TpNm,TpMrkr,_,Nm),
  makeKey(TpNm,Key),
  lookup(Map,Key,transutils:findType,Entry).

findType(lyr(_,TpMap,_,_),Key,Entry) :-
  get_dict(Key,TpMap,Entry),!.

lookupThetaVar(Map,Nm,V) :-
  makeKey(Nm,Key),
  lookup(Map,Key,transutils:findThetaVar,V),
  \+V=notInMap.

findThetaVar(lyr(VrMap,_,_,_),Key,V) :-
  get_dict(Key,VrMap,Entry),
  getThetaVar(Entry,V),!.

getThetaVar(localFun(_,_,_,ThVr),ThVr).
getThetaVar(localClass(_,_,_,_,ThVr),ThVr).
getThetaVar(labelArg(_,_,ThVr),ThVr).

lookupTypeIndex(Map,TpNm,Index) :-
  makeKey(TpNm,Key),
  lookupIndex(Map,Key,Index).

lookupIndex([lyr(_,_,ConsIndex,_)|_],Key,Index) :-
  get_dict(Key,ConsIndex,Index),!.
lookupIndex([_|Map],Key,Index) :-
  lookupIndex(Map,Key,Index).

findConsType(Map,CnsNm,Tp) :-
 lookupVar(Map,CnsNm,moduleCons(_,Tp,_)).

extraVars([lyr(_,_,_,void)|_],[]) :- !.
extraVars([lyr(_,_,_,ThVr)|_],[ThVr]).

thisVar([lyr(_,_,_,ThVr)|_],ThVr) :- ThVr \= void.

definedProgs(Map,Prgs) :-
  definedProgs(Map,[],Prgs).

definedProgs([],Pr,Pr).
definedProgs([lyr(Defs,_,_,_)|Map],Pr,Prx) :-
  dict_pairs(Defs,_,Pairs),
  definedInDefs(Pairs,Pr,Pr0),
  definedProgs(Map,Pr0,Prx).

definedInDefs([],Pr,Pr).
definedInDefs([Nm-Entry|Defs],Pr,Prx) :-
  definedP(Nm,Entry),!,
  (is_member(idnt(Nm),Pr) -> Pr0=Pr ; Pr0=[idnt(Nm)|Pr]),
  definedInDefs(Defs,Pr0,Prx).
definedInDefs([_|Defs],Pr,Prx) :-
  definedInDefs(Defs,Pr,Prx).

definedP(_Nm,moduleFun(_,_,_)).
definedP(_Nm,localFun(_,_,_,_)).
definedP(_Nm,moduleVar(_)).
definedP(_Nm,localVar(_,_,_)).

labelVars(Map,Prgs) :-
  lblVars(Map,[],Prgs).

lblVars([],Pr,Pr).
lblVars([lyr(Vrs,_,_,ThVr)|Map],Pr,Prx) :-
  (ThVr=void -> Pr=Pr0 ;add_mem(ThVr,Pr,Pr0)),
  dict_pairs(Vrs,_,Pairs),
  labelVarsInDefs(Pairs,Pr0,Pr1),
  lblVars(Map,Pr1,Prx).

labelVarsInDefs([],Pr,Pr).
labelVarsInDefs([_-labelArg(V,_,_ThVr)|Defs],Pr,Prx) :-
  (is_member(V,Pr) -> Pr0=Pr ; Pr0=[V|Pr]),
  labelVarsInDefs(Defs,Pr0,Prx).
labelVarsInDefs([_|Defs],Pr,Prx) :-
  labelVarsInDefs(Defs,Pr,Prx).

mergeGoal(none,G,_,G).
mergeGoal(some(G),none,_,G).
mergeGoal(some(G1),some(G2),Lc,some(cnj(Lc,G1,G2))).

mergeWhere(Exp,Cnd,Lc,Rslt) :-
  isCnd(Exp),!,
  mergeGoal(Cnd,some(Exp),Lc,Rslt).
mergeWhere(Exp,none,_,Exp).
mergeWhere(Exp,some(G),Lc,whr(Lc,Exp,G)).

mergeSeq(_,L,R,R) :- isUnit(L).
mergeSeq(_,L,R,L) :- isUnit(R).
mergeSeq(Lc,seq(Lc0,L0,R0),R,seq(Lc0,L0,RR)) :-
  mergeSeq(Lc,R0,R,RR).
mergeSeq(Lc,L,R,seq(Lc,L,R)).

pushOpt(Opts,Opt,[Opt|Opts]).

isOption(Opt,Opts) :- is_member(Opt,Opts),!.

genVar(Prefix,idnt(V)) :-
  genstr(Prefix,V).

genVars(0,[]).
genVars(K,[V|Rest]) :-
  K>0,
  K1 is K-1,
  genVar("_V",V),
  genVars(K1,Rest).

genVoids(0,[]).
genVoids(K,[voyd|Rest]) :-
  K>0,
  K1 is K-1,
  genVoids(K1,Rest).

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
  ssMap(Msg,Map,S),
  displayln(S).

ssMap(Msg,Map,sq([ss(Msg),nl(0),iv(nl(0),MM)])) :-
  map(Map,transutils:ssLayer,MM).

ssLayer(lyr(VarMap,TpMap,ConsMap,void),sq([ss("Top layer:"),nl(0),iv(nl(0),DD)])) :-
  ssConsMap(ConsMap,MM),
  ssDecMap(VarMap,VV),
  ssDecMap(TpMap,TT),
  flatten([MM,VV,TT],DD).
ssLayer(lyr(VarMap,TpMap,ConsMap,ThVr),
	  sq([ss("layer:"),ss("«"),TV,ss("»"),nl(0),iv(nl(0),DD)])) :-
  ssConsMap(ConsMap,MM),
  ssTrm(ThVr,0,TV),
  ssDecMap(VarMap,VV),
  ssDecMap(TpMap,TT),
  flatten([MM,VV,TT],DD).

ssDecMap(Map,XX) :-
  dict_pairs(Map,_,Pairs),
  map(Pairs,transutils:ssLyrDec,XX).
  
ssConsMap(Map,S) :-
  dict_pairs(Map,_,Pairs),
  map(Pairs,transutils:ssPair,S).

ssPair(K-V,sq([id(K),ss("-"),XX])) :-
  ssConsIndex(V,XX).

ssConsIndex(V,iv(ss(","),XX)) :-
  map(V,transutils:ssIxPr,XX).

ssIxPr((Lbl,Ix),sq([LL,ss(":"),ix(Ix)])) :-
  ssTrm(Lbl,0,LL).

ssLyrDec(Nm-moduleFun(LclName,_AccessName,Ar),
	 sq([ss("Global Fun "),id(Nm),ss("="),ss(LclName),ss("/"),ix(Ar)])).
ssLyrDec(Nm-localFun(LclName,_ClosureName,Ar,ThV),
	 sq([ss("Fun "),id(Nm),ss("="),ss(LclName),ss("/"),ix(Ar),ss("@"),TT])) :-
  ssTrm(ThV,0,TT).
ssLyrDec(Nm-localVar(LclName,_AccessName,ThV),
	 sq([ss("Var "),id(Nm),ss("="),ss(LclName),ss("@"),TT])) :-
  ssTrm(ThV,0,TT).
ssLyrDec(Nm-localVar(_,Val),
	 sq([ss("Var "),id(Nm),ss("="),VV])) :-
  ssTerm(Val,0,VV).
ssLyrDec(Nm-moduleVar(LclName),
	 sq([ss("Global Var "),id(Nm),ss("="),ss(LclName)])).
ssLyrDec(Nm-labelArg(_Field,Ix,ThV),
	 sq([ss("Free Var "),id(Nm),ss("="),TT,ss("["),ix(Ix),ss("]")])) :-
  ssTrm(ThV,0,TT).
ssLyrDec(Nm-moduleType(TpNm,_Tp,IxMap),
	 sq([ss("Module type "),id(Nm),ss("="),ss(TpNm),CC])) :-
  ssConsIndex(IxMap,CC).
ssLyrDec(Nm-moduleCons(LclName,_,Ar),
	 sq([ss("Module Cons "),id(Nm),ss("="),ss(LclName),ss("/"),ix(Ar)])).
ssLyrDec(Nm-fieldAcc(TpNm,FldNm,_FldTp),
	 sq([ss("Field accesss "),id(Nm),ss(" in "),ss(TpNm),ss(" is "),id(FldNm)])).
