:- module(transUtils,[trCons/3,labelAccess/5,extraVars/2,thisVar/2,streamVar/2,pushStreamVar/3,
          lookupVarName/3,lookupRelName/3,lookupFunName/3,lookupClassName/3,
          makePkgMap/6,programAccess/5,
          genNewName/4,genVar/2,
          pushOpt/3, isOption/2,layerName/2,
          genAnons/2,genVars/2]).

:- use_module(misc).
:- use_module(dict).
:- use_module(types).
:- use_module(freshen).

trCons(Nm,Arity,strct(Name,Arity)) :-
  integer(Arity),!,
  number_string(Arity,Sz),
  string_concat(Nm,"%",N1),
  string_concat(N1,Sz,Name).
trCons(Nm,Args,strct(Name,Arity)) :-
  length(Args,Arity),
  number_string(Arity,Sz),
  string_concat(Nm,"%",N1),
  string_concat(N1,Sz,Name).

genNewName(Map,Variant,Ar,prg(Nm,Ar)) :-
  layerName(Map,Prefix),
  genstr(Variant,V),
  localName(Prefix,"@",V,Nm).

/*
 * Each element in Layers defines a scope. It is a tuple of the form:
 * lyr(Prefix,Defs:list[(Name,Class),Loc,Label,Clvr,Thvr,StreamVr)
 * Where Prefix is the current prefix
 * Defs is the set of local programs and other names defined in this scope
 * Loc is the file location of the defining label
 * Label is the full form of the label term.
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
 *    lyr(pk#foo#bar#anon23,Defs,Lc,anon23(Free,bar(B,foo(A))),ClVar,ThVar,void)
 * the outermost class layer will look like
 *    lyr(pk#foo,Defs,Lc,foo(A),clVar,thVar,void)
 * the package layer will look like
 *    lyr(pk,Defs,Lc,void,void,void)
 */

makePkgMap(Pkg,Defs,Types,Imports,Enums,Map) :-
  makeModuleMap(Pkg,Defs,DfList,Rest,Enums),
  makeImportsMap(Imports,Rest,R0),
  makeTypesMap(Pkg,Types,R0,[]),
  pushMap(Pkg,DfList,[],Map).

pushMap(PkgName,Defs,Std,[lyr(PkgName,Defs,'',void,void,void,void)|Std]).

makeModuleMap(Pkg,[Def|Rest],Map,Mx,Enums) :-
  makeMdlEntry(Pkg,Def,Map,M0,Enums,Clx),
  makeModuleMap(Pkg,Rest,M0,Mx,Clx).
makeModuleMap(_,[],Map,Map,[]).

makeMdlEntry(Pkg,function(_,Nm,Tp,_,_),[(Nm,moduleFun(Pkg,LclName,AccessName,ClosureName,Arity))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName),
  localName(Pkg,"^",Nm,ClosureName),
  typeArity(Tp,Ar),
  Arity is Ar+1.
makeMdlEntry(Pkg,grammar(_,Nm,Tp,_,_),[(Nm,moduleRel(Pkg,LclName,AccessName,ClosureName,Arity))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName),
  localName(Pkg,"^",Nm,ClosureName),
  typeArity(Tp,Ar),
  Arity is Ar+2.
makeMdlEntry(Pkg,predicate(_,Nm,Tp,_,_),[(Nm,moduleRel(Pkg,LclName,AccessName,ClosureName,Arity))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName),
  localName(Pkg,"^",Nm,ClosureName),
  typeArity(Tp,Arity).
makeMdlEntry(Pkg,defn(_,Nm,_,_,_,_),[(Nm,moduleVar(Pkg,LclName,AccessName))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName).
makeMdlEntry(Pkg,class(_,Nm,Tp,_,_,_),[(Nm,moduleClass(LclName,AccessName,Ar))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,AccessName),
  localName(Pkg,"#",Nm,LclName),
  typeArity(Tp,Ar).
makeMdlEntry(Pkg,enum(_,Nm,_,_,_,_),[(Nm,moduleClass(LclName,AccessName,0))|Mx],Mx,[Nm|Clx],Clx) :-
  localName(Pkg,"#",Nm,LclName),
  localName(Pkg,"@",Nm,AccessName).
makeMdlEntry(Pkg,typeDef(_,Nm,Tp,_),[(Nm,moduleType(Pkg,LclName,Tp))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"*",Nm,LclName).
makeMdlEntry(_,impl(_,_,ImplNm,0,_,_,_,_,_),[(ImplNm,moduleImpl(ImplNm,enum(ImplNm)))|Mx],Mx,Clx,Clx).
makeMdlEntry(_,impl(_,_,ImplNm,Arity,_,_,_,_,_),[(ImplNm,moduleImpl(ImplNm,strct(ImplNm,Arity)))|Mx],Mx,Clx,Clx).
makeMdlEntry(_,_,Mx,Mx,Clx,Clx).

makeImportsMap([Import|Rest],Map,Mx) :-
  makeImportMap(Import,Map,M0),
  makeImportsMap(Rest,M0,Mx).
makeImportsMap([],Map,Map).

makeImportMap(import(_,pkg(Pkg,_),faceType(Fields,Types),_,Enums,_,Impls),Map,Mx) :-
  importFields(Pkg,Enums,Fields,Map,M0),
  importImplementations(Impls,M0,M1),
  importTypes(Types,M1,Mx).

importFields(_,_,[],Map,Map).
importFields(Pkg,Enums,[(Nm,Tp)|Fields],Map,Mx) :-
  moveQuants(Tp,_,QTp),
  moveConstraints(QTp,_,Template),
  makeImportEntry(Template,Enums,Pkg,Nm,Map,M0),
  importFields(Pkg,Enums,Fields,M0,Mx).

importImplementations([],Map,Map).
importImplementations([imp(Nm,Con)|L],[(Nm,moduleImpl(Nm,Struct))|M],Mx) :-
  contractArity(Con,Ar),
  contractStruct(Ar,Nm,Struct),
  importImplementations(L,M,Mx).

contractArity(allType(_,Con),Ar) :- contractArity(Con,Ar).
contractArity(constrained(Con,_),Ar) :- contractArity(Con,A), Ar is A+1.
contractArity(_,0).

contractStruct(0,Nm,enum(Nm)).
contractStruct(Ar,Nm,strct(Nm,Ar)).

makeImportEntry(funType(A,_),_,Pkg,Nm,[(Nm,moduleFun(Pkg,LclName,AccessName,ClosureName,Arity))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName),
  localName(Pkg,"^",Nm,ClosureName),
  typeArity(A,Ar),
  Arity is Ar+1.
makeImportEntry(grammarType(A,_),_,Pkg,Nm,[(Nm,moduleRel(Pkg,LclName,AccessName,ClosureName,Arity))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName),
  localName(Pkg,"^",Nm,ClosureName),
  typeArity(A,Ar),
  Arity is Ar+2.
makeImportEntry(_,Enums,Pkg,Nm,[(Nm,moduleClass(LclName,AccessName,0))|Mx],Mx) :-
  is_member(Nm,Enums),!,
  localName(Pkg,"@",Nm,AccessName),
  localName(Pkg,"#",Nm,LclName).
makeImportEntry(consType(A,_),_,Pkg,Nm,[(Nm,moduleClass(LclName,AccessName,Ar))|Mx],Mx) :-
  localName(Pkg,"#",Nm,LclName),
  localName(Pkg,"@",Nm,AccessName),
  length(A,Ar).
makeImportEntry(_,_,Pkg,Nm,[(Nm,moduleVar(Pkg,LclName,AccessName))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName).

makeTypesMap(_,_,List,List).

lookup([],_,_,notInMap).
lookup([lyr(_Prefix,Defns,_Lc,_Lbl,_LbVr,_ThVr,_StrmVr)|_Layers],Nm,Filter,Reslt) :-
  filteredSearch(Defns,Filter,Nm,Reslt),!.
lookup([_|Layers],Nm,Filter,Reslt) :-
  lookup(Layers,Nm,Filter,Reslt).

filteredSearch(Defns,Filter,Nm,Defn) :-
  is_member((Nm,Defn),Defns),
  call(Filter,Defn),!.

lookupVarName(Map,Nm,V) :-
  lookup(Map,Nm,nonType,V).

anyDef(_).

lookupRelName(Map,Nm,V) :-
  lookup(Map,Nm,relDef,V).

relDef(localRel(_,_,_,_,_,_)).
relDef(moduleRel(_,_,_,_,_)).
relDef(inherit(_,_,_,_)).
relDef(inheritField(_,_,_)).

lookupFunName(Map,Nm,V) :-
  lookup(Map,Nm,funDef,V).

funDef(localFun(_,_,_,_,_,_)).
funDef(moduleFun(_,_,_,_,_)).
funDef(inherit(_,_,_,_)).
funDef(inheritField(_,_,_)).
funDef(localClass(_,_,_,_,_)).
funDef(moduleClass(_,_,_)).
funDef(moduleImpl(_,_)).

lookupClassName(Map,Nm,V) :-
  lookup(Map,Nm,classDef,V).

classDef(localClass(_,_,_,_,_)).
classDef(moduleClass(_,_,_)).
classDef(inherit(_,_,_,_)).
classDef(inheritField(_,_,_)).

lookupDefn(Map,Nm,Df) :-
  lookup(Map,Nm,nonType,Df).

nonType(Df) :- Df \= moduleType(_,_,_), Df \= localType(_).

programAccess(moduleFun(_,Prog,Access,Closure,Arity),Prog,Access,Closure,Arity).
programAccess(localFun(Prog,Access,Closure,Arity,_,_),Prog,Access,Closure,Arity).
programAccess(moduleRel(_,Prog,Access,Closure,Arity),Prog,Access,Closure,Arity).
programAccess(localRel(Prog,Access,Closure,Arity,_,_),Prog,Access,Closure,Arity).
programAccess(moduleVar(_,Prog,Access),Prog,Access,Access,1).
programAccess(localVar(Prog,Access,_,_),Prog,Access,Access,1).

extraVars([lyr(_,_,_,_,void,void,_)|_],[]) :- !.
extraVars([lyr(_,_,_,_,LbVr,ThVr,_)|_],[LbVr,ThVr]).

thisVar([lyr(_,_,_,_,_,ThVr,_)|_],ThVr) :- ThVr \= void.

streamVar([lyr(_,_,_,_,_,_,StrmVr)|_],StrmVr) :- StrmVr \= void.
pushStreamVar([lyr(Nm,Defs,Lc,GlVr,LbVr,ThVr,_)|Rest],StrmVr,[lyr(Nm,Defs,Lc,GlVr,LbVr,ThVr,StrmVr)|Rest]).

labelAccess(Q,Q,[lyr(_,_,_,_,void,void,_)|_],G,G) :- !.
labelAccess(Q,Qx,[lyr(_,_,_,LblGl,LbVr,_,_)|_],G,Gx) :- concat(LblGl,Gx,G),merge([LbVr],Q,Qx).

pushOpt(Opts,Opt,[Opt|Opts]).

isOption(Opt,Opts) :- is_member(Opt,Opts),!.

layerName([lyr(Nm,_,_,_,_,_,_)|_],Nm).

genVar(Prefix,idnt(V)) :-
  genstr(Prefix,V).

genAnons(0,[]).
genAnons(K,[anon|Rest]) :-
  K>0,
  K1 is K-1,
  genAnons(K1,Rest).

genVars(0,[]).
genVars(K,[V|Rest]) :-
  K>0,
  K1 is K-1,
  genVar("V",V),
  genVars(K1,Rest).
