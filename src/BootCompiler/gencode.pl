:- module(gencode,[compCode/3]).

:- use_module(misc).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

compCode(module(Pkg,Imports,Fields,Types,Enums,Defs,Contracts,Impls),Sig,Text) :-
  genPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Sig),
  genDefs(Defs,Chrs,[]),
  string_chars(Text,Chrs).

genPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Sig) :-
  constructPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Term),
  encodeTerm(Term,Chrs,[]),
  string_chars(Sig,Chrs).

constructPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,
    tpl([PkgNm,tpl(Imps),strg(FTps),tpl(ClsSigs),tpl(ConSigs),tpl(ImplSigs)])) :-
  encPkg(Pkg,PkgNm),
  encImports(Imports,Imps),
  encType(faceType(Fields,Types),FTps),
  formatEnums(Enums,ClsSigs),
  formatContracts(Contracts,ConSigs),
  formatImpls(Impls,ImplSigs).

encType(Tp,Sig) :-
  encodeType(Tp,O,[]),
  string_chars(Sig,O).

encPkg(pkg(Nm,Vers),ctpl(strct("pkg",2),[strg(Nm),V])) :-
  encVer(Vers,V).

encVer(defltVersion,enum("*")).
encVer(ver(V),strg(V)).

encImports([],[]).
encImports([I|M],[IP|L]) :-
  encImport(I,IP),
  encImports(M,L).

encImport(import(Viz,Pkg,_,_,_,_,_),ctpl(strct("import",2),[enum(Viz),Enc])) :-
  encPkg(Pkg,Enc).

formatEnums([],[]).
formatEnums([Nm|M],[strg(Nm)|R]) :-
  formatEnums(M,R).

formatContracts([],[]).
formatContracts([conDef(Nm,CnNm,Spec)|M],[tpl([strg(Nm),strg(CnNm),strg(CSig)])|R]) :-
  encodeType(Spec,CChars,[]),
  string_chars(CSig,CChars),
  formatContracts(M,R).

formatImpls([],[]).
formatImpls([imp(Nm,Spec)|M],[tpl([strg(Nm),strg(Sig)])|R]) :-
  encodeConstraint(Spec,Chars,[]),
  string_chars(Sig,Chars),
  formatImpls(M,R).

genDefs(Defs,O,Ox) :-
  rfold(Defs,gencode:genDef,O,Ox).

genDef(fnDef(_,Nm,Tp,[eqn(_,Args,Value)]),O,[Txt|O]) :-
  encType(Tp,Sig),
  Code = [method(Nm,Sig)|C0],
  buildArgDict(Args,0,[],D0),
  compTerm(Value,D0,Dx,C1,[]),
  genEnter(Dx,C0,C1),
  assemble(Code,Chrs),
  string_chars(Txt,Chrs).

buildArgDict([],_,D,D).
buildArgDict([A|L],C,D,Dx) :-
  buildArg(A,C,C0,D,D0),
  buildArgDict(L,C0,D0,Dx).

buildArg(Nm,C,Cx,D,[(Nm,a(C))|D]) :-
   Cx is C+1.

genEnter(D,[enter(C)|Cx],Cx) :-
  findMaxLocal(D,L).

compTerm(intgr(Ix),D,D,[ldc(intgr(Ix))|Cx],Cx,Stk,Stk1) :- Stk1 is Stk+1.
compTerm(float(Dx),D,D,[ldc(float(Dx))|Cx],Cx,Stk,Stk1) :- Stk1 is Stk+1.
compTerm(strg(Sx),D,D,[ldc(strg(Sx))|Cx],Cx,Stk,Stk1) :- Stk1 is Stk+1.
compTerm(v(_,Nm),D,D,[lda(X)|Cx],Cx,Stk, Stk1) :-
  is_member((Nm,a(X)),D),!,
  Stk1 is Stk+1.
compTerm(v(_,Nm),D,D,[ldl(X)|Cx],Cx,Stk, Stk1) :-
  is_member((Nm,l(X)),D),!,
  Stk1 is Stk+1.
compTerm(enum(Nm),D,D,[ldc(enum(Nm))|Cx],Cx,Stk,Stk1) :- Stk1 is Stk+1.
compTerm(tpl(A),D,Dx,C,Cx,Stk,Stk1) :- Stk1 is Stk+1,
  compTerms(A,D,Dx,C,[alloc(TpStrct),frame(Stk2)|Cx],Stk,Stk2),
  length(A,Ar),
  genTplStruct(Ar,TpStrct).
compTerm(ctpl(C,A),D,Dx,C,Cx,Stk,Stk1) :- Stk1 is Stk+1,
  compTerms(A,D,Dx,C,[alloc(C),frame(Stk2)|Cx],Stk,Stk2).
compTerm(ecll(_,Nm,A),D,Dx,C,Cx,Stk,Stk1) :- Stk1 is Stk+1,
  compTerms(A,D,Dx,C,[escape(Nm),frame(Stk2)|Cx],Stk,Stk2).
compTerm()
