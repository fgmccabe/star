:- module(macroRules,[build_main/2,
		      macroRl/3]).

:- use_module(abstract).
:- use_module(display).
:- use_module(grammar).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(astdisp).
:- use_module(location).

macroRl("[]",pattern,macroRules:squarePtnMacro).
macroRl("[]",expression,macroRules:squareSequenceMacro).
macroRl("$[]",expression,macroRules:indexMacro).
macroRl("<||>",expression,macroRules:macroQuote).
macroRl("{}",expression,macroRules:comprehensionMacro).
macroRl("{}",expression,macroRules:mapLiteralMacro).
macroRl("{}",expression,macroRules:totalizerMacro).
%macroRl("{}",expression,macroRules:anonBraceMacro).
macroRl("{}",pattern,macroRules:anonBraceMacro).
macroRl("{!!}",expression,macroRules:macroIotaComprehension).
macroRl("{??}",expression,macroRules:iterableGoalMacro).
macroRl("::",expression,macroRules:coercionMacro).
macroRl(":?",expression,macroRules:coercionMacro).
macroRl("*",expression,macroRules:multicatMacro).
macroRl("__pkg__",expression,macroRules:pkgNameMacro).
macroRl("__loc__",expression,macroRules:macroLocationExp).
macroRl("-",expression,macroRules:uminusMacro).
macroRl("?=",expression,macroRules:optionMatchMacro).
macroRl("?|",expression,macroRules:optionCondMacro).
macroRl("?",expression,macroRules:pullMacro).
macroRl("!",expression,macroRules:binRefMacro).
macroRl(":",rule,macroRules:caseRuleMacro).
macroRl(":",actionRule,macroRules:caseRuleMacro).
macroRl(":=",action,macroRules:spliceAssignMacro).
macroRl(":=",action,macroRules:indexAssignMacro).
macroRl(":=",expression,macroRules:spliceAssignMacro).
macroRl(":=",expression,macroRules:indexAssignMacro).
macroRl("=>",statement,macroRules:curryMacro).
macroRl("=>",expression,macroRules:curryMacro).
macroRl("do",action,macroRules:forLoopMacro).
macroRl(":",action,macroRules:forMacro).
macroRl(":",expression,macroRules:rangeMacro).
macroRl("->",expression,macroRules:arrowMacro).
macroRl("->",pattern,macroRules:arrowMacro).
macroRl("..<",expression,macroRules:incRangeMacro).
macroRl("..>",expression,macroRules:decRangeMacro).
macroRl("assert",action,macroRules:assertMacro).
macroRl("show",action,macroRules:showMacro).
macroRl("trace",expression,macroRules:traceMacro).
macroRl("generator{}",expression,macroRules:generatorMacro).
macroRl("yield",action,macroRules:yieldMacro).
macroRl("task{}",expression,macroRules:taskMacro).
macroRl("collect",expression,macroRules:collectMacro).
macroRl("-->",statement,macroRules:grammarMacro).
macroRl("-->",expression,macroRules:grammarCallMacro).
macroRl("-->",type,macroRules:grammarTypeMacro).
macroRl("async",type,macroRules:asyncMacro).

build_main(As,Bs) :-
  look_for_signature(As,"main",Lc,Ms,Tp),
  \+look_for_signature(As,"_main",_,_,_),!,
  synthesize_main(Lc,Ms,Tp,As,Bs).
build_main(A,A).

look_for_signature([St|_],Nm,Lc,Ms,T) :-
  (isTypeDecl(St,Lc,V,T) ;
   (isPublic(St,_,I),isTypeDecl(I,Lc,V,T))),
  isIden(V,_,Nm),
  (isFuncType(T,_,L,_) ; isProcType(T,_,L,_)),
  isTuple(L,_,Ms),!.
look_for_signature([_|As],Nm,Lc,Ms,Tp) :-
  look_for_signature(As,Nm,Lc,Ms,Tp).

/*
   main:(t1,..,tk) => E

  becomes

   _main([A1,..,Ak]) => valof{
     try{
       V1 = _coerce(A1):t1;
       try{
       ...
          main(V1,..,Vk);
          valis 0
       } catch { _ do {
           _show("Cant coerce [#(Ak)] into a tk");
           valis 1
         }
       }
      ...
    } catch { _ do {
       _show("Cant coerce [#(Ak)] into a tk");
       valis 1
     }
   }
  }
  _main(_) default => valof{
    _show("Expecting k args");
    valis 1
  }
*/


synthesize_main(Lc,Ts,Tp,As,[MainTp,Main,MnDeflt|As]) :-
  (isFuncType(Tp,_,_,_) ->
     Inner = macroRules:synth_main_fun;
     Inner = macroRules:synth_main_prc),
  synthesize_coercion(Lc,Ts,Vs,Vrs,Vrs,Inner,MnSeq),
%  dispAst(MnSeq),
  list_pttrn(Lc,Vs,Arg),
  roundTerm(Lc,name(Lc,"_main"),[Arg],Lhs),
  braceTuple(Lc,[MnSeq],MnAct),
  mkValof(Lc,MnAct,MnReslt),
  eqn(Lc,Lhs,MnReslt,Main),

  mkAnon(Lc,Anon),
  roundTerm(Lc,name(Lc,"_main"),[Anon],DLhs),

  mkEnum(Lc,"nil",Nil),
  mkConApply(Lc,name(Lc,"cons"),[string(Lc," arguments"),Nil],M0),
  length(Ts,Ar),
  ast2String(Lc,integer(Lc,Ar),ArTxt),
  mkConApply(Lc,name(Lc,"cons"),[ArTxt,M0],M1),
  mkConApply(Lc,name(Lc,"cons"),[string(Lc,"Expecting "),M1],M2),
  unary(Lc,"_str_multicat",M2,Msg),
  roundTerm(Lc,name(Lc,"_show"),[Msg],Log),
  mkValis(Lc,integer(Lc,1),Vl),
  mkSequence(Lc,[Log,Vl],Body),
  braceTuple(Lc,[Body],VB),
  mkValof(Lc,VB,GVl),
  eqn(Lc,DLhs,GVl,MnDeflt),
%  dispAst(MnDeflt),
  squareTerm(Lc,name(Lc,"cons"),[name(Lc,"string")],T1),
  roundTuple(Lc,[T1],T3),
  binary(Lc,"=>",T3,name(Lc,"integer"),TU),
  mkTypeDecl(Lc,name(Lc,"_main"),TU,MainTp).
%  dispAst(Main).
  
/* T -> main(NV1,..,NVk); valis 0
   or
   T -> valis main(NV1,..,NVk)
*/
synth_main_fun(Lc,Vrs,C) :-
  roundTerm(Lc,name(Lc,"main"),Vrs,Mn),
  mkValis(Lc,Mn,C).

synth_main_prc(Lc,Vrs,C) :-
  roundTerm(Lc,name(Lc,"main"),Vrs,Mn),
  mkValis(Lc,integer(Lc,0),Vl),
  mkSequence(Lc,[Mn,Vl],C).

synthesize_coercion(Lc,[],[],[],Vrs,Inner,C) :-
  call(Inner,Lc,Vrs,C).

  
/* T -> try {
     NV = _coerce(V):T; Inner; }
   catch {
    _ do {_logmsg("Cannot coerce [#(V)] to T"); valis 1}
   }
*/
synthesize_coercion(Lc,[T|Ts],[V|Vs],[NV|Vx],Vrs,Inner,C) :-
  genIden(Lc,V),
  genIden(Lc,NV),
  coerce(Lc,V,T,O),
  mkDefn(Lc,NV,O,S1),
  synthesize_coercion(Lc,Ts,Vs,Vx,Vrs,Inner,S2),
  mkSequence(Lc,[S1,S2],Body),
  mkEnum(Lc,"nil",Nil),
  ast2String(Lc,T,Tp),
  mkConApply(Lc,name(Lc,"cons"),[Tp,Nil],M0),
  mkConApply(Lc,name(Lc,"cons"),[string(Lc,"to "),M0],M1),
  mkConApply(Lc,name(Lc,"cons"),[V,M1],M2),
  mkConApply(Lc,name(Lc,"cons"),[string(Lc,"Cannot coerce "),M2],M3),
  unary(Lc,"_str_multicat",M3,Msg),
  roundTerm(Lc,name(Lc,"_show"),[Msg],Log),
  mkValis(Lc,integer(Lc,1),Vl),
  mkSequence(Lc,[Log,Vl],CB),
  mkAnon(Lc,Anon),
  mkProcedure(Lc,Anon,none,CB,Ctch),
  mkTry(Lc,Body,[Ctch],C).

list_pttrn(Lc,[],Arg) :-!,
  mkEnum(Lc,"nil",Arg).
list_pttrn(Lc,[T|Ts],Arg) :-
  list_pttrn(Lc,Ts,As),
  mkConApply(Lc,name(Lc,"cons"),[T,As],Arg).

squareSequenceMacro(A,expression,Trm) :-
  isSquareTuple(A,Lc,Els),
  \+isMapLiteral(A,_,_),
  macroListEntries(Lc,Els,Trm,nilGen,consGen).

nilGen(Lc,name(Lc,"_nil")).

consGen(Lc,L,R,Trm) :-
  binary(Lc,"_cons",L,R,Trm).

appndGen(Lc,L,R,Trm) :-
  binary(Lc,"_apnd",L,R,Trm).

emptyGen(Lc,name(Lc,"_empty")).

putGen(Lc,Pr,R,Trm) :-
  isPair(Pr,_,F,V),
  ternary(Lc,"_put",R,F,V,Trm).

squarePtnMacro(A,pattern,Ptn) :-
  isSquareTuple(A,Lc,Els),!,
  macroListEntries(Lc,Els,Ptn,genEofTest,genHedTest).

genEofTest(Lc,Trm) :-
  genIden(Lc,X),
  unary(Lc,"_eof",X,E),
  mkWhere(Lc,X,E,Trm).

genHedTest(Lc,L,R,Trm) :-
  mkWherePtn(Lc,tuple(Lc,"()",[L,R]),name(Lc,"_hdtl"),Trm).

macroListEntries(Lc,[],Trm,End,_) :-
  call(End,Lc,Trm).
macroListEntries(_,[Cns],Trm,_,Hed) :-
  isConsTerm(Cns,Lc,H,T),
  call(Hed,Lc,H,T,Trm).
macroListEntries(Lc,[E|L],Trm,End,Hed) :-
  macroListEntries(Lc,L,Tr,End,Hed),
  call(Hed,Lc,E,Tr,Trm).

indexMacro(T,expression,Rp) :-
  isIndexTerm(T,Lc,M,A),!,
  (isBinary(A,_,"->",Ky,Vl) ->
   ternary(Lc,"_put",M,Ky,Vl,Rp);
   isNegation(A,_,Ky) ->
   binary(Lc,"_remove",M,Ky,Rp);
   binary(Lc,"_index",M,A,Rp)).
indexMacro(T,expression,Rp) :-
  isSlice(T,Lc,M,Fr,To),
  ternary(Lc,"_slice",M,Fr,To,Rp).

mapLiteralMacro(A,expression,Trm) :-
  isMapLiteral(A,Lc,Prs),!,
  macroListEntries(Lc,Prs,T,emptyGen,putGen),
  mkAnon(Lc,Anon),
  squareTerm(Lc,name(Lc,"map"),[Anon,Anon],MapTp),
  typeAnnotation(Lc,T,MapTp,Trm).

comprehensionMacro(T,expression,Rp) :-
  isComprehension(T,Lc,Bnd,Body),!,
  (isBinary(Bnd,"<*",_,L,R) ->
   makeTotalizer(Lc,L,R,Body,Rp);
   makeComprehension(Lc,Bnd,Body,Rp)).
%  dispAst(Rp).

makeComprehension(Lc,Bnd,Body,Rp) :-
  makeCondition(Body,macroRules:passThru,macroRules:push(Lc,Bnd),grounded(name(Lc,"_null")),Rp).

totalizerMacro(T,expression,Rp) :-
  isTotalizerComprehension(T,Lc,Fun,El,Zr,Body),!,
  makeTotalizer(Lc,Fun,El,Zr,Body,Rp).

makeTotalizer(Lc,Fn,El,Zr,Body,Rp) :-
  makeCondition(Body,macroRules:passThru,macroRules:foldResult(Lc,Fn,El),grounded(Zr),Rp).

macroIotaComprehension(T,expression,Rp) :-
  isIotaComprehension(T,Lc,Bnd,Body),!,
  mkConApply(Lc,name(Lc,"some"),[Bnd],Res),
  mkEnum(Lc,"none",Empty),
  makeCondition(Body,macroRules:passThru,macroRules:rtn(Res),grounded(Empty),Rp).

iterableGoalMacro(G,expression,Gx) :-
  isTestComprehension(G,_Lc,B),
  makeIterableGoal(B,Gx).

makeIterableGoal(G,Rp) :-
  locOfAst(G,Lc),
  mkEnum(Lc,"false",False),
  mkEnum(Lc,"true",True),
  makeCondition(G,macroRules:passThru,macroRules:rtn(True),grounded(False),Rp).

mkName(Lc,Nm,name(Lc,VrNm)) :-
  genstr(Nm,VrNm).

rtn(Vl,grounded(_),Vl).
rtn(_,lyfted(St),St).

passThru(grounded(X),X).
passThru(lyfted(X),X).

push(Lc,Bnd,grounded(St),Res) :-
  binary(Lc,"_push",Bnd,St,Res).
push(_,_,lyfted(St),St).

foldResult(Lc,F,E,grounded(St),Res) :-
  roundTerm(Lc,F,[E,St],Res).
foldResult(_,_,_,lyfted(St),St).

% create a conditional lambda:
% (El,St) => (Ptn.=El ?? push(Val,St) || St)

makeCondition(A,Lift,Succ,Zed,Rp) :-
  isSearch(A,Lc,Ptn,Src),!,
  genIden(Lc,"El",El),
  genIden(Lc,"St",St),
  roundTuple(Lc,[El,St],S),
  call(Succ,grounded(St),Sc),
  call(Lift,grounded(St),Lf),
  match(Lc,Ptn,El,Tst),
  conditional(Lc,Tst,Sc,Lf,Body),
  mkEquation(Lc,S,none,Body,FF),
  call(Lift,Zed,ZZ),
%  unary(Lc,"_more",ZZ,ZI),
  ternary(Lc,"_iter",Src,ZZ,FF,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isConjunct(A,_,Lhs,Rhs),!,
  makeCondition(Lhs,Lift,macroRules:makeCondition(Rhs,Lift,Succ),Zed,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isDisjunct(A,_,Lhs,Rhs),!,
  makeCondition(Lhs,Lift,Succ,Zed,E1),
  makeCondition(Rhs,Lift,Succ,lyfted(E1),Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isNegation(A,Lc,I),!,
  mkEnum(Lc,"true",True),
  mkEnum(Lc,"false",False),
  makeCondition(I,Lift,macroRules:rtn(True),grounded(False),Negated),
  call(Succ,Zed,Ok),
  call(Lift,Zed,NotOk),
  conditional(Lc,Negated,NotOk,Ok,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isForall(A,Lc,Lhs,Rhs),!,
  negation(Lc,Rhs,RR),
  conjunct(Lc,Lhs,RR,R1),
  negation(Lc,R1,RI),
  makeCondition(RI,Lift,Succ,Zed,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isTuple(A,[I]),!,
  makeCondition(I,Lift,Succ,Zed,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  locOfAst(A,Lc),
  call(Succ,Zed,ZZ),
  call(Lift,Zed,OO),
  conditional(Lc,A,ZZ,OO,Rp).

coercionMacro(Term,expression,N) :-
  isCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  typeAnnotation(Lc,LT,R,N).
coercionMacro(Term,expression,N) :-
  isOptCoerce(Term,Lc,L,R),!,
  % (try _coerce(L):R catch { _ => unreachable })
  unary(Lc,"_coerce",L,LT),
  typeAnnotation(Lc,LT,R,O),
  isUnreachable(Unreach,Lc),
  eqn(Lc,name(Lc,"_"),Unreach,Ctch),
  mkTry(Lc,O,[Ctch],N).

multicatMacro(T,expression,Tx) :-
  isUnary(T,Lc,"*",I),!,
  unary(Lc,"_multicat",I,Tx).

uminusMacro(T,expression,Tx) :-
  isUnaryMinus(T,Lc,A),
  isInteger(A,_,Ix),!,
  IIx is -Ix,
  mkInteger(Lc,IIx,Tx).
uminusMacro(T,expression,Tx) :-
  isUnaryMinus(T,Lc,A),
  isFloat(A,_,Dx),!,
  MDx is -Dx,
  mkFloat(Lc,MDx,Tx).
uminusMacro(T,expression,Tx) :-
  isUnaryMinus(T,Lc,A),
  isBigInt(A,_,Bx),!,
  negateString(Bx,Mx),
  mkBigInt(Lc,Mx,Tx).
uminusMacro(T,expression,Tx) :-
  isUnaryMinus(T,Lc,A),!,
  unary(Lc,"__minus",A,Tx).

optionMatchMacro(T,expression,Tx) :-
  isOptionMatch(T,Lc,P,E),!,
  mkConApply(Lc,name(Lc,"some"),[P],SP),
  match(Lc,SP,E,Tx).

optionCondMacro(T,expression,Tx) :-
  isOptionCond(T,Lc,P,E),!,
  genIden(Lc,X),
  mkConApply(Lc,name(Lc,"some"),[X],SP),
  match(Lc,SP,P),
  conditional(Lc,P,X,E,Tx).

pullMacro(T,expression,Tx) :-
  isOptVal(T,Lc,E),!,
  unary(Lc,"pull_",E,Tx).

unwrapExpMacro(T,expression,Tx) :-
  isBinary(T,Lc,"^",L,R),isIden(L,_,Con),!,
  genIden(Lc,V),
  unary(Lc,Con,V,Ptn),
  match(Lc,Ptn,R,Mtch),
  mkWhere(Lc,V,Mtch,Tx).

pkgNameMacro(T,expression,string(Lc,P)) :-
  isName(T,Lc,"__pkg__"),
  lcPk(Lc,P).

macroLocationExp(T,expression,Loc) :-
  isName(T,Lc,"__loc__"),!,
  mkLoc(Lc,Loc).

/*
 assert C 
becomes
  assrt(C,"failed: C",Loc)
*/
assertMacro(T,_,Act) :-
  isIntegrity(T,Lc,Cond),!,
  makeAssert(Lc,Cond,Act).

makeAssert(Lc,Cond,Act) :-
  ast2String(Lc,Cond,Msg),
  locOfAst(Cond,CLc),
  dispLocation(CLc,Lcn),
  roundTerm(Lc,name(Lc,"assrt"),[Cond,Msg,string(Lc,Lcn)],Act).

/*
 show E 
becomes
  shwMsg(E,"E",Lc)
*/
showMacro(T,_,Act) :-
  isShow(T,Lc,Exp),!,
  makeShow(Lc,Exp,Act).

makeShow(Lc,Exp,Act) :-
  ast2String(Lc,Exp,Txt),
  dispLocation(Lc,Lcn),
  roundTerm(Lc,name(Lc,"shwMsg"),[Exp,Txt,string(Lc,Lcn)],Act).

/*
  trace E
becomes
  traceCall("Lc: "E"",E)
  */

traceMacro(T,expression,Exp) :-
  isTrace(T,Lc,Grd,E),!,
  ast2String(Lc,E,string(_,Txt)),
  locOfAst(E,ELc),
  dispLocation(ELc,Loc),
  ss_to_str(sq([ss(Loc),ss(":"),ss(Txt)]),Msg),
  roundTerm(Lc,name(Lc,"traceCall"),[string(Lc,Msg),Grd,E],Exp).

mkConses([],Lc,Nl) :-
  mkEnum(Lc,"nil",Nl).
mkConses([E|Es],Lc,L) :-
  mkConses(Es,Lc,LL),
  mkConApply(Lc,name(Lc,"cons"),[E,LL],L).
  
genArgsDisplay([],_,[]) :- !.
genArgsDisplay([A|As],Lc,[AA|Ls]) :-
  unary(Lc,"disp",A,AA),
  genArgsDisplay(As,Lc,Ls).

/*
   A[F:T] := R
  becomes
  A := _splice(A!,F,T,R)
*/

spliceAssignMacro(A,action,Act) :-
  isSplice(A,Lc,S,F,T,R),!,
  unary(Lc,"!",S,Src),
  nary(Lc,"_splice",[Src,F,T,R],Rep),
  assignment(Lc,S,Rep,Act).

/*
  A[Ix] := R
  becomes
  A := _put(A!,Ix,R)
*/
indexAssignMacro(A,_,Act) :-
  isAssignment(A,Lc,L,R),
  isIndexTerm(L,LLc,C,I),!,
  unary(LLc,"!",C,CC),
  ternary(LLc,"_put",CC,I,R,Repl),
  binary(Lc,":=",C,Repl,Act).

macroQuote(T,expression,Rp) :-
  isQuote(T,_,A),!,
  quoteExp(A,Rp).

quoteExp(A,I) :-
  isUnary(A,_,"$",I),!.
quoteExp(name(Lc,Id),I) :-
  mkLoc(Lc,Loc),
  binary(Lc,"_name",Loc,string(Lc,Id),I).
quoteExp(qnme(Lc,Id),I) :-
  mkLoc(Lc,Loc),
  binary(Lc,"_qnme",Loc,string(Lc,Id),I).
quoteExp(integer(Lc,Ix),I) :-
  mkLoc(Lc,Loc),
  binary(Lc,"_integer",Loc,integer(Lc,Ix),I).
quoteExp(float(Lc,Dx),I) :-
  mkLoc(Lc,Loc),
  binary(Lc,"_float",Loc,float(Lc,Dx),I).
quoteExp(char(Lc,Cp),I) :-
  mkLoc(Lc,Loc),
  binary(Lc,"_char",Loc,char(Lc,Cp),I).
quoteExp(string(Lc,Sx),I) :-
  mkLoc(Lc,Loc),
  binary(Lc,"_string",Loc,string(Lc,Sx),I).
quoteExp(tuple(Lc,Lb,Els),Rp) :-
  mkLoc(Lc,Loc),
  map(Els,macroRules:quoteExp,QEls),
  macroListEntries(Lc,QEls,R,nilGen,consGen),
  ternary(Lc,"_tuple",Loc,string(Lc,Lb),R,Rp).
quoteExp(app(Lc,O,A),Rp) :-
  mkLoc(Lc,Loc),
  quoteExp(O,Oq),
  quoteExp(A,Aq),
  ternary(Lc,"_apply",Loc,Oq,Aq,Rp).

/*
  A![Ix]
  becomes
  (A!)[Ix]
*/
binRefMacro(T,expression,Rp) :-
  isBinary(T,Lc,"!",L,B),
  isSquareTuple(B,_,[A]),!,
  cellRef(Lc,L,LR),
  squareTerm(Lc,LR,[A],Rp).

/*
   Lb..<Up
   becomes
   .range(Lc,Up,one)
*/
incRangeMacro(T,expression,Rp) :-
  isBinary(T,Lc,"..<",Lb,Up),!,
  mkConApply(Lc,name(Lc,"range"),[Lb,Up,name(Lc,"one")],Rp).

/*
   Lb..>Up
   becomes
   .range(Lc,Up,-one)
*/
decRangeMacro(T,expression,Rp) :-
  isBinary(T,Lc,"..>",Lb,Up),!,
  mkConApply(Lc,name(Lc,"range"),[Lb,Up,unary(Lc,"-",name(Lc,"one"))],Rp).

/*
   Lb..<Up:St
  becomes
  .range(Lb,Up,St)
*/
rangeMacro(T,expression,Rp) :-
  isBinary(T,Lc,":",L,St),
  isBinary(L,_,"..<",Lb,Up),!,
  mkConApply(Lc,name(Lc,"range"),[Lb,Up,St],Rp).

/*
  for P in C do B
  becomes
  {
    I .= _generate(C);
    lb{ while .true do{
      case I resume ._next in {
        _yld(P) => B.
        _yld(_) default => {}.
        ._all => break lb
      }
    }
  }
  }
*/
forLoopMacro(A,action,Ax) :-
  isForDo(A,Lc,P,C,Bd),!,
  genIden(Lc,I),
  genIden(Lc,Lb),

  mkEnum(Lc,"true",True),

  /* Build :_all => break Lb */
  mkBreak(Lc,Lb,Brk),
  mkEnum(Lc,"_all",All),
  mkEquation(Lc,All,none,Brk,EndEq),

  /* build :_yld(P) => B */
  mkConApply(Lc,name(Lc,"_yld"),[P],BYld),
  mkEquation(Lc,BYld,none,Bd,YldEqn),

  /* build :_yld(_) default => {} */
  braceTuple(Lc,[],Nop),
  mkAnon(Lc,Anon),
  mkConApply(Lc,name(Lc,"_yld"),[Anon],DYld),
  mkDefault(Lc,DYld,Dflt),
  mkEquation(Lc,Dflt,none,Nop,DefltEqn),

  /* build case I resume ._next in .. */
  mkEnum(Lc,"_next",Next),
  mkResume(Lc,I,Next,G),
  caseExp(Lc,G,[YldEqn,DefltEqn,EndEq],Rsme),
  braceTuple(Lc,[Rsme],Resume),

  /* Build while .true loop */
  mkWhileDo(Lc,True,Resume,Loop),

  /* Build Lb:while .true do .. */
  mkLbldAction(Lc,Lb,[Loop],Lbld),

  /* Build call to _generate */
  roundTerm(Lc,name(Lc,"_generate"),[C],IT),
  mkDefn(Lc,I,IT,S1),
  mkSequence(Lc,[S1,Lbld],Ax).

/*
  for P : G do B
  becomes
  lb{
  while .true do{
  case G resume ._next in {
    | _yld(P) => B
    | _yld(_) default => {}
    | ._all => break lb
  }
  }
  }
  */

forMacro(A,action,Ax) :-
  isBinary(A,Lc,":",LL,RR),
  isUnary(LL,_,"for",P),
  isBinary(RR,_,"do",G,B),!,

  genIden(Lc,Lb),

  mkEnum(Lc,"true",True),

  /* Build :_all => break Lb */
  mkBreak(Lc,Lb,Brk),
  mkEnum(Lc,"_all",All),
  mkEquation(Lc,All,none,Brk,EndEq),

  /* build :_yld(P) => B */
  mkConApply(Lc,name(Lc,"_yld"),[P],BYld),
  mkEquation(Lc,BYld,none,B,YldEqn),

  /* build :_yld(_) default => {} */
  braceTuple(Lc,[],Nop),
  mkAnon(Lc,Anon),
  mkConApply(Lc,name(Lc,"_yld"),[Anon],DYld),
  mkDefault(Lc,DYld,Dflt),
  mkEquation(Lc,Dflt,none,Nop,DefltEqn),

  /* build case _resume(G,._next) in .. */
  mkEnum(Lc,"_next",Next),
  mkResume(Lc,G,Next,GV),
  caseExp(Lc,GV,[YldEqn,DefltEqn,EndEq],Rsme),
  braceTuple(Lc,[Rsme],Resume),

  /* Build while .true loop */
  mkWhileDo(Lc,True,Resume,Loop),

  /* Build Lb { while .true do .. } */
  mkLbldAction(Lc,Lb,[Loop],Ax).

/* generator{A}
   becomes
   _fiber((this,_) => valof{
     A*;
   valis .all
   })
*/
   
generatorMacro(E,expression,Ex) :-
  isGenerator(E,Lc,B),!,

  mkEnum(Lc,"_all",All),
  mkValis(Lc,All,Rt),

  mkSequence(Lc,B,Rt,TB),
  braceTuple(Lc,[TB],VB),
  mkValof(Lc,VB,GVl),

  roundTuple(Lc,[name(Lc,"this"),name(Lc,"_")],LmLhs),
  mkEquation(Lc,LmLhs,none,GVl,Eqn),
  unary(Lc,"_fiber",Eqn,Ex).
%  dispAst(Ex).

/* yield E
   becomes
   case this suspend ._yld(E) in {
     ._next do {}.
     ._cancel do retire ._all
   }
*/
yieldMacro(E,action,Ax) :-
  isUnary(E,Lc,"yield",A),!,

  mkConApply(Lc,name(Lc,"_yld"),[A],Yld),

  /* build ._next => {} */
  braceTuple(Lc,[],Nop),
  mkEnum(Lc,"_next",Nxt),
  mkProcedure(Lc,Nxt,none,Nop,NxtRl),

  /* build ._cancel => _retire(this,._all) */
  mkEnum(Lc,"_cancel",Can),
  mkEnum(Lc,"_all",All),
  mkRetire(Lc,name(Lc,"this"),All,Rs),
  mkProcedure(Lc,Can,none,Rs,Cancel),

  /* Build suspend */
  mkSuspend(Lc,name(Lc,"this"),Yld,SS),
  caseExp(Lc,SS,[NxtRl,Cancel],Ax).

/* task { A }

  becomes

    tsk(this, let{
      tk:async () => _ throws _.
      tk() => valof { A }
      } in ζ tk)

  where tsk is a library function defined in mbox.
  */

taskMacro(E,expression,Rp) :-
  isTask(E,Lc,A),!,
  genIden(Lc,"tk",Tk),
  Anon = name(Lc,"_"),
  roundTuple(Lc,[],Empty),

  % Build type annotation:
  % tk:async () => _ throws _.

  mkThrows(Lc,Anon,Anon,Rslt),
  funcType(Lc,Empty,Rslt,FnT),
  unary(Lc,"async",FnT,FnTp),

  mkTypeDecl(Lc,Tk,FnTp,St1),

    % Build function:
    % tk() => valof { A }

  braceTuple(Lc,[A],AB),
  mkValof(Lc,AB,V),
  buildEquation(Lc,Tk,[],none,false,V,St2),

    % Build let defn
  mkSuppress(Lc,Tk,Bnd),
  mkLetDef(Lc,[St1,St2],Bnd,LetFn),

    % Build call to tsk
  roundTerm(Lc,name(Lc,"tsk"),[name(Lc,"this"),LetFn],Rp).
%    dispAst(Rp).


/* collect { A }

  becomes

    valof{
     $result := _null;
     A;
     valis $result
     }
 */

     
collectMacro(E,expression,Rp) :-
  isCollect(E,Lc,A),!,
  genIden(Lc,"result",Res),

  /* Build $result := _null */
  assignment(Lc,Res,name(Lc,"_null"),A1),

  foldOverAction(macroRules:replaceElemis(Res),A,A2),

  /* Build valis $result! */
  cellRef(Lc,Res,Final),
  mkValis(Lc,Final,A3),

  braceTuple(Lc,[A1,A2,A3],B),
  mkValof(Lc,B,Rp).

/* elemis E is replaced by

   Res := _cons(E,Res!)
*/

replaceElemis(Res,A,Ax) :-
  isElemis(A,Lc,E),
  cellRef(Lc,Res,Rf),
  binary(Lc,"_push",E,Rf,NRes),
  assignment(Lc,Res,NRes,Ax).

/*
  K -> V
becomes
  .kv(K,V)
*/
arrowMacro(E,Md,Rp) :- (Md=expression;Md=pattern),
  isBinary(E,Lc,"->",K,V),!,
  mkConApply(Lc,name(Lc,"kv"),[K,V],Rp).

/*
 f(A)(K) => E
becomes
 f(A) => (K)=>E
*/

curryMacro(T,Mode,Tx) :-
  (Mode=expression ; Mode=statement),
  isEquation(T,Lc,Lhs,Cond,Rhs),
  isRoundTerm(Lhs,OLc,Op,KArgs),
  isRoundTerm(Op,OOLc,F,FArgs),!,
  roundTuple(OLc,KArgs,LmLhs),
  mkEquation(Lc,LmLhs,Cond,Rhs,CRle),
  roundTerm(OOLc,F,FArgs,RLhs),
  mkEquation(Lc,RLhs,none,CRle,Tx).

/* { .. Ptn:Type => Exp}
   in case rule becomes
   { .. (Ptn:Type) => Exp }
*/

caseRuleMacro(T,_,Tx) :-
  isTypeAnnotation(T,Lc,L,R),
  isBinary(R,LLc,"=>",A,B),!,
  typeAnnotation(Lc,L,A,Ptn),
  binary(LLc,"=>",Ptn,B,Tx).

asyncMacro(T,type,Tx) :-
  isUnary(T,Lc,"async",R),!,
  mkSqType(Lc,"task",[name(Lc,"_")],TTp),
  mkDynamic(Lc,"this",TTp,CTp),
  binary(Lc,"|=",CTp,R,Tx).
