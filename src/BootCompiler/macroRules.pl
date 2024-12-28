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
%macroRl("^",expression,macroRules:unwrapExpMacro).
%macroRl("^",expression,macroRules:optvalMacro).
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
macroRl("-->",statement,macroRules:grammarMacro).
macroRl("-->",expression,macroRules:grammarCallMacro).
macroRl("-->",type,macroRules:grammarTypeMacro).
macroRl("raises",type,macroRules:raisesMacro).
macroRl("async",type,macroRules:asyncMacro).

build_main(As,Bs) :-
  look_for_signature(As,"main",Lc,Ms),
  \+look_for_signature(As,"_main",_,_),!,
  synthesize_main(Lc,Ms,As,Bs).
build_main(A,A).

look_for_signature([St|_],Nm,Lc,Ms) :-
  (isTypeAnnotation(St,Lc,V,T) ;
   (isPublic(St,_,I),isTypeAnnotation(I,Lc,V,T))),
  isIden(V,_,Nm),
  isFuncType(T,_,L,_),
  isTuple(L,_,Ms),!.
look_for_signature([_|As],Nm,Lc,Ms) :-
  look_for_signature(As,Nm,Lc,Ms).

synthesize_main(Lc,Ts,As,[MainTp,Main|As]) :-
  synthesize_coercions(Ts,Vs,Cs),
  list_pttrn(Lc,Vs,Arg),
  roundTerm(Lc,name(Lc,"_main"),[Arg],Lhs),
  roundTerm(Lc,name(Lc,"main"),Cs,MnCall),
%  mkValof(Lc,Rhs,MnCall),
  eqn(Lc,Lhs,MnCall,Main),
  squareTerm(Lc,name(Lc,"cons"),[name(Lc,"string")],T1),
  roundTuple(Lc,[T1],T3),
  roundTuple(Lc,[],Unit),
  binary(Lc,"=>",T3,Unit,TU),
  binary(Lc,":",name(Lc,"_main"),TU,MainTp).
%  dispAst(Main).
  
synthesize_coercions([],[],[]).
synthesize_coercions([T|Ts],[V|Vs],[C|Cs]) :-
  locOfAst(T,Lc),
  genIden(Lc,V),
  coerce(Lc,V,T,C),
  synthesize_coercions(Ts,Vs,Cs).

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
  unary(Lc,"_optval",LT,OLT),
  binary(Lc,":",OLT,R,N).
coercionMacro(Term,expression,N) :-
  isOptCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  sqUnary(Lc,"option",R,OR),
  binary(Lc,":",LT,OR,N).

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

optvalMacro(T,expression,Tx) :-
  isOptVal(T,Lc,E),!,
  unary(Lc,"_optval",E,Tx).

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
  unary(Lc,"_name",string(Lc,Id),I).
quoteExp(qnme(Lc,Id),I) :-
  unary(Lc,"_qnme",string(Lc,Id),I).
quoteExp(integer(Lc,Ix),I) :-
  unary(Lc,"_integer",integer(Lc,Ix),I).
quoteExp(float(Lc,Dx),I) :-
  unary(Lc,"_float",float(Lc,Dx),I).
quoteExp(char(Lc,Cp),I) :-
  unary(Lc,"_char",char(Lc,Cp),I).
quoteExp(string(Lc,Sx),I) :-
  unary(Lc,"_string",string(Lc,Sx),I).
quoteExp(tuple(Lc,Lb,Els),Rp) :-
  map(Els,macroRules:quoteExp,QEls),
  macroListEntries(Lc,QEls,R,nilGen,consGen),
  binary(Lc,"_tuple",string(Lc,Lb),R,Rp).
quoteExp(app(Lc,O,A),Rp) :-
  quoteExp(O,Oq),
  quoteExp(A,Aq),
  binary(Lc,"_apply",Oq,Aq,Rp).

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
  for P in C do B
  becomes
  {
    I .= _generate(C);
    lb:while .true do{
      case _resume(I,._next) in {
        _yld(P) => B.
        _yld(_) default => {}.
        ._all => break lb
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

   /* build case _resume(I,._next) in .. */
   mkEnum(Lc,"_next",Next),
   binary(Lc,"_resume",I,Next,G),
   caseExp(Lc,G,[YldEqn,DefltEqn,EndEq],Rsme),
   braceTuple(Lc,[Rsme],Resume),

   /* Build while .true loop */
   mkWhileDo(Lc,True,Resume,Loop),

   /* Build Lb:while .true do .. */
   mkLbldAction(Lc,Lb,Loop,Lbld),

   /* Build call to _generate */
   roundTerm(Lc,name(Lc,"_generate"),[C],IT),
   mkDefn(Lc,I,IT,S1),

   mkSequence(Lc,[S1,Lbld],Ax).

  /*
  for P : G do B
  becomes
  {
    lb:while .true do{
  case _resume(G,._next) in {
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
   binary(Lc,"_resume",G,Next,GV),
   caseExp(Lc,GV,[YldEqn,DefltEqn,EndEq],Rsme),
   braceTuple(Lc,[Rsme],Resume),

   /* Build while .true loop */
   mkWhileDo(Lc,True,Resume,Loop),

   /* Build Lb:while .true do .. */
   mkLbldAction(Lc,Lb,Loop,Ax).

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
   case _suspend(this,._yld(E)) in {
     ._next => {}.
     ._cancel => _retire(this,._all)
   }
*/
yieldMacro(E,action,Ax) :-
  isUnary(E,Lc,"yield",A),!,

  mkConApply(Lc,name(Lc,"_yld"),[A],Yld),

  /* build ._next => {} */
  braceTuple(Lc,[],Nop),
  mkEnum(Lc,"_next",Nxt),
  mkEquation(Lc,Nxt,none,Nop,NxtRl),

  /* build ._cancel => _retire(this,._all) */
  mkEnum(Lc,"_cancel",Can),
  mkEnum(Lc,"_all",All),
  binary(Lc,"_retire",name(Lc,"this"),All,Rs),
  mkEquation(Lc,Can,none,Rs,Cancel),

  /* Build suspend */
  binary(Lc,"_suspend",name(Lc,"this"),Yld,SS),
  caseExp(Lc,SS,[NxtRl,Cancel],Ax).

  /* task { A }

  becomes

  _fiber((this,Frst)=> case Frst in {
      | .go_ahead => .result(valof{A})
      | .shut_down => raise .canceled
    })

  */

  taskMacro(E,expression,Rp) :-
    isTask(E,Lc,A),!,
    genIden(Lc,"Frst",Frst),

    /* Build .shut_down_ => raise .canceled */
    mkEnum(Lc,"shut_down_",Down),
    mkEnum(Lc,"canceled",Can),
    mkRaise(Lc,Can,Raise),
    mkEquation(Lc,Down,none,Raise,End),

    /* build .go_ahead => .result(valof{A}) */
    mkEnum(Lc,"go_ahead",Ahead),

    mkValof(Lc,A,V),
    mkConApply(Lc,name(Lc,"result"),V,Vl),
    mkEquation(Lc,Ahead,none,Vl,Go),

    /* build case Frst in .. */
    caseExp(Lc,Frst,[End,Go],Body),
    roundTuple(Lc,[name(Lc,"this"),Frst],Args),

    mkEquation(Lc,Args,none,Body,Lm),
    unary(Lc,"_fiber",Lm,Rp).


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
  isBinary(T,Lc,":",L,R),
  isBinary(R,LLc,"=>",A,B),!,
  binary(Lc,":",L,A,Ptn),
  binary(LLc,"=>",Ptn,B,Tx).

raisesMacro(T,type,Tx) :-
  isBinary(T,Lc,"raises",L,R),!,
  unary(Lc,"raises",R,E),
  binary(Lc,"|:",E,L,Tx).

asyncMacro(T,type,Tx) :-
  isUnary(T,Lc,"async",R),!,
  mkSqType(Lc,"task",[name(Lc,"_")],TTp),
  typeAnnotation(Lc,name(Lc,"this"),TTp,CTp),
  binary(Lc,"|:",CTp,R,Tx).
