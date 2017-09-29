:- module(debug,[debugPreamble/7,frameDebug/6,deframeDebug/5,breakDebug/4,lineDebug/4]).

:- use_module(misc).
:- use_module(location).
:- use_module(transutils).

isDebug(Opts) :-
  isOption(debugging,Opts).

% are we debugging?
debugPreamble(Nm,Q,Qx,[call(prg("go.debug@current",1),[DgVr])|Gx],Gx,Opts,ClOpts) :-
  isDebug(Opts),!,
  genVar("__D",DgVr),
  genVar("__F",FrVr),
  Qx = [DgVr,FrVr|Q],
  pushOpt(Opts,dbgVars(Nm,DgVr,FrVr),ClOpts).
debugPreamble(_,Q,Q,P,P,Opts,Opts).

frameDebug(Nm,QNo,G,Gx,Q,Opts) :-
  isOption(dbgVars(Nm,DgVr,FrVr),Opts),!,
  trCons("frame",3,RlNm),
  constructFrameList(Q,FQ),
  G=[unify(FrVr,FQ),
      ocall(cons(RlNm,[strg(Nm),intgr(QNo),FrVr]),DgVr,DgVr)|Gx].
frameDebug(_,_,F,F,_,_).

deframeDebug(Nm,QNo,FB,FBx,Opts) :-
  isOption(dbgVars(Nm,DgVr,FrVr),Opts),!,
  trCons("deframe",3,RlNm),
  FB=[ocall(cons(RlNm,[strg(Nm),intgr(QNo),FrVr]),DgVr,DgVr)|FBx].
deframeDebug(_,_,F,F,_).

constructFrameList([],enum("lo.list#[]")).
constructFrameList([idnt(V)|Vars],FQ) :-
  starts_with(V,"_"),!,
  constructFrameList(Vars,FQ).
constructFrameList([idnt(V)|Vars],
      cons(strct("lo.list#,..",2),[tpl([strg(V),idnt(V)]),Q])) :-
  constructFrameList(Vars,Q).

breakDebug(Nm,[ocall(cons(BC,[strg(Nm)]),DgVr,DgVr)|BG],BG,Opts) :-
  isOption(dbgVars(Nm,DgVr,_),Opts),!,
  trCons("break",1,BC).
breakDebug(_,G,G,_).

lineDebug(Lc,[ocall(cons(RlNm,[strg(PkgName),intgr(Lno),intgr(Off),intgr(Sz)]),DgVr,DgVr)|P],P,Opts) :-
  isOption(dbgVars(_,DgVr,_),Opts),!,
  trCons("line",4,RlNm),
  isOption(pkgName(PkgName),Opts),!,
  lcLine(Lc,Lno),
  lcColumn(Lc,Off),
  lcSize(Lc,Sz).
lineDebug(_,P,P,_).

