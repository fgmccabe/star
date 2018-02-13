:- module(template,[processTemplateFile/3,buildTemplateVars/2,processTemplate/3,procTmpl/4]).

:- use_module(misc).
:- use_module(resource).
:- use_module(uri).

processTemplateFile(Url,Vars,Out) :-
  parseURI(Url,U),
  locateResource(U,Plate),
  buildTemplateVars(Vars,Dict),
  processTemplate(Plate,Dict,Out).

processTemplate(Text,Vars,Out) :-
  string_chars(Text,Chrs),
  procTmpl(Chrs,Vars,O,[]),
  string_chars(Out,O).

procTmpl([],_,Ox,Ox) :-!.
procTmpl(['#','#','('|L],D,O,Ox) :- !,
  getVar(L,R,V),
  lookupVar(V,D,Vl),
  procTmpl(Vl,D,O,O1),
  procTmpl(R,D,O1,Ox).
procTmpl(['#','('|L],D,O,Ox) :- !,
  getVar(L,R,V),
  lookupVar(V,D,Vl),
  appStr(Vl,O,O1),
  procTmpl(R,D,O1,Ox).
procTmpl(['#','*','('|L],D,O,Ox) :- !,
  getVar(L,R,V),
  lookupVar(V,D,Vl),
  appStrs(Vl,O,O1),
  procTmpl(R,D,O1,Ox).
procTmpl([C|L],D,[C|O],Ox) :-
  procTmpl(L,D,O,Ox).

getVar(L,R,Vr) :-
  getVr(L,R,V),!,
  string_chars(Vr,V).

getVr([')'|R],R,[]).
getVr([C|L],R,[C|V]) :-
  getVr(L,R,V).

lookupVar(V,D,Vl) :-
  get_dict(V,D,Vl).

emptyTVars(D) :-
  put_dict("#",vars{},"#",D0), % This allows #(#) to be just #
  put_dict("##",D0,"##",D).    % This allows #(##) to name ##

defineTemplVar((Nm,Vl),D,Dx) :-
  put_dict(Nm,D,Vl,Dx).

buildTemplateVars(V,D) :-
  emptyTVars(I),
  rfold(V,template:defineTemplVar,I,D).
