:- module(compmacro,[compMacroRule/2]).

:- use_module(misc).
:- use_module(wff).
:- use_module(abstract).
:- use_module(errors).

compMacroRule(Rl,Cd) :-
  isMacroRule(Rl,Lc,Lhs,Rhs),
  compilePtn(Lhs,Ptn)
