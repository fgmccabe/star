:- module(macro,[macroRewrite/2]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(polyfill).
:- use_module(display).

macroRewrite(Stmts,Reslt) :-
  rewriteStmts(Stmts,Reslt).
  % displayAll(Reslt).

rewriteStmts([],[]).
rewriteStmts([St|More],[StX|Stmts]) :-
  rewriteStmt(St,StX),
  rewriteStmts(More,Stmts).
rewriteStmts([St|More],[St|Stmts]) :-
  rewriteStmts(More,Stmts).

% a stub for now
rewriteStmt(X,X).
