:- module(lambdalift,[transformProg/3]).

/*
 * Implement a lambda lifting transformation to reduce (slightly) the semantics to
 * a flat language
 */

:- use_module(canon).
:- use_module(errors).
:- use_module(types).
:- use_module(debug).
:- use_module(matcher).
:- use_module(misc).
:- use_module(escapes).
:- use_module(location).
:- use_module(freevars).

/*
  Functions are converted to top-level functions with explicit parameters containing
  free variables.

  E.g. add(X) => a(Y)=>X+Y.

  is converted to (assuming add is already top-level):

  add(X) => lambdaUU(X)

  together with a top-level function:

  lambdaUU(lambdaUU(X),Y) => X+Y.

  A call to a variable function:

  apply(F,X) => F(X)

  becomes

  apply(F,X) => call(F,X)

  where call is swi-prolog's call(F,A1,..,Ak)
*/

liftTheta(Q,F,Path,theta(Lc,Lbl,Defs,Others,Types,Sig),theta(Lc,Lbl,LDefs,LOthers,Types,Sig)) :-
  freeVars(theta(Lc,Lbl,Defs,Others,Types,Sig),Q,[],F0),
  genLbl("theta",F0,ThLbl),
  subPath(Path,".",Lbl,SubPath),
  liftDefs(Q,F0,ThLbl,SubPath,Defs,LDefs),
  liftOthers(Q,F0,ThLbl,SubPath,Others,LOthers).

liftDefs(_,_,_,_,[],[]).
liftDefs(Q,F,Lbl,SubPath,[D|Defs],LDefs) :-
  liftDef(Q,F,Lbl,SubPath,D,LDefs,L0),
  liftDefs(Q,F,Lbl,SubPath,Defs,L0).

liftDef(Q,F,Lbl,SubPath,varDef(Lc,Nm,[],Tp,Value),[varDef(Lc,Nm,[],Tp,Value)|Dx],Dx) :-
