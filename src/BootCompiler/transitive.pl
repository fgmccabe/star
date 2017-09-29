:- module(transitive,[closure/5]).

:- use_module(misc).

closure([],Result,_,_,Result).
closure([E|More],SoFar,Test,Proc,Result) :-
  call(Test,E,SoFar),!,
  call(Proc,E,SoFar,Next,More,Input),
  closure(Input,Next,Test,Proc,Result).
closure([_|More],SoFar,Test,Proc,Result) :-
  closure(More,SoFar,Test,Proc,Result).
