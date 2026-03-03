:- initialization(top,main).

:- use_module(driver).

eval :-
        current_prolog_flag(argv, Argv),
        main(Argv),
	halt.

top :-
        catch(eval, E, (print_message(error, E), fail)),
        halt.
top :-
        halt(1).
