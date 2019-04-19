:- initialization top.

:- use_module(driver).

eval :-
        current_prolog_flag(argv, Argv),
        main(Argv).

top :-
        catch(eval, E, (print_message(error, E), fail)),
        halt(0).
top :-
        halt(1).
