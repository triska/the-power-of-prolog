/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Client/server based animal identification using Pengines.
   Written 2016, 2018 by Markus Triska (triska@metalevel.at)


   For more information, see:

          https://www.metalevel.at/prolog/web
          ===================================

   Sample interaction:

    ?- main.
    has fur?
    |: yes.
    says woof?
    |: no.
    says meow?
    |: yes.
    identified(cat).
    true.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(pengines)).

main :-
        pengine_create([
                        server('http://localhost:6357')
                       ]),
        pengine_event_loop(handle, []).

handle(prompt(ID, Q)) :-
        format("~w?\n", [Q]),
        read(Answer),
        pengine_respond(ID, Answer, []).
handle(create(ID, _)) :-
        pengine_ask(ID, animal(_), []).
handle(success(_, [animal(A)], false)) :-
        portray_clause(identified(A)).
handle(success(ID, [animal(A)], true)) :-
        portray_clause(identified(A)),
        pengine_next(ID, []).
handle(failure(_)) :-
        portray_clause(identification_failed).
