/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Client/server based animal identification using Pengines.
   Written 2016, 2018 by Markus Triska (triska@metalevel.at)

   For more information, see:

          https://www.metalevel.at/prolog/web
          ===================================

   The logic is all in the server (i.e., this file).

   To try this example:

     1) start the server with:

        $ swipl animal_server.pl --port=6357 --interactive

     2) start the client with:

        $ swipl -g main animal_client.pl


   Tested with SWI-Prolog 7.7.14.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(pengines)).
:- use_module(library(http/http_unix_daemon)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Main logic: Completely identical to the stand-alone expert system.

   For more information, see:

            https://www.metalevel.at/prolog/expertsystems
            =============================================

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tree(if_then_else('has fur',
                  if_then_else('says woof',
                               identified(dog),
                               if_then_else('says meow',
                                            identified(cat),
                                            false)),
                  if_then_else('has feathers',
                               if_then_else('says quack',
                                            identified(duck),
                                            false),
                               false))).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpreter for the DSL.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

animal(A) :-
        tree(T),
        tree_animal(T, A).

tree_animal(identified(A), A).
tree_animal(if_then_else(Cond,Then,Else), A) :-
        (   is_true(Cond) ->
            tree_animal(Then, A)
        ;   tree_animal(Else, A)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Only this depends on Pengines: Instead of asking on the terminal,
   we ask the client!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

is_true(Question) :-
        pengine_input(Question, yes).
