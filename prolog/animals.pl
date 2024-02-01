
:- use_module(library(reif)).
:- use_module(library(dcgs)).
:- use_module(library(format)).

/*
  Initial version

animal(dog)  :- is_true("has fur"), is_true("goes woof").
animal(cat)  :- is_true("has fur"), is_true("goes meow").
animal(duck) :- is_true("has feathers"), is_true("goes quack").

*/

is_true(Q) :-
	format("~s?\n", [Q]),
	read(yes).

/* interaction

?- animal(A).
%@ has fur?
%@ |: yes.
%@ goes woof?
%@ |: no.
%@ has fur?
%@ |: yes.
%@ goes meow?
%@ |: yes.
%@ 
%@ A = cat .
has fur?

*/

/*

  Expert system, capturing the essence of animal identification.

 */

animals([animal(dog, [is_true("has fur"), is_true("says woof")]),
         animal(cat, [is_true("has fur"), is_true("says meow")]),
         animal(duck, [is_true("has feathers"), is_true("says quack")])]).

/*

  Vanilla interpeter for these rules:

    animal(A) :-
            animals(As),
            member(animal(A,Cs), As),
            maplist(call, Cs).


    is_true(Q) :-
            format("~s?\n", [Q]),
            read(yes).

  Drawback: questions may appear redundantly:


   %?- animal(A).
   %@ has fur?
   %@ |: yes.
   %@ says woof?
   %@ |: no.
   %@ has fur?
   %@ |: yes.
   %@ says meow?
   %@ |: yes.
   %@ 
   %@ A = cat .

*/




/*
  Interpreter for animal identification rules, with memory.

  DCG semicontext notation is used to *implicitly* thread through
  what is already known.

  For more information, see:

  https://www.metalevel.at/prolog/dcg
  ===================================

  In our case, the state we are threading through is a *list*
  of known(Question,Truth) term. We start with [].

*/

animal(A) :-
        animals(Animals),
        Known0 = [],
        phrase(any_animal(Animals, A), [Known0], _).

any_animal([Animal|Animals], A) -->
        any_animal_(Animal, Animals, A).

any_animal_(animal(A0, []), Animals, A) -->
        (   { A0 = A }
        ;   any_animal(Animals, A)
        ).
any_animal_(animal(A0, [C|Cs]), Animals, A) -->
        state0_state(Known0, Known),
        { condition_truth(C, T, Known0, Known) },
        next_animal(T, animal(A0,Cs), Animals, A).

next_animal(yes, Animal, Animals, A)  --> any_animal([Animal|Animals], A).
next_animal(no, _, Animals, A)        --> any_animal(Animals, A).

/*
  The nonterminal state0_state(S0, S) is read as:
  "Currently, the state is S0, and henceforth, it is S."
*/


state0_state(S0, S), [S] --> [S0].

/*
  Platform-dependent part: Ask user (on terminal, over web etc.)

  We distinguish the cases with if_/3.

  Implementation:
  SICStus:
  http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/sicstus/reif.pl
  SWI:
  http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/swi/reif.pl

*/

condition_truth(is_true(Q), Truth, Known0, Known) :-
        if_(known_(Q,Truth,Known0),
            Known0 = Known,
            ( format("~s?\n", [Q]),
              read(Truth),
              Known = [known(Q,Truth)|Known0])).

known_(What, Answer, Known, Truth) :-
        if_(memberd_t(known(What,yes), Known),
            ( Answer = yes, Truth = true ),
            if_(memberd_t(known(What,no), Known),
                ( Answer = no, Truth = true),
                Truth = false)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- if_(memberd_t(known(fur,true), [known(fur,false)]),
      T=yes,
      T=unknown).
%@ F = T, T = true 
%@ F = T, T = true 
%@ F = T, T = true
%?- memberd_t(known(What,_), [known(fur,_)], false).
%?- tmember(known, [A]).

%?- call_residue_vars(known_(fur, T, [known(fur,yes)], T0), Vs).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
  Sample interaction:

  ?- animal(A).
  %@ has fur?
  %@ |: yes.
  %@ says woof?
  %@ |: no.
  %@ says meow?
  %@ |: no.
  %@ has feathers?
  %@ |: no.
  %@ has fur?
  %@ |: yes.
  %@ says meow?
  %@ |: no.
  %@ has feathers?
  %@ |: yes.
  %@ says quack?
  %@ |: no.
  %@ 
  %@ false.

  
  %@ has fur?
  %@ |: yes.
  %@ says woof?
  %@ |: no.
  %@ says meow?
  %@ |: no.
  %@ has feathers?
  %@ |: yes.
  %@ says quack?
  %@ |: no.
  %@ has fur?
  %@ |: yes.
  %@ 
  %@ false.
  %@ has fur?
  %@ |: yes.
  %@ says woof?
  %@ |: yes.
  %@ 
  %@ A = dog ;
  %@ says meow?
  %@ |: yes.
  %@ 
  %@ A = cat ;
  %@ has feathers?
  %@ |: no.
  %@ 
  %@ false.


*/

tree(if_then_else("has fur",
                  if_then_else("says woof",
                               identified(dog),
                               if_then_else("says meow",
                                            identified(cat),
                                            false)),
                  if_then_else("has feathers",
                               if_then_else("says quack",
                                            identified(duck),
                                            false),
                               false))).


animal(A) :-
        tree(T),
        tree_animal(T, A).

tree_animal(identified(A), A).
tree_animal(if_then_else(Cond,Then,Else), A) :-
        (   is_true(Cond) ->
            tree_animal(Then, A)
        ;   tree_animal(Else, A)
        ).

is_true(Q) :-
	format("~s?\n", [Q]),
	read(yes).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- animal(A).
has fur?
|: yes.
says woof?
|: no.
says meow?
|: yes.

A = cat.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
