/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   This was our initial program:

   pet(dog) :- is_true('has fur'), is_true('goes woof').
   pet(cat) :- is_true('has fur'), is_true('goes meow').
   pet(duck) :- is_true('has feathers'), is_true('goes quack').

   is_true(Question) :-
           pengine_input(Question, yes).

   The drawback is clear: Some questions may be asked redundantly.

   Therefore, we consider an alternative. First, we switch everything
   to a domain-specific language (DSL), so that we can more easily
   reason about the program.

   Note in particular that we use *lists* of goals instead of a
   defaulty representation:

   pets([pet(dog, [ask('has fur'), ask('says woof')]),
         pet(cat, [ask('has fur'), ask('says meow')]),
         pet(duck, [ask('has feathers'), ask('says quack')]) ]).

   This obviously has the same problem as before. However, we can
   somewhat more easily reason about it. For example, let us obtain a
   REDUCED decision diagram for this!

   We can use CLP(B) constraints to minimize the decision diagram: In
   a BDD, no node occurs redundantly, and all isomorphic subgraphs are
   merged.

   For a rudimentary example, see:

   http://swish.swi-prolog.org/p/pets_clpb.pl

   This is not fully worked out, but could be worked out in principle.

   In the following, we assume that we have somehow constructed such a
   reduced decision diagram to identify animals. Once obtained, we
   represent each branching node as:

       (  If -> Then ; Else )

   This is the same as in Prolog, so why are we using a DSL for this?

   Easy: Because we can INTERPRET this in any way we wish: Instead of
   asking on the terminal, we could also send a query to a client,
   look it up in a database, get clues from a scanned photograph etc.

   So the decision tree represented in this DSL follows:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pets((   ask('has fur') ->
         (   ask('says woof') -> say(dog)
         ;   ask('says meow') -> say(cat)
         ;   false
         )
     ;   ask('has feathers') ->
         (   ask('says quack') -> say(duck)
         ;   false
         )
     ;   false
     )).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Now the interpretation of the tree.

   Note that by keeping this separate from the actual logic, we retain
   a lot of flexibility.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

run(( If -> Then ; Else)) :-
        (   run(If) -> run(Then)
        ;   run(Else)
        ).
run(ask(Q)) :-
        format("~w\n", [Q]),
        read(yes).
run(say(S)) :-
        format("it's a ~w!\n", [S]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   Sample queries and answers:

   ?- pets(P), run(P).
   %@ has fur
   %@ |: yes.
   %@ says woof
   %@ |: yes.
   %@ it's a dog!
   %@ P =  (ask('has fur')->(ask('says woof')->say(dog);ask('says meow')->say(cat);false);ask('has feathers')->(ask('says quack')->say(duck);false);false).

   ?- pets(P), run(P).
   %@ has fur
   %@ |: yes.
   %@ says woof
   %@ |: no.
   %@ says meow
   %@ |: yes.
   %@ it's a cat!
   %@ P =  (ask('has fur')->(ask('says woof')->say(dog);ask('says meow')->say(cat);false);ask('has feathers')->(ask('says quack')->say(duck);false);false).


   ?- pets(P), run(P).
   %@ has fur
   %@ |: no.
   %@ has feathers
   %@ |: yes.
   %@ says quack
   %@ |: no.
   %@ false.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
