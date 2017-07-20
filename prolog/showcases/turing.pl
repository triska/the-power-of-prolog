/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Turing Machine in Prolog.
   Copyright (C) 2017 Markus Triska (triska@metalevel.at)

   This example demonstrates one way to describe the workings of a
   Turing machine, using a logic programming language like Prolog.

   The idea is to express the whole process as a relation between
   *states*. In our case, the state of interest is comprised of:

     -) the contents of the tape
     -) the position of the tape head.

   Given this state, we can look up what to do next, using a table of
   state transitions that encodes the rules of the Turing machine.

   In the following, the tape is encoded as 2 lists: One, called Ls,
   describes the tape contents to the _left_ of the tape head. The
   other, called Rs, describes the tape contents to the _right_ of the
   tape head. In addition, Ls is encoded "in reverse", and the first
   element of Rs holds the symbol under the tape head.

   For example, if the tape is:

        a b c d e f
        ===========
              A
              |
              +---------- tape head

   then we represent it as:

        Ls = [c,b,a]
        Rs = [d,e,f]

   The tape extends indefinitely to the right and to the left. Unless
   otherwise specified, each cell is blank, represented by the atom b.

   We start with Ls0 = [] and Rs0 = Tape0, which is the initial tape.
   If the machine reaches the final state called 'final', then we
   obtain Ls and Rs, which combined represent the result of the
   computation as the tape content after the computation.

   Any state S that is _not_ the final state is represented as s(S),
   using the symbolic wrapper s/1 that lets us cleanly distinguish the
   final state from any other state, and hence keep the code pure.

   We are using Prolog DCG notation to _implicitly_ thread through the
   current state of the tape, and relate every state to the state
   after the current step. For more information, see the DCG primer:

                 https://www.metalevel.at/prolog/dcg
                 ===================================

   You can supply your own definitions of tm/6 to represent any
   Turing machine:

        tm(+Name, +Q0, +Symbol0, -Q, -Symbol, -Action)

   The predicate is called with the following input arguments:

      * Name is the the name of the Turing machine that is
        currently running
      * Q0 is the machine's current state
      * Symbol0 is the symbol under the tape head

   The predicate relates these arguments to:

      * Q, the next state of the machine
      * Symbol, the symbol that is written at the current position
      * Action, which is either left, right, or stay, to indicate
        how to move the tape head.

   The main interface predicate is turing(+Name, +Tape0, -Tape).
   The initial state is called q0.

   This program shows that Prolog is a Turing complete language.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

turing(Name, Tape0, Tape) :-
        phrase(turing_(s(q0), Name), [[]-Tape0], [Ls-Rs]),
        reverse(Ls, Ls1),
        append(Ls1, Rs, Tape).

state(S0, S), [S] --> [S0].

turing_(final, _) --> [].
turing_(s(Q0), Name) -->
        state(Ls0-Rs0, Ls-Rs),
        { right_symbol_rest(Rs0, Symbol0, RsRest),
          tm(Name, Q0, Symbol0, Q, Symbol, Action),
          action(Action, Ls0, Ls, [Symbol|RsRest], Rs) },
        turing_(Q, Name).

action(left, Ls0, Ls, Rs0, Rs) :- left(Ls0, Ls, Rs0, Rs).
action(stay, Ls, Ls, Rs, Rs).
action(right, Ls0, [Symbol|Ls0], [Symbol|Rs], Rs).

left([], [], Rs, [b|Rs]).
left([L|Ls], Ls, Rs, [L|Rs]).

right_symbol_rest([], b, []).
right_symbol_rest([Symbol|Rest], Symbol, Rest).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Example 1: TM computing (x + 1) of a number x in unary encoding.

   Sample query:

   ?- turing(plus1, [1,1,1], Ts).
   %@ Ts = [1, 1, 1, 1] ;
   %@ false.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tm(plus1, q0, 1, s(q0), 1, right).
tm(plus1, q0, b, final, 1, stay).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Example 2: TM producing 501 "1"s and then halting.
   This machine is a Beavy Beaver candidate with 5 states (where
   the final state is not counted).

   Sample query:

   ?- turing(bb5, [], Ts), include(=(1), Ts, Ones), length(Ones, L).
   %@ Ts = [1, 1, b, 1, 1, 1, b, 1, 1|...],
   %@ Ones = [1, 1, 1, 1, 1, 1, 1, 1, 1|...],
   %@ L = 501 .
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tm(bb5, q0, b, s(1), 1, right).
tm(bb5, q0, 1, s(2), b, left).
tm(bb5, 1, b, s(2), 1, right).
tm(bb5, 1, 1, s(3), 1, right).
tm(bb5, 2, b, s(q0), 1, left).
tm(bb5, 2, 1, s(1), b, right).
tm(bb5, 3, b, s(4), b, right).
tm(bb5, 3, 1, final, 1, right).
tm(bb5, 4, b, s(2), 1, left).
tm(bb5, 4, 1, s(q0), 1, right).
