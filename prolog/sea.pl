:- use_module(library(clpb)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).

sea([N,M,L,R,I]) :-
        sat(M =< N),   % statement 1
        sat(L =< R),   % statement 2
        sat(I =< ~R),  % statement 3
        sat(N =< L).   % statement 4

implication_chain([], Prev) --> [Prev].
implication_chain(Vs0, Prev) --> [Prev],
        { select(V, Vs0, Vs) },
        (   { taut(Prev =< V, 1) } -> implication_chain(Vs, V)
        ;   { taut(Prev =< ~V, 1) } -> implication_chain(Vs, ~V)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

?- sea(Vs),
   Vs = [N,M,L,R,I],
   member(Start, Vs),
   phrase(implication_chain(Vs, Start), Cs).


?- sea(Vs),
   Vs = [N,M,L,R,I],
   select(Start, Vs, Rest),
   phrase(implication_chain(Rest, Start), Cs).


?- sea(Vs),
   Vs = [N,M,L,R,I],
   select(Start, Vs, Rest),
   phrase(implication_chain(Rest, ~Start), Cs).



?- sea(Vs),
   Vs0 = [N,M,L,R,I],
   select(Start0, Vs0, Vs),
   ( Start = Start0 ; Start = ~Start0 ),
   phrase(implication_chain(Vs, Start), Cs).


?- sea(Vs),
   Vs0 = [N,M,L,R,I],
   select(Start0, Vs0, Vs),
   ( Start = Start0 ; Start = ~Start0 ),
   phrase(implication_chain(Vs, Start0), Cs).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */