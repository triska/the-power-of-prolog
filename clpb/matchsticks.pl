:- use_module(library(clpz)).
:- use_module(library(clpb)).
:- use_module(library(between)).
:- use_module(library(pairs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Task: Remove minimum number of matchsticks so that no subsquare remains.

   Tested with Scryer Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  o = inside of the square
  variable = matchstick
  x = nothing here
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

layout(L) :-
        L = [[x,_,x,_,x,_,x,_,x],
             [_,o,_,o,_,o,_,o,_],
             [x,_,x,_,x,_,x,_,x],
             [_,o,_,o,_,o,_,o,_],
             [x,_,x,_,x,_,x,_,x],
             [_,o,_,o,_,o,_,o,_],
             [x,_,x,_,x,_,x,_,x],
             [_,o,_,o,_,o,_,o,_],
             [x,_,x,_,x,_,x,_,x]].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- matchsticks(L, Vs),
   same_length(Vs, Coeffs), maplist(=(1), Coeffs),
   weighted_maximum(Coeffs, Vs, Maximum),
   maplist(portray_clause, L).
%@ [x,1,x,0,x,1,x,1,x].
%@ [1,o,1,o,1,o,1,o,1].
%@ [x,0,x,1,x,0,x,0,x].
%@ [1,o,1,o,1,o,1,o,1].
%@ [x,1,x,0,x,0,x,1,x].
%@ [1,o,1,o,1,o,1,o,1].
%@ [x,0,x,1,x,1,x,0,x].
%@ [1,o,1,o,0,o,1,o,1].
%@ [x,1,x,1,x,1,x,1,x].
%@    L = [[x,1,x,0,x,1,x,1,x]|...], Vs = [1,1,0,1,0,1,1,1,0,1,1,0,1,1,1,1,0,1,0,1,...], Coeffs = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,...], Maximum = 31
%@ ;  ... .
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

matchsticks(L, Vs) :-
        findall(L-S, layout_subsquare(L,S), LSs),
        pairs_keys_values(LSs, Ls, Subs),
        maplist(=(L), Ls),
        maplist(not_subsquare, Subs),
        term_variables(Subs, Vs).

list_odds_evens([], [], []).
list_odds_evens([E], [], [E]).
list_odds_evens([E,O|Ls], [O|Os], [E|Es]) :- list_odds_evens(Ls, Os, Es).

not_subsquare(Subs) :- sat(~ *(Subs)).

layout_subsquare(L, Sub) :-
        layout(L),
        anyeven(SubList, Len),
        Len #> 0,
        anyeven(DropsTop, _),
        append(DropsTop, Rest0, L),
        anyeven(DropsLeft, _),
        maplist(drop_first(DropsLeft), Rest0, Rest),
        Rest = [First|_],
        phrase(horizontals(SubList, First), Tops),
        nth0(Len, Rest, Bot),
        phrase(horizontals(SubList, Bot), Bots),
        phrase(verticals(SubList, Rest), Lefts),
        maplist(drop_first(SubList), Rest, Rests1),
        phrase(verticals(SubList, Rests1), Rights),
        append([Tops,Lefts,Bots,Rights], Sub).

%?- findall(., layout_subsquare(_, _), Ls), length(Ls, L).

%?- layout_subsquare(L, Sub), maplist(writeln, L).

verticals([], _) --> [].
verticals([_,_|Vs], [_,[Var|_]|Rest]) --> [Var], verticals(Vs, Rest).

horizontals([], _) --> [].
horizontals([_,_|Hs], [_,Var|Rest]) --> [Var], horizontals(Hs, Rest).

drop_first(Ls0, Rests0, Rests) :-
        same_length(Ls0, Ls),
        append(Ls, Rests, Rests0).


anyeven(Ls, E) :-
        between(0, 4, E0),
        E #= E0 * 2,
        length(Ls, E).
