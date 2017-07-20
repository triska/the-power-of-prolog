:- use_module(library(clpfd)).

list_length([], 0).
list_length([_|Ls], N) :-
        N #> 0,
        N #= N0 + 1,
        list_length(Ls, N0).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- list_length([a,b,c], L).
L = 3.

?- list_length(Ls, 3).
Ls = [_G1007, _G1087, _G1167] ;
false.

?- list_length(Ls, L).
Ls = [],
L = 0 ;
Ls = [_G21],
L = 1 ;
Ls = [_G21, _G70],
L = 2 .
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */