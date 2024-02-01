:- use_module(library(clpz)).

list_length([], 0).
list_length([_|Ls], N) :-
        N #> 0,
        N #= N0 + 1,
        list_length(Ls, N0).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- list_length("abc", L).
   L = 3.

?- list_length(Ls, 3).
   Ls = [_A,_B,_C]
;  false.

?- list_length(Ls, L).
   Ls = [], L = 0
;  Ls = [_A], L = 1
;  Ls = [_A,_B], L = 2
;  Ls = [_A,_B,_C], L = 3
;  Ls = [_A,_B,_C,_D], L = 4
;  ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
