:- use_module(library(clpz)).
:- use_module(library(reif)).


hailstone(N, N).
hailstone(N0, N) :-
        if_(N0 mod 2 #= 0,
            N1 #= N0//2,
            N1 #= 3*N0 + 1),
        hailstone(N1, N).


#=(A0, B0, T) :-
        A #= A0,
        B #= B0,
        zcompare(C, A, B),
        c_eq(C, T).

c_eq(=, true).
c_eq(>, false).
c_eq(<, false).

% hailstone(N, N).
% hailstone(N0, N) :-
%         N0 mod 2 #= 0,
%         N1 #= N0 // 2,
%         hailstone(N1, N).
% hailstone(N0, N) :-
%         N0 mod 2 #= 1,
%         N1 #= 3*N0 + 1,
%         hailstone(N1, N).

%?- listing(hailstone/2).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- hailstone(3, N).
   N = 3
;  N = 10
;  N = 5
;  N = 16
;  N = 8
;  N = 4
;  N = 2
;  N = 1
;  ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
