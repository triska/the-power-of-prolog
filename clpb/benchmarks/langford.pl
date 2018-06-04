/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Langford Sequence of order N: sequence of numbers 1,1,2,2,...,N,N
   such that the two occurrences of all k in 1..N are k units apart.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- include(bench).

langford(N, Sats) :-
        langford(N, _, Sats, _).

%?- satsn(5).

%?- length(_, N), langford(N, Vs, Sats, Conj), portray_clause(N), time(sat(Conj)), false.
%@ 0.
%@ % 100 inferences, 0.000 CPU in 0.000 seconds (91% CPU, 1538462 Lips)
%@ 1.
%@ % 35 inferences, 0.000 CPU in 0.000 seconds (77% CPU, 2500000 Lips)
%@ 2.
%@ % 3,890 inferences, 0.001 CPU in 0.001 seconds (88% CPU, 4814356 Lips)
%@ 3.
%@ % 126,913 inferences, 0.017 CPU in 0.018 seconds (98% CPU, 7263378 Lips)
%@ 4.
%@ % 1,115,458 inferences, 0.163 CPU in 0.164 seconds (99% CPU, 6862245 Lips)
%@ 5.
%@ % 3,298,303 inferences, 0.477 CPU in 0.481 seconds (99% CPU, 6918743 Lips)
%@ 6.
%@ % 7,755,915 inferences, 1.035 CPU in 1.046 seconds (99% CPU, 7495296 Lips)
%@ 7.
%@ % 31,545,100 inferences, 4.288 CPU in 4.326 seconds (99% CPU, 7357065 Lips)
%@ 8.
%@ % 112,305,689 inferences, 17.373 CPU in 17.519 seconds (99% CPU, 6464445 Lips)
%@ 9.

langford(N, Vs, Sats, Conj) :-
        Len #= 3*N,
        length(Row, Len),
        findall(Row, (between(1,N,K), phrase(row(N,K), Row)), Matrix0),
        sort(Matrix0, Matrix),
        transpose(Matrix, TMatrix),
        phrase(sats(TMatrix, Vs), Sats),
        list_conj(Sats, Conj).

sats([], _) --> [].
sats([Col|Cols], Vs0) -->
        { phrase(column_selection(Col, Vs0), Vs) },
        [card([1],Vs)],
        sats(Cols, Vs0).

column_selection([], []) --> [].
column_selection([C|Cs], [V|Vs]) -->
        (   { C =:= 1 } -> [V]
        ;   []
        ),
        column_selection(Cs, Vs).

row(N, K) -->
        n_zeros(_), [1], n_zeros(K), [1], n_zeros(_), % langford sequence
        { Prefix #= K - 1,                            % rest: represent K
          Suffix #= N - K },
        n_zeros(Prefix),
        [1],
        n_zeros(Suffix).

n_zeros(0)  --> [].
n_zeros(K0) --> [0], { K0 #> 0, K #= K0 - 1 }, n_zeros(K).

%?- length(Ls, 10), phrase(row(4, 3), Ls).
%@ Ls = [1, 0, 0, 0, 1, 0, 0, 0, 1, 0] ;
%@ Ls = [0, 1, 0, 0, 0, 1, 0, 0, 1, 0] ;
%@ false.
