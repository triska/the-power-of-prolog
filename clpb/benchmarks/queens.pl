:- include(bench).

queens(N, Sats) :-
        n_queens(N, _, Sats).

n_queens(N, Qs, Sats) :-
        length(Qs, N),
        maplist(length_(N), Qs),
        transpose(Qs, TQs),
        phrase((rows(Qs),rows(TQs),
                diagonals(Qs, 1, 1, N)), Sats).


list_disj([], 0).
list_disj([L|Ls], L+Goal) :- list_disj(Ls, Goal).

rows([]) --> [].
rows([Row|Rows]) -->
        { list_disj(Row, Sat) },
        [Sat],
        not_same_row(Row),
        rows(Rows).

not_same_row([]) --> [].
not_same_row([Q|Qs]) -->
        not_same_row_(Qs, Q),
        not_same_row(Qs).

not_same_row_([], _) --> [].
not_same_row_([L|Ls], Q) -->
        [~Q + ~L],
        not_same_row_(Ls, Q).

length_(L, Ls) :- length(Ls, L).

diagonals(Qs, Row, Col, N) -->
        (   { Row #> N } -> []
        ;   { Col #> N } ->
            { Row1 #= Row + 1 },
            diagonals(Qs, Row1, 1, N)
        ;   { queen_at(Qs, Row, Col, Q),
              DRow #= Row + 1,
              DCol #= Col + 1 },
            diagonal_down(Qs, DRow, DCol, N, Q),
            { URow #= Row - 1,
              UCol #= Col + 1 },
            diagonal_up(Qs, URow, UCol, N, Q),
            { Col1 #= Col + 1 },
            diagonals(Qs, Row, Col1, N)
        ).

diagonal_down(Qs, Row, Col, N,Q) -->
        (   { Row #> N } -> []
        ;   { Col #> N } -> []
        ;   { queen_at(Qs, Row, Col, Q0) },
            [~Q + ~Q0],
            { Row1 #= Row + 1,
              Col1 #= Col + 1 },
            diagonal_down(Qs, Row1, Col1, N, Q)
        ).

diagonal_up(Qs, Row, Col, N, Q) -->
        (   { Row #< 1 } -> []
        ;   { Col #> N } -> []
        ;   { queen_at(Qs, Row, Col, Q0) },
            [~Q + ~Q0],
            { Row1 #= Row - 1,
              Col1 #= Col + 1 },
            diagonal_up(Qs, Row1, Col1, N, Q)
        ).



queen_at(Qs, NRow, NCol, Q) :-
        nth1(NRow, Qs, Row),
        nth1(NCol, Row, Q).
