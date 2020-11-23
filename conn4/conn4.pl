/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   Connect 4 in Prolog
   ===================

   Written 2006, 2020 by Markus Triska (triska@metalevel.at)
   Public domain code. Tested with Scryer Prolog.

   To make the computer play against itself, use:

       ?- play.

   If you have GhostScript ("gs") installed, you can use:

       $ scryer-prolog -g show conn4.pl | \
            gs -dNOPROMPT -g600x600 -r120 -q

   Press 1, 2, ..., 7 to drop a piece in that column. Alternatively,
   press SPACE to let the computer choose a column for you.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Since we reason over integers, we use CLP(ℤ) constraints.

   For more information, see:

   https://www.metalevel.at/prolog/clpz
   ====================================
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(reif)).
:- use_module(library(charsio), [get_single_char/1]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The game board is represented as a list of columns.

   Each column is a term col(Num,Free,TP,TN,Ps), where:
      Num: column number
      Free: yes/no, whether a piece can be placed "on top" (= at the end)
      TP: Colour of topmost piece
      TN: max. number of consecutive topmost pieces of same colour
      Ps: Pieces in this column

   Each piece is one of:

      - : empty cell
      x : piece of player x
      o : piece of player o
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

empty(-).

empty_board(N, M, Board) :-
        length(Board, M),
        length(Es, N),
        maplist(empty, Es),
        foldl(column(Es), Board, 1, _).

column(Empty, col(N0,yes,empty,0,Empty), N0, N) :-
        N #= N0 + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

win(Player, Board) :-
        (   member(col(_,_,Player,N,_), Board), N #>= 4
        ;   un_col(Board, Board1),
            (   four_in_a_row(Board1, Player)
            ;   diagonal(Board1, Player)
            )
        ).


un_col([], []).
un_col([col(_,_,_,_,Cs)|Rest], [Cs|Css]) :-  un_col(Rest, Css).

four_in_a_row([Col1,Col2,Col3,Col4|Cs], Player) :-
        (   four_in_a_row(Col1, Col2, Col3, Col4, Player)
        ;   four_in_a_row([Col2,Col3,Col4|Cs], Player)
        ).

four_in_a_row([C1|Cs1], [C2|Cs2], [C3|Cs3], [C4|Cs4], P) :-
        empty(E),
        Firsts = [C1,C2,C3,C4],
        maplist(dif(E), Firsts),
        (   maplist(=(P), Firsts)
        ;   four_in_a_row(Cs1, Cs2, Cs3, Cs4, P)
        ).


diagonal(Board, Player) :-
        Board = [_,_,_,_|_],
        (   diagonal_down(Board, Player)
        ;   diagonal_up(Board, Player)
        ;   Board = [_|Rest],
            diagonal(Rest, Player)
        ).

diagonal_down([Col1,Col2,Col3,Col4|_], Player) :-
        Col2 = [_|Rot2],
        Col3 = [_,_|Rot3],
        Col4 = [_,_,_|Rot4],
        four_in_a_row(Col1, Rot2, Rot3, Rot4, Player).

diagonal_up([Col1,Col2,Col3,Col4|_], Player) :-
        Col1 = [_,_,_|Rot1],
        Col2 = [_,_|Rot2],
        Col3 = [_|Rot3],
        four_in_a_row(Rot1, Rot2, Rot3, Col4, Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_piece_([P|Ps], Player, Is, Free) :-
        (   empty(P) ->
            Is = [Player|Ps],
            (   Ps = [] ->  Free = no
            ;   Free = yes
            )
        ;   Is = [P|Rest],
            insert_piece_(Ps, Player, Rest, Free)
        ).

play_column([Col0|Cols0], Column, Player, [Col|Cols]) :-
        Col0 = col(CN0,_,TP0,TN0,Cs0),
        (   CN0 #= Column ->
            insert_piece_(Cs0, Player, Cs, Free),
            Cols = Cols0,
            (   TP0 == Player ->
                TN #= TN0 + 1
            ;   TN = 1
            ),
            Col = col(CN0,Free,Player,TN,Cs)
        ;   Col = Col0,
            play_column(Cols0, Column, Player, Cols)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Minimax algorithm with alpha-beta pruning
   =========================================

   x is the maximizing player. We rate each column that x can (still)
   choose by rating the best move that o can make in response, and
   then take one of the columns where this value attains its maximum.
   In other words, the best move of x makes it hardest for o.

   The "best move o could do" is computed analogously, by making it
   hard for x.

   Each possible move is simply represented by its column number.
   The score of each move is:

   - positive if x wins by this move
   - zero if no decision is reached yet
   - negative if o wins.

   0 can thus be assigned only if the depth limit is exceeded. In such
   cases, it would be a valuable extension to assign a heuristic score
   from the interval (-1,1). Note that higher and lower numbers can be
   assigned to force particular decisions.

   Ties are broken by picking a random move among the best ones,
   therefore the game typically differs from run to run.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

opponent(x, o).
opponent(o, x).

max_player(x).

max_depth(3).

free_column_t(col(_,A,_,_,_), T) :- =(A, yes, T).

column_number(col(Num,_,_,_,_), Num).

possible_columns(Cols0, Cs) :-
        tfilter(free_column_t, Cols0, Cols),
        maplist(column_number, Cols, Cs).

play(Player, Board0, Move) :-
        possible_columns(Board0, Columns),
        max_depth(Depth),
        Alpha #= -Depth - 1,
        Beta #= Depth + 1,
        moves_with_scores(Columns, Depth, Alpha, Beta, Player, Board0, SMs),
        best_move(Player, SMs, Move).

key_eq_t(A, B-_, T) :- =(A, B, T).

best_move(Player, SMs, Move) :-
        best_score(SMs, Player, Score),
        tfilter(key_eq_t(Score), SMs, BestMoves),
        length(BestMoves, LC),
        random_integer(0, LC, Index),
        nth0(Index, BestMoves, _-Move).

moves_with_scores([], _, _, _, _, _, []).
moves_with_scores([M|Ms], Depth, Alpha0, Beta0, Player, Board0, [Score-M|SMs]) :-
        move_score(Depth, Alpha0, Beta0, Player, Board0, M, Score),
        (   max_player(Player) ->
            Alpha #= max(Alpha0, Score),
            Beta #= Beta0
        ;   Alpha #= Alpha0,
            Beta #= min(Beta0, Score)
        ),
        (   Beta #< Alpha -> SMs = []
        ;   moves_with_scores(Ms, Depth, Alpha, Beta, Player, Board0, SMs)
        ).

move_score(Depth, Alpha, Beta, Player, Board0, Move, Score) :-
        (   Depth #= 0 -> Score #= 0
        ;   play_column(Board0, Move, Player, Board1),
            (   win(Player, Board1) ->
                (   max_player(Player) ->
                    Score #= 1 + Depth % favour early wins
                ;   Score #= -1 - Depth
                )
            ;   possible_columns(Board1, Moves),
                (   Moves == [] ->
                    Score #= 0
                ;   D1 #= Depth - 1,
                    opponent(Player, Opp),
                    moves_with_scores(Moves, D1, Alpha, Beta, Opp, Board1, SMs),
                    best_score(SMs, Opp, Score)
                )
            )
        ).


best_score(SMs0, Player, Score) :-
        keysort(SMs0, SMs),
        (   max_player(Player) ->
            last(SMs, Score-_)
        ;   SMs = [Score-_|_]
        ).

last(Ls, L) :- reverse(Ls, [L|_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_board(Board0) :-
        un_col(Board0, Board1),
        maplist(reverse, Board1, Board2),
        transpose(Board2, Ts),
        maplist(print_line, Ts),
        nl.

print_line(Line) :-
        format("\n\t", []),
        maplist(print_col, Line).

print_col(Col) :- format("~w ", [Col]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play :-
        empty_board(6, 7, Board0),
        alternate(x, Board0).

alternate(Player, Board0) :-
        (   play(Player, Board0, Column) ->
            play_column(Board0, Column, Player, Board1),
            format("\n\n~w plays:\n", [Player]),
            display_board(Board1),
            (   win(Player, Board1) ->
                format("~w wins\n", [Player])
            ;   opponent(Player, Opp),
                alternate(Opp, Board1)
            )
        ;   format("draw\n", [])
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- play.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   PostScript definitions.

   Sample instructions, with these definitions loaded:

   /x 4 drop
   /o 4 drop
   /x 3 drop
   /x wins
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

postscript -->
        "360 7 div dup scale 0.05 setlinewidth \
        /Helvetica-Bold 0.7 selectfont \
        0 0 moveto 7 0 lineto stroke \
        0 1 7 { dup 0 moveto 6 lineto stroke } for \
        gsave 0.5 6.2 translate \
            1 1 7 { 1 string cvs dup stringwidth pop -2 div 0 moveto show \
            1 0 translate } for grestore \
         \
        /heights [7 { 0 } repeat] def \
        /player { /x eq { 1 0 0 } { 0 0 1 } ifelse setrgbcolor \
            newpath 0.5 0.5 0.4 0 360 arc fill } bind def \
        /drop { gsave 1 sub /c exch def c heights c get translate \
             player grestore heights c heights c get 1 add put } bind def \
         \
        /nonumbers { 1 setgray 0 6 7 1 rectfill } bind def \
        /wins { nonumbers gsave 0.5 6 translate player \
               1 0.2 moveto (wins) show grestore  } bind def \
        /draw { nonumbers 0.5 6.2 moveto 0 setgray (draw) show } bind def".

show :-
        empty_board(6, 7, Board0),
        phrase(postscript, Ps),
        format("~s\n", [Ps]),
        interact(x, Board0).

user_input(Board, Char) :-
        get_single_char(Char0),
        (   Char0 == ' ' -> Char = c
        ;   member(Char0, "1234567") ->
            number_chars(Col, [Char0]),
            (   play_column(Board, Col, x, _) -> Char = Col
            ;   user_input(Board, Char)
            )
        ;   user_input(Board, Char)
        ).

interact(Player, Board0) :-
        (   \+ play(Player, Board0, _) -> format("draw\n", [])
        ;   user_input(Board0, Char),
            (   Char == c ->
                play(Player, Board0, Column)
            ;   Column = Char
            ),
            play_column(Board0, Column, Player, Board1),
            format("/~w ~w drop\n", [Player,Column]),
            (   win(Player, Board1) ->
                format("/~w wins\n", [Player])
            ;   opponent(Player, Opp),
                interact(Opp, Board1)
            )
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ?- show.

   For interactive use:

   $ scryer-prolog -g show conn4.pl | gs -dNOPROMPT -g600x600 -r120 -q
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

