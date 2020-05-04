/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   N Queens animation.

   https://www.metalevel.at/queens/
   ================================

   Written Feb. 2008 by Markus Triska (triska@metalevel.at)
   Public domain code. Tested with Scryer Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(freeze)).
:- use_module(library(charsio)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Constraint posting.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

n_queens(N, Qs) :-
        length(Qs, N),
        Qs ins 1..N,
        safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :- safe_queens(Qs, Q, 1), safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
        Q0 #\= Q,
        abs(Q0 - Q) #\= D0,
        D1 #= D0 + 1,
        safe_queens(Qs, Q0, D1).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Animation.

   For each N of the domain of queen Q, a reified constraint of the form

      Q #= N #<==> B

   is posted. When N vanishes from the domain, B becomes 0. A frozen
   goal then emits PostScript instructions for graying out the field.
   When B becomes 1, the frozen goal emits instructions for placing
   the queen. On backtracking, the field is cleared.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

animate(Qs) :- animate(Qs, Qs, 1).

animate([], _, _).
animate([_|Rest], Qs, N) :-
        animate_(Qs, 1, N),
        N1 #= N + 1,
        animate(Rest, Qs, N1).

animate_([], _, _).
animate_([Q|Qs], C, N) :-
        freeze(B, queen_value_truth(C,N,B)),
        Q #= N #<==> B,
        C1 #= C + 1,
        animate_(Qs, C1, N).

queen_value_truth(Q, N, 1) :- format("~w ~w q\n", [Q,N]).
queen_value_truth(Q, N, 0) :- format("~w ~w i\n", [Q,N]).
queen_value_truth(Q, N, _) :- format("~w ~w c\n", [Q,N]), false.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   PostScript definitions.

   Sample instructions, with these definitions loaded:

   2 init   % initialize a 2x2 board
   1 1 q    % place a queen on 1-1
   1 2 q
   1 2 c    % remove the queen from 1-2
   2 2 i    % gray out 2-2
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

postscript -->
    "/init { /N exch def 340 N div dup scale -1 -1 translate \
             /Palatino-Roman 0.8 selectfont 0 setlinewidth \
             1 1 N { 1 1 N { 1 index c } for pop } for } bind def \
     /r { translate 0 0 1 1 4 copy rectfill 0 setgray rectstroke } bind def \
     /i { gsave 0.5 setgray r grestore } bind def \
     /q { gsave r 0.5 0.28 translate (Q) dup stringwidth pop -2 div 0 moveto \
          1 setgray show grestore } bind def \
     /c { gsave 1 setgray r grestore } bind def\n".


show(N, Options) :-
        N #> 0,
        n_queens(N, Qs),
        phrase(postscript, Ps),
        format("~s ~w init\n", [Ps,N]),
        animate(Qs),
        labeling(Options, Qs),
        get_single_char(_),
        false.
show(_, _ ) :- halt.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Examples:

   ?- n_queens(8, Qs), labeling([ff], Qs).
      Qs = [1,5,8,6,3,7,2,4]
   ;  Qs = [1,6,8,3,7,4,2,5]
   ;  Qs = [1,7,4,6,8,2,5,3]
   ;  ...

   ?- n_queens(100, Qs), labeling([ff], Qs).
      Qs = [1,3,5,57,59,4,64,7,58,71|...]
   ;  Qs = [1,3,5,57,59,4,64,7,58,71|...]
   ;  ...


   Shell invocation:

   $ scryer-prolog -g 'show(20,[ff])' queens.pl | \
     gs -dNOPROMPT -g680x680 -dGraphicsAlphaBits=2 -r144 -q
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
