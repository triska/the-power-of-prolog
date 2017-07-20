:- use_module(library(clpfd)).

regions(Rs) :-
        Rs = [A,B,C,D,E,F],
        Rs ins 0..3,
        A #\= B, A #\= C, A #\= D, A #\= F,
        B #\= C, B #\= D,
        C #\= D, C #\= E,
        D #\= E, D #\= F,
        E #\= F.

integer_color(0, red).
integer_color(1, green).
integer_color(2, blue).
integer_color(3, yellow).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

?- regions(Rs), Rs ins 0..2, label(Rs).
false.

?- regions(Rs), findall(., label(Rs), Ls), length(Ls, L).

?- regions(Rs), label(Rs).
Rs = [0, 1, 2, 1, 0, 3] ;
Rs = [0, 1, 2, 1, 3, 3] ;
Rs = [0, 1, 2, 3, 0, 1] .

?- regions(Rs), label(Rs),
   maplist(integer_color, Rs, Cs),
   pairs_keys_values(Pairs, [a,b,c,d,e,f], Cs).
Rs = [0, 1, 2, 3, 0, 1],
Cs = [red, green, blue, yellow, red, green],
Pairs = [a-red, b-green, c-blue, d-yellow, e-red, f-green] ;
Rs = [0, 1, 2, 3, 0, 2],
Cs = [red, green, blue, yellow, red, blue],
Pairs = [a-red, b-green, c-blue, d-yellow, e-red, f-blue] .
   

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   

regions(Rs):-
        Rs = [R1,R2,R3,R4,R5,R6],

        % neighbouring regions have different 
        dif(R1, R2),
        dif(R1, R3),
        dif(R1, R4),
        dif(R1,R6),
        dif(R2, R3),
        dif(R2, R5),
        dif(R3, R4),
        dif(R3,R5),
        dif(R3, R6),
        dif(R4, R5),
        dif(R4, R6),
        maplist(color, Rs).

color(red).
color(green).
color(blue).
color(yellow).

   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% neighbouring regions have different 


so_regions(Rs):-
        Rs = [A,B,C,D,E,F],
        Rs ins 0..3,
        A #\= B, A #\= C, A #\= D,
        A #\= F, B #\= C, B #\= E,
        C #\= D, C #\= E, C #\= F,
        D #\= E, D #\= F.

print_colors(Rs) :-
        pairs_keys_values(Pairs, Rs, [a,b,c,d,e,f]),
        maplist(print_color, Pairs).

print_color(Var-Name) :-
        fd_dom(Var, Dom),
        dom_integers(Dom, Is),
        format("~w", [Name]),
        maplist(name_color(Is), [0-red,1-green,2-blue,3-yellow]),
        nl.

name_color(Is, I-Name) :-
        (   memberchk(I, Is) ->
            format(" ~w ", [Name])
        ;   true
        ).
            

dom_integers(D, Is) :- phrase(dom_integers_(D), Is).

dom_integers_(I)      --> { integer(I) }, [I].
dom_integers_(L..U)   --> { numlist(L, U, Is) }, Is.
dom_integers_(D1\/D2) --> dom_integers_(D1), dom_integers_(D2).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

?- regions(Rs).
Rs = [_640, _646, _652, _658, _664, _670],
_640 in 0..3,
_640#\=_670,
_640#\=_658,
_640#\=_652,
_640#\=_646,
%@ _670 in 0..3,
%@ _658#\=_670,
%@ _652#\=_670,
%@ _658 in 0..3,
%@ _658#\=_664,
%@ _652#\=_658,
%@ _664 in 0..3,
%@ _652#\=_664,
%@ _646#\=_664,
%@ _652 in 0..3,
%@ _646#\=_652,
%@ _646 in 0..3.
Rs = [0, 1, 2, 1, 0, 3] .
?- regions(Rs), print_colors(Rs), false.
%@ false.

?- regions([A,B,C,D,E,F]), F = 0, C = 1.
C = 1,
F = 0,
A in 2..3,
A#\=D,
A#\=B,
D in 2..3,
D#\=E,
E in 0\/2..3,
B#\=E,
B in 0\/2..3.
F = 0,
A in 1..3,
A#\=D,
A#\=C,
A#\=B,
D in 1..3,
D#\=E,
C#\=D,
E in 0..3,
C#\=E,
B#\=E,
C in 1..3,
B#\=C,
B in 0..3.


?- regions(Rs), Rs = [A,B,C,D,E,F], F = 0, print_colors(Rs), false.
%@ false.

?- regions(Rs), Rs = [A,B,C,D,E,F], F = 0, C = 1, print_colors(Rs), false.
%@ a blue  yellow 
%@ b red  blue  yellow 
%@ c green 
%@ d blue  yellow 
%@ e red  blue  yellow 
%@ f red 
%@ false.

?- regions(Rs), Rs = [A,B,C,D,E,F], F = 0, C = 1, B = 3, print_colors(Rs), false.
%@ a blue 
%@ b yellow 
%@ c green 
%@ d yellow 
%@ e red  blue 
%@ f red 
%@ false.


%@ a green  blue  yellow 
%@ b green  blue  yellow 
%@ c green  blue  yellow 
%@ d green  blue  yellow 
%@ e red 
%@ f red 
%@ false.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */