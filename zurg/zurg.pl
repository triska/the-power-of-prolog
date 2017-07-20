%   Solution for "Escape from Zurg"
%   Written by Markus Triska (triska@metalevel.at), Sept. 4th 2007

toy_time(buzz,   5).
toy_time(woody, 10).
toy_time(rex,   20).
toy_time(hamm,  25).

%   The current state is represented as a term of the form state(T,Ls,Rs),
%   where: T is an integer storing the time taken so far, Ls is a list of
%   toys standing on the left, Rs is a list of toys standing on the right.

moves(Ms) :- phrase(moves(state(0,[buzz,woody,rex,hamm],[])), Ms).

moves(state(T0,Ls0,Rs0)) -->
        { select(Toy1, Ls0, Ls1), select(Toy2, Ls1, Ls2),
          Toy1 @< Toy2,
          toy_time(Toy1, Time1), toy_time(Toy2, Time2),
          T1 #= T0 + max(Time1,Time2), T1 #=< 60 },
        [left_to_right(Toy1,Toy2)],
        moves_(state(T1,Ls2,[Toy1,Toy2|Rs0])).

moves_(state(_,[],_))     --> [].
moves_(state(T0,Ls0,Rs0)) -->
        { select(Toy, Rs0, Rs1),
          toy_time(Toy, Time),
          T1 #= T0 + Time, T1 #=< 60 },
        [right_to_left(Toy)],
        moves(state(T1,[Toy|Ls0],Rs1)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Example, using GNU Prolog 1.3.0:

    | ?- moves(Ms).

    Ms = [left_to_right(buzz,woody),right_to_left(buzz),left_to_right(hamm,rex),right_to_left(woody),left_to_right(buzz,woody)] ? ;

    Ms = [left_to_right(buzz,woody),right_to_left(woody),left_to_right(hamm,rex),right_to_left(buzz),left_to_right(buzz,woody)] ? ;

    no
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
