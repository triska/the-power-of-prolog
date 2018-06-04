/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   pigeon N: place N+1 pigeons into N holes, such that each hole
   holds at most one pigeon.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
:- include(bench).

pigeon(N, Sats) :-
        N1 #= N + 1,
        pigeon(N, N1, _, Sats).

pigeon(I, J, Rows, Ls) :-
        length(Rows, J),
        maplist(length_list(I), Rows),
        transpose(Rows, TRows),
        phrase((all_card1(Rows),all_max1(TRows)), Ls).

length_list(N, Ls) :- length(Ls, N).

all_card1([]) --> [].
all_card1([Ls|Lss]) --> [card([1],Ls)], all_card1(Lss).

all_max1([]) --> [].
all_max1([Ls|Lss]) --> [card([0,1],Ls)], all_max1(Lss).
