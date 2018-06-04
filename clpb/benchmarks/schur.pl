/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Distribute the numbers 1,...,N into three boxes such that (a) no
   box contains the numbers i and 2*i, and (b) no box contains the
   numbers i,j, and i+j.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- include(bench).

schur(N, Sats) :-
        length(Ls, N),
        maplist(same_length([_,_,_]), Ls),
        findall(triple(I,J,Sum), (between(1,N,I),
                                     between(I,N,J),
                                     %I < J,
                                     Sum is I+J, Sum =< N), Triples),
        phrase((cards1(Ls),
                triples(Triples, Ls)), Sats).

triples([], _) --> [].
triples([T|Ts], Ls) --> triple(T, Ls), triples(Ts, Ls).

triple(triple(I,J,Sum), Ls) -->
        { nth1(I, Ls, As),
          nth1(J, Ls, Bs),
          nth1(Sum, Ls, Cs),
          transpose([As,Bs,Cs], Ts) },
        cards012(Ts).

cards1([]) --> [].
cards1([L|Ls]) --> [card([1],L)], cards1(Ls).

cards012([]) --> [].
cards012([L|Ls]) --> [card([0,1,2],L)], cards012(Ls).

%?- satn(3).
