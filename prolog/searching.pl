:- use_module(library(clpfd)).
:- use_module(reif).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- tpartition(=(3), [1,2,3], Is, Os).

?- tfilter(dif(3), [1,2,3], Os).

%@ Is = [3],
%@ Os = [1, 2].

?- k_n(3, Adjs).
Adjs = [1-[2, 3], 2-[1, 3], 3-[1, 2]] .

?- k_n(3, Adjs),
   reachable(Adjs, [], 1, To).

?- k_n(3, Adjs),
   setof(To, reachable(Adjs, [], 1, To), Tos).
Adjs = [1-[2, 3], 2-[1, 3], 3-[1, 2]],
Tos = [1, 2, 3] .

?- length(_, N), portray_clause(N),
   k_n(N, Adjs),
   time(setof(To, reachable(Adjs, [], 1, To), Tos)),
   false.
%@ 0.
%@ % 17 inferences, 0.000 CPU in 0.000 seconds (80% CPU, 708333 Lips)
%@ 1.
%@ % 19 inferences, 0.000 CPU in 0.000 seconds (52% CPU, 1055556 Lips)
%@ 2.
%@ % 63 inferences, 0.000 CPU in 0.000 seconds (78% CPU, 1968750 Lips)
%@ 3.
%@ % 357 inferences, 0.000 CPU in 0.000 seconds (93% CPU, 5100000 Lips)
%@ 4.
%@ % 2,203 inferences, 0.000 CPU in 0.000 seconds (98% CPU, 7467797 Lips)
%@ 5.
%@ % 14,775 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 8773753 Lips)
%@ 6.
%@ % 110,079 inferences, 0.012 CPU in 0.012 seconds (98% CPU, 9334266 Lips)
%@ 7.
%@ % 914,953 inferences, 0.093 CPU in 0.098 seconds (95% CPU, 9816460 Lips)
%@ 8.
%@ % 8,446,107 inferences, 0.823 CPU in 0.837 seconds (98% CPU, 10265329 Lips)
%@ 9.
%@ % 85,982,043 inferences, 8.086 CPU in 8.135 seconds (99% CPU, 10633230 Lips)

?- length(_, N), portray_clause(N),
   k_n(N, Adjs),
   time(warshall(Adjs, [1], Tos)).

?- k_n(3, Adjs),
   warshall(Adjs, [1], Ns).

?- k_n(9, Adjs),
   time(warshall(Adjs, [1], Tos)).
%@ % 322 inferences, 0.000 CPU in 0.000 seconds (93% CPU, 4735294 Lips)
Tos = [1, 2, 3, 4, 5, 6, 7, 8, 9] .

?- k_n(6, Adjs),
   pairs_keys(Adjs, Ks),
   maplist(dot_adj, Adjs).
%@ 1 -- 2;
%@ 1 -- 3;
%@ 1 -- 4;
%@ 1 -- 5;
%@ 1 -- 6;
%@ 2 -- 3;
%@ 2 -- 4;
%@ 2 -- 5;
%@ 2 -- 6;
%@ 3 -- 4;
%@ 3 -- 5;
%@ 3 -- 6;
%@ 4 -- 5;
%@ 4 -- 6;
%@ 5 -- 6;
%@ Adjs = [1-[2, 3, 4, 5, 6], 2-[1, 3, 4, 5, 6], 3-[1, 2, 4, 5, 6], 4-[1, 2, 3, 5|...], 5-[1, 2, 3|...], 6-[1, 2|...]],
%@ Ks = [1, 2, 3, 4, 5, 6] .



?- k_n(3, Adjs),
   phrase(reachables([1]

?- list_length(_, L).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
dot_adj(Node-Adjs) :-
        maplist(edge(Node), Adjs).

edge(From, To) :-
        (   From @< To ->
            format("~w -- ~w;~n", [From,To])
        ;   true
        ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Main predicate
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

warshall(Adjs, Nodes0, Nodes) :-
        phrase(reachables(Nodes0, Adjs), Nodes1, Nodes0),
        sort(Nodes1, Nodes2),
        if_(Nodes2 = Nodes0,
            Nodes = Nodes2,
            warshall(Adjs, Nodes2, Nodes)).

reachables([], _) --> [].
reachables([Node|Nodes], Adjs) -->
        { member(Node-Rs, Adjs) },
        Rs,
        reachables(Nodes, Adjs).

reachable(_, _, From, From).
reachable(Adjs, Visited, From, To) :-
        maplist(dif(Next), Visited),
        member(From-As, Adjs),
        member(Next, As),
        reachable(Adjs, [From|Visited], Next, To).

list_length([], 0).
list_length([_|Ls], Length) :-
        Length #= Length0 + 1,
        Length #> 0,
        list_length(Ls, Length0).


k_n(N, Adjs) :-
        list_length(Nodes, N),
        Nodes ins 1..N,
        all_distinct(Nodes),
        once(label(Nodes)),
        maplist(adjs(Nodes), Nodes, Adjs).


adjs(Nodes, Node, Node-As) :-
        tfilter(dif(Node), Nodes, As).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- k_n(3, As).
%@ As = [1-[2, 3], 2-[1, 3], 3-[1, 2]] ;
%@ false.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */