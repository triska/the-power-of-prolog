:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(library(format)).
:- use_module(library(time)).
:- use_module(library(pairs)).

lists(["abcd",
       "abc",
       "abcde",
       "a",
       "ab"]).

list_pair(Ls, L-Ls) :-
        list_length(Ls, L).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- tpartition(=(3), [1,2,3], Is, Os).

?- tfilter(dif(3), [1,2,3], Os).

%@ Is = [3],
%@ Os = [1, 2].

?- k_n(3, Adjs).
   Adjs = [1-[2,3],2-[1,3],3-[1,2]]
;  false.


?- k_n(3, Adjs),
   reachable(Adjs, [], 1, To).

?- k_n(3, Adjs),
   setof(To, reachable(Adjs, [], 1, To), Tos).
   Adjs = [1-[2,3],2-[1,3],3-[1,2]], Tos = [1,2,3]
;  false.

?- length(_, N), portray_clause(N),
   k_n(N, Adjs),
   time(setof(To, reachable(Adjs, [], 1, To), Tos)),
   false.
%@ 0.
%@    % CPU time: 0.000 seconds
%@    % CPU time: 0.000 seconds
%@ 1.
%@    % CPU time: 0.000 seconds
%@    % CPU time: 0.000 seconds
%@ 2.
%@    % CPU time: 0.000 seconds
%@    % CPU time: 0.000 seconds
%@ 3.
%@    % CPU time: 0.000 seconds
%@    % CPU time: 0.001 seconds
%@ 4.
%@    % CPU time: 0.002 seconds
%@    % CPU time: 0.003 seconds
%@ 5.
%@    % CPU time: 0.016 seconds
%@    % CPU time: 0.017 seconds
%@ 6.
%@    % CPU time: 0.108 seconds
%@    % CPU time: 0.109 seconds
%@ 7.
%@    % CPU time: 0.831 seconds
%@    % CPU time: 0.832 seconds
%@ 8.
%@    % CPU time: 7.348 seconds
%@    % CPU time: 7.353 seconds



?- length(_, N), portray_clause(N),
   k_n(N, Adjs),
   time(warshall(Adjs, [1], Tos)).

?- k_n(3, Adjs),
   warshall(Adjs, [1], Ns).

?- k_n(9, Adjs),
   time(warshall(Adjs, [1], Tos)).
%@    % CPU time: 0.000 seconds
%@    Adjs = [1-[2,3,4,5,6,7,8,9],2-[1,3,4,5,6,7,8,...],3-[1,2,4,5,6,7,...],4-[1,2,3,5,6,...],5-[1,2,3,4,...],6-[1,2,3,...],7-[1,2,...],8-[1,...],9-[...]], Tos = [1,2,3,4,5,6,7,8,9]
%@ ;  % CPU time: 0.003 seconds
%@    false.

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
%@    Adjs = [1-[2,3,4,5,6],2-[1,3,4,5,6],3-[1,2,4,5,6],4-[1,2,3,5,6],5-[1,2,3,4,...],6-[1,2,3,...]], Ks = [1,2,3,4,5,6]
%@ ;  false.


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
%@    As = [1-[2,3],2-[1,3],3-[1,2]]
%@ ;  false.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
