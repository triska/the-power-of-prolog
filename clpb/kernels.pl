:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(between)).
:- use_module(library(pairs)).
:- use_module(library(time)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Independent sets, maximal independent sets (kernels) and maximal
   independent sets with maximum weight of the Cycle graph C_100.

   Example queries, tested with Scryer Prolog:

   (1) Independent sets:

       ?- time((independent_set(I,_),sat_count(I,N))).
       %@    % CPU time: 0.999s
       %@    I = *(....), N = 792070839848372253127
       %@ ;  % CPU time: 0.000s
       %@    false.

   (2) Maximal independent sets:

       ?- time((kernel(K,_),sat_count(K,N))).
       %@    % CPU time: 3.893s
       %@    K = *(...)* *(...), N = 1630580875002
       %@ ;  % CPU time: 0.000s
       %@    false.

   (3) Maximal independent sets with maximum weight:

       %?- time(maximum_thue_morse_kernel(Is, Negatives, Max)).
       %@    % CPU time: 122.740s
       %@    Is = [1,3,6,9,12,15,18,20,23,25,27,30,33,36,39,41,43,46,48,51,...], Negatives = [1,25,41,73,97], Max = 28
       %@ ;  ... .

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Cycle graph C_100.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

cycle(100).

edge(X, Y)     :- edge_(1, 2, X, Y).
edge(1, Limit) :- cycle(Limit).

edge_(A, B, A, B).
edge_(_, N1, X, Y) :-
        N2 #= N1 + 1,
        cycle(Limit),
        N2 #=< Limit,
        edge_(N1, N2, X, Y).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Node N has weight w_N = (-1)^n(N), where n(N) is the number of 1s in
  the binary representation of N. This is similar to Thue-Morse codes.

  For example, the Thue-Morse weights of the integers 1,...,10 are:

   ?- thue_morse_weights(10, Ms).
   %@    Ms = [-1,-1,1,-1,1,1,-1,-1,1,1].

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

integer_binaries(I, Bs) :-
        once(phrase(binaries(I, 0), Bs0)),
        reverse(Bs0, Bs).

binaries(0, _) --> [].
binaries(I, E0) -->
        (   { I mod 2 #= 0 } -> [0]
        ;   [1]
        ),
        { I1 #= I // 2, E1 #= E0 + 1 },
        binaries(I1, E1).

thue_morse_weights(N, Ms) :-
        length(Ms, N),
        numlist(1, N, Ns),
        maplist(integer_binaries, Ns, Bss),
        maplist(sum_list, Bss, Cards),
        maplist(neg1_pow, Cards, Ms).

neg1_pow(Card, Pow) :- Pow #= (-1)^Card.

maximum_thue_morse_kernel(Is, Negatives, Max) :-
        kernel(K, Assoc),
        assoc_to_values(Assoc, Vs),
        sat(K),
        length(Vs, L),
        thue_morse_weights(L, Weights),
        weighted_maximum(Weights, Vs, Max),
        numlist(1, L, Ns),
        pairs_keys_values(Pairs0, Vs, Ns),
        include(key_one, Pairs0, Pairs),
        pairs_values(Pairs, Is),
        pairs_keys_values(WNs, Weights, Ns),
        pairs_keys_values(WPairs0, Vs, WNs),
        include(key_one, WPairs0, WPairs1),
        pairs_values(WPairs1, WPairs2),
        include(key_negative, WPairs2, WPairs),
        pairs_values(WPairs, Negatives).

key_negative(K-_) :- K #< 0.

key_one(1-_).

include(_, [], []).
include(Goal_1, [E|Es], Ls) :-
        (   call(Goal_1, E) ->
            Ls = [E|Rs]
        ;   Ls = Rs
        ),
        include(Goal_1, Es, Rs).

%?- time(maximum_thue_morse_kernel(Is, Negatives, Max)).
%@    % CPU time: 124.214s
%@    Is = [1,3,6,9,12,15,18,20,23,25,27,30,33,36,39,41,43,46,48,51,...], Negatives = [1,25,41,73,97], Max = 28
%@ ;  % CPU time: 0.039s
%@    Is = [1,3,6,9,12,15,18,20,23,25,27,30,33,36,39,41,43,46,48,51,...], Negatives = [1,25,41,73,94], Max = 28
%@ ;  % CPU time: 1.020s
%@    Is = [1,3,6,9,12,15,18,20,23,25,27,30,33,36,39,41,43,46,48,51,...], Negatives = [1,25,41,73,97], Max = 28
%@ ;  ... .

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Independent set:

   IND(X) = not OR_(u->v){ x_u /\ x_v } = AND_(u->v){not x_u \/ not x_v}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- time((independent_set(I,_),sat_count(I,N))).
%@ % 2,101,745 inferences, 0.245 CPU in 0.256 seconds (96% CPU, 8594864 Lips)
%@ I = *([~_G69+ ~_G70, ~_G70+ ~_G71, ~_G71+ ~_G72, ~_G72+ ~_G73, ~_G73+ ~_G74, ~_G74+ ~_G75, ~ ... + ~ ..., ... + ...|...]),
%@ N = 792070839848372253127.


independent_set(*(NBs), Assoc) :-
        findall(U-V, edge(U, V), Edges),
        setof(U, V^(member(U-V, Edges);member(V-U, Edges)), Nodes),
        pairs_keys_values(Pairs, Nodes, _),
        list_to_assoc(Pairs, Assoc),
        maplist(not_both(Assoc), Edges, NBs).

not_both(Assoc, U-V, ~BU + ~BV) :-
        get_assoc(U, Assoc, BU),
        get_assoc(V, Assoc, BV).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Kernels.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

kernel(Ind* *(Ors), Assoc) :-
        independent_set(Ind, Assoc),
        assoc_to_keys(Assoc, Nodes),
        maplist(node_or(Assoc), Nodes, Ors).

node_or(Assoc, Node, Var + +(Vars)) :-
        get_assoc(Node, Assoc, Var),
        setof(U-Node, (edge(U, Node);edge(Node,U)), Edges),
        pairs_keys(Edges, Us),
        maplist(u_to_var(Assoc), Us, Vars).

u_to_var(Assoc, Node, Var) :- get_assoc(Node, Assoc, Var).

%?- kernel(Sat, _), sat_count(Sat, C).
%@    Sat = *(...)* *(...), C = 1630580875002
%@ ;  ... .
