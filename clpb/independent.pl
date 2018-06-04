:- use_module(library(clpb)).

:- include(edges).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Independent set:

   IND(X) = not OR_(u->v){ x_u /\ x_v } = AND_(u->v){not x_u \/ not x_v}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- independent_set(Sat), time(sat_count(Sat, Count)).
%@ % 14,002,141 inferences, 1.563 CPU in 1.575 seconds (99% CPU, 8956017 Lips)
%@ Sat = *(...),
%@ Count = 211954906.

independent_set(*(NBs)) :-
        findall(U-V, edge(U, V), Edges),
        setof(U, V^(member(U-V, Edges);member(V-U, Edges)), Nodes),
        pairs_keys_values(Pairs, Nodes, _),
        list_to_assoc(Pairs, Assoc),
        maplist(not_both(Assoc), Edges, NBs).

not_both(Assoc, U-V, ~BU + ~BV) :-
        get_assoc(U, Assoc, BU),
        get_assoc(V, Assoc, BV).

