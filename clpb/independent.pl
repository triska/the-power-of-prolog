:- use_module(library(clpb)).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(pairs)).
:- use_module(library(time)).
:- use_module(library(format)).

:- use_module(edges).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Independent set:

   IND(X) = not OR_(u->v){ x_u /\ x_v } = AND_(u->v){not x_u \/ not x_v}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- independent_set(Sat), time(sat_count(Sat, Count)).
%@    % CPU time: 6.306s
%@    Sat = *(...), Count = 211954906

independent_set(*(NBs)) :-
        findall(U-V, edge(U, V), Edges),
        findall(U, (member(U-V, Edges);member(V-U, Edges)), Nodes0),
        sort(Nodes0, Nodes),
        pairs_keys_values(Pairs, Nodes, _),
        list_to_assoc(Pairs, Assoc),
        maplist(not_both(Assoc), Edges, NBs).

not_both(Assoc, U-V, ~BU + ~BV) :-
        get_assoc(U, Assoc, BU),
        get_assoc(V, Assoc, BV).

