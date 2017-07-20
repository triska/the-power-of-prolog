% Written 2005-2016 by Markus Triska (triska@metalevel.at)
% Public domain code.

:- use_module(library(clpfd)).

natnum(0).
natnum(s(X)) :-
        natnum(X).


mi1(true).
mi1((A,B)) :-
        mi1(A),
        mi1(B).
mi1(G) :-
        G \= true,
        G \= (_,_),
        clause(G, Body),
        mi1(Body).

%complicated_clause(A) :-
%       goal1(A),
%       goal2(A),
%       goal3(A).

mi_clause(Goal, Body) :-
        clause(Goal, Body0),
        defaulty_better(Body0, Body).

defaulty_better(true, true).
defaulty_better((A,B), (BA,BB)) :-
        defaulty_better(A, BA),
        defaulty_better(B, BB).
defaulty_better(G, g(G)) :-
        G \= true,
        G \= (_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

natnum_clean(0).
natnum_clean(s(X)) :-
        g(natnum_clean(X)).

integer_natnum(0, 0).
integer_natnum(N, s(T)) :-
        N #> 0,
        N1 #= N - 1,
        integer_natnum(N1, T).

mi2(true).
mi2((A,B)) :-
        mi2(A),
        mi2(B).
mi2(g(G)) :-
        clause(G, Body),
        mi2(Body).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_goal(natnum(_)).

mi2_safe(true).
mi2_safe((A,B)) :-
        mi2_safe(A),
        mi2_safe(B).
mi2_safe(g(G)) :-
        (   safe_goal(G) ->
            mi_clause(G, Body),
            mi2_safe(Body)
        ;   throw(cannot_execute(G))
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declarative_false :-
        declarative_false,
        false.

mi3(true).
mi3((A,B)) :-
        mi3(B),
        mi3(A).
mi3(g(G)) :-
        clause(G, Body),
        mi3(Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mi_list_clause(G, Ls) :-
        clause(G, Body),
        phrase(body_list(Body), Ls).


body_list(true)  --> [].
body_list((A,B)) -->
        body_list(A),
        body_list(B).
body_list(G) -->
        { G \= true },
        { G \= (_,_) },
        [G].

always_infinite :- always_infinite.

mi_list1([]).
mi_list1([G|Gs]) :-
        mi_list_clause(G, Body),
        mi_list1(Body),
        mi_list1(Gs).

mi_list2([]).
mi_list2([G0|Gs0]) :-
        mi_list_clause(G0, Body),
        append(Body, Gs0, Gs),
        mi_list2(Gs).


mi_ldclause(natnum(0), Rest, Rest).
mi_ldclause(natnum(s(X)), [natnum(X)|Rest], Rest).

mi_list3([]).
mi_list3([G0|Gs0]) :-
        mi_ldclause(G0, Remaining, Gs0),
        mi_list3(Remaining).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- op(750, xfy, =>).

mi_tree(true, true).
mi_tree((A,B), (TA,TB)) :-
        mi_tree(A, TA),
        mi_tree(B, TB).
mi_tree(g(G), TBody => G) :-
        mi_clause(G, Body),
        mi_tree(Body, TBody).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mi_limit(Goal, Max) :-
        mi_limit(Goal, Max, _).

mi_limit(true, N, N).
mi_limit((A,B), N0, N) :-
        mi_limit(A, N0, N1),
        mi_limit(B, N1, N).
mi_limit(g(G), N0, N) :-
        N0 #> 0,
        N1 #= N0 - 1,
        mi_clause(G, Body),
        mi_limit(Body, N1, N).


mi_id(Goal) :-
        length(_, N),
        mi_limit(Goal, N).

edge(a, b).
edge(b, a).
edge(b, c).

path(A, A, []).
path(A, C, [e(A,B)|Es]) :-
        edge(A, B),
        path(B, C, Es).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


occ(X, f(X)).

mi_occ(true).
mi_occ((A,B)) :-
        mi_occ(A),
        mi_occ(B).
mi_occ(g(G)) :-
        functor(G, F, Arity),
        functor(H, F, Arity),
        mi_clause(H, Body),
        unify_with_occurs_check(G, H),
        mi_occ(Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mi_circ(true).
mi_circ((A,B)) :-
        mi_circ(A),
        mi_circ(B).
mi_circ(clause(A,B)) :-
        clause(A,B).
mi_circ(A \= B) :-
        A \= B.
mi_circ(G) :-
        G \= true,
        G \= (_,_),
        G \= (_\=_),
        G \= clause(_,_),
        clause(G, Body),
        mi_circ(Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

as([]).
as([a]).   % redundant
as([a,a]). % redundant
as([A|As]) :-
        A = a,           % test built-in (=)/2
        true,            % test built-in true/0
        as(As).

redundant(Functor/Arity, Reds) :-
        functor(Term, Functor, Arity),
        findall(Term-Body, clause(Term, Body), Defs),
        setof(Red, Defs^redundant_(Defs, Red), Reds).

redundant_(Defs, Fact) :-
        select(Fact-true, Defs, Rest),
        once(provable(Fact, Rest)).


provable(true, _) :- !.
provable((G1,G2), Defs) :- !,
        provable(G1, Defs),
        provable(G2, Defs).
provable(BI, _) :-
        predicate_property(BI, built_in),
        !,
        call(BI).
provable(Goal, Defs) :-
        member(Def, Defs),
        copy_term(Def, Goal-Body),
        provable(Body, Defs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resstep_([A|As0], As) :-
        findall(Gs-G, (A = [G0|Rest]-G,mi_ldclause(G0,Gs,Rest)), As, As0).

mi_backtrack(G0) :- mi_backtrack_([[G0]-G0], G0).

mi_backtrack_([[]-G|_], G).
mi_backtrack_(Alts0, G) :-
        resstep_(Alts0, Alts1),
        mi_backtrack_(Alts1, G).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


dcgnumber(0).
dcgnumber(1).

expr(N)   --> [N], { dcgnumber(N) }.
expr(A+B) --> expr(A), [(+)], expr(B).


dcg_clause(expr(N),   [t(N),{dcgnumber(N)}]).
dcg_clause(expr(A+B), [l,nt(expr(A)),t(+),nt(expr(B))]).

mi_dcg(t(T), Rest, Rest, [T|Ts], Ts).
mi_dcg({Goal}, Rest, Rest, Ts, Ts) :-
        call(Goal).
mi_dcg(nt(NT), Rest0, Rest, Ts0, Ts) :-
        dcg_clause(NT, Body),
        mi_dcg_(Body, Rest0, Rest, Ts0, Ts).
mi_dcg(l, [_|Rest], Rest, Ts, Ts).

mi_dcg_([], Rest, Rest, Ts, Ts).
mi_dcg_([G|Gs], Rest0, Rest, Ts0, Ts) :-
        mi_dcg(G, Rest0, Rest1, Ts0, Ts1),
        mi_dcg_(Gs, Rest1, Rest, Ts1, Ts).

mi_dcg(NT, String) :-
        length(String, L),
        length(Rest0, L),
        mi_dcg_([nt(NT)], Rest0, _, String, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pe_expr(Expr, String) :-
        length(String, L),
        length(Rest0, L),
        pe_expr(Expr, Rest0, _, String, []).

pe_expr(N, Rest, Rest, Ts0, Ts) :-
        Ts0 = [N|Ts],
        dcgnumber(N).
pe_expr(A+B, [_|Rest0], Rest, Ts0, Ts) :-
        pe_expr(A, Rest0, Rest1, Ts0, Ts1),
        Ts1 = [+|Ts2],
        pe_expr(B, Rest1, Rest, Ts2, Ts).

sum_of_ones(0, [1]).
sum_of_ones(N0, [1,+|Rest]) :-
        N0 #> 0,
        N1 #= N0 - 1,
        sum_of_ones(N1, Rest).

%?- sum_of_ones(10^3, Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1) Ackermann function

ack0(0, N, s(N)) :-
        natnum(N).
ack0(s(M), 0, Res) :-
        ack0(M, s(0), Res).
ack0(s(M), s(N), Res) :-
        ack0(s(M), N, Res1),
        ack0(M, Res1, Res).

% 2) Make unifications explicit (can be automated)

ack1(A, B, C) :-
        A = 0,
        C = s(B),
        natnum(B).
ack1(A, B, C) :-
        A = s(M),
        B = 0,
        D = s(0),
        ack1(M, D, C).
ack1(A, B, C) :-
        A = s(M),
        B = s(N),
        ack1(A, N, Res1),
        ack1(M, Res1, C).

% 3) Change representation to list of goals (can be automated)

ack(ack(A,B,C), [A = 0, C = s(B), natnum(B)]).
ack(ack(A,B,C), [A = s(M), B = 0, D = s(0), ack(M,D,C)]).
ack(ack(A,B,C), [A = s(M), B = s(N), ack(A,N,Res1), ack(M,Res1,C)]).

% Unification over abstract parity domain {zero, one, even, odd}

unify(X, Y) :-
        must_be(ground, Y),
        (   Y = 0 -> X = zero
        ;   abstract_natnum(Y) -> X = Y
        ;   Y = s(T) ->
            unify(T1, T),
            (   T1 = zero -> X = one
            ;   T1 = one ->  X = even
            ;   T1 = even -> X = odd
            ;   T1 = odd ->  X = even
            ;   domain_error(unif, Y)
            )
        ;   domain_error(unif, Y)
        ).

abstract_natnum(zero).
abstract_natnum(one).
abstract_natnum(even).
abstract_natnum(odd).


ack_fixpoint(Ds) :-
        findall(Head-Body, ack(Head, Body), Clauses0),
        maplist(body_permutation, Clauses0, Clauses),
        catch(ack_fixpoint(Clauses, [], Ds), error(instantiation_error,_), false).

body_permutation(Head-Body0, Head-Body) :-
        permutation(Body0, Body).

ack_fixpoint(Clauses, Ds0, Ds) :-
        ack_derive(Clauses, Ds0, Ds1),
        (   same_length(Ds0, Ds1) ->
            Ds = Ds0
        ;   ack_fixpoint(Clauses, Ds1, Ds)
        ).


ack_derive(Clauses, Derived0, Heads) :-
        findall(Head, ack_derive_(Clauses, Derived0, Head), Heads0, Derived0),
        sort(Heads0, Heads).

ack_derive_(Clauses, Derived0, Head) :-
        member(Clause, Clauses),
        copy_term(Clause, Head-Body),
        maplist(ack_prove(Derived0), Body).

ack_prove(_, A = B)            :- unify(A, B).
ack_prove(_, natnum(X))        :- abstract_natnum(X).
ack_prove(Derived, ack(A,B,C)) :- member(ack(A,B,C), Derived).

%?- ack_fixpoint(Ds).
