:- use_module(library(clpfd)).  % for SICStus, SWI
:- use_module(library(lists)).  % for SICStus
:- use_module(library(terms)).  % for SICStus

pairs_keys_values([], [], []).  % for SICStus, GNU
pairs_keys_values([A-B|ABs], [A|As], [B|Bs]) :-
        pairs_keys_values(ABs, As, Bs).

forall(Pred, Goal) :-           % for SICStus
        \+ ( Pred, \+ Goal ).

foldl(Goal_3, Ls, A0, A) :-     % for SICStus, GNU
        foldl_(Ls, Goal_3, A0, A).

foldl_([], _, A, A).
foldl_([L|Ls], G_3, A0, A) :-
        call(G_3, L, A0, A1),
        foldl_(Ls, G_3, A1, A).
:- op(800, xfx, ==>).

step([L==>R|Rs], T0, T) :-
        (   subsumes_term(L, T0) ->
            copy_term(L-R, T0-T)
        ;   step(Rs, T0, T)
        ).
normal_form(Rs, T0, T) :-
        (   var(T0) -> T = T0
        ;   T0 =.. [F|Args0],
            maplist(normal_form(Rs), Args0, Args1),
            T1 =.. [F|Args1],
            (   step(Rs, T1, T2) ->
                normal_form(Rs, T2, T)
            ;   T = T1
            )
        ).
ord(Fs, F1, F2, Ord) :-
        once((nth0(N1, Fs, F1),
              nth0(N2, Fs, F2))),
        compare(Ord, N1, N2).
lex(Cmp, Xs, Ys, Ord) :- lex_(Xs, Ys, Cmp, Ord).

lex_([], [], _, =).
lex_([X|Xs], [Y|Ys], Cmp, Ord) :-
        call(Cmp, X, Y, Ord0),
        (   Ord0 == (=) -> lex_(Xs, Ys, Cmp, Ord)
        ;   Ord = Ord0
        ).
rpo(Fs, Stats, S, T, Ord) :-
        (   var(T) ->
            (   S == T -> Ord = (=)
            ;   term_variables(S, Vs),
                member(V, Vs), V == T -> Ord = (>)
            ;   Ord = (<)
            )
        ;   var(S) -> Ord = (<)
        ;   S =.. [F|Ss], T =.. [G|Ts],
            (   forall(member(Si, Ss),
                       rpo(Fs, Stats, Si, T, (<))) ->
                ord(Fs, F, G, Ord0),
                (   Ord0 == (>) ->
                    (   forall(member(Ti, Ts),
                               rpo(Fs, Stats, S, Ti, >)) ->
                        Ord = (>)
                    ;   Ord = (<)
                    )
                ;   Ord0 == (=) ->
                    (   forall(member(Ti, Ts),
                               rpo(Fs, Stats, S, Ti, >)) ->
                        memberchk(F-Stat, Stats),
                        call(Stat, rpo(Fs, Stats), Ss, Ts, Ord)
                    ;   Ord = (<)
                    )
                ;   Ord0 == (<) -> Ord = (<)
                )
            ;   Ord = (>)
            )
        ).
context(conc(F,Ls,Rs), Arg, T) :-
        append(Ls, [Arg|Rs], Ts),
        T =.. [F|Ts].
roots_cps([], _, _) --> [].
roots_cps([Left0==>Right0|Rules], L0==>R0, Cs0) -->
        { copy_term(f(L0,R0,Cs0), f(L,R,Cs)),
          copy_term(Left0-Right0, Left-Right) },
        (   { unify_with_occurs_check(L, Left) } ->
            { foldl(context, Cs, Right, Reduced) },
            [R=Reduced]
        ;   []
        ),
        roots_cps(Rules, L0==>R0, Cs0).
critical_pairs([], _) --> [].
critical_pairs([R|Rs], Rules) -->
        rule_cps(R, Rules, []),
        critical_pairs(Rs, Rules).

rule_cps(L ==> R, Rules, Cs) -->
        (   { var(L) } -> []
        ;   roots_cps(Rules, L ==> R, Cs),
            { L =.. [F|Args] },
            inner_cps(Args, F, [], R, Rules, Cs)
        ).

inner_cps([], _, _, _, _, _) --> [].
inner_cps([T|Ts], F, Ls0, R, Rules, Cs) -->
        { reverse(Ls0, Ls) },
        rule_cps(T ==> R, Rules, [conc(F,Ls,Ts)|Cs]),
        inner_cps(Ts, F, [T|Ls0], R, Rules, Cs).
completion(Es0, Cmp, Ss0, Rs0, Rs) :-
        orient(Es0, Cmp, Ss0, Ss1, Rs0, Rs1),
        (   Ss1 == [] -> Rs = Rs1
        ;   smallest_rule_first(Ss1, [R|Ss]),
            phrase((critical_pairs([R], Rs1),
                    critical_pairs(Rs1, [R]),
                    critical_pairs([R], [R])), CPs),
            completion(CPs, Cmp, Ss, [R|Rs1], Rs)
        ).
orient([], _, Ss, Ss, Rs, Rs).
orient([S0=T0|Es0], Cmp, Ss0, Ss, Rs0, Rs) :-
        append(Rs0, Ss0, Rules),
        maplist(normal_form(Rules), [S0,T0], [S,T]),
        (   S == T -> orient(Es0, Cmp, Ss0, Ss, Rs0, Rs)
        ;   (   call(Cmp, S, T, >) -> Rule = (S ==> T)
            ;   call(Cmp, T, S, >) -> Rule = (T ==> S)
            ;   false /* identity cannot be oriented */
            ),
            foldl(simpler(Rule, Rules), Ss0, Es0-[], Es1-Ss1),
            foldl(simpler(Rule, Rules), Rs0, Es1-[], Es-Rs1),
            orient(Es, Cmp, [Rule|Ss1], Ss, Rs1, Rs)
        ).

simpler(Rule, Rules, L0==>R0, Es0-Us0, Es-Us) :-
        normal_form([Rule], L0, L),
        (   L0 == L ->
            normal_form([Rule|Rules], R0, R),
            Es-Us = Es0-[L==>R|Us0]
        ;   Es-Us = [L=R0|Es0]-Us0
        ).
rule_size(T, S) :-
        (   var(T) -> S #= 1
        ;   T =.. [_|Args],
            foldl(rule_size_, Args, 0, S0),
            S #= S0 + 1
        ).

rule_size_(T, S0, S) :-
        rule_size(T, TS),
        S #= S0 + TS.
smallest_rule_first(Rs0, Rs) :-
        maplist(rule_size, Rs0, Sizes0),
        pairs_keys_values(Pairs0, Sizes0, Rs0),
        keysort(Pairs0, Pairs),
        pairs_keys_values(Pairs, _, Rs).
functors_order(Functors0, rpo(Functors,Pairs)) :-
        pairs_keys_values(Pairs, Functors0, Status),
        maplist(=(lex), Status),
        permutation(Functors0, Functors).
