/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Reason about Term Rewriting Systems.
   Written 2015-2022 by Markus Triska (triska@metalevel.at)
   Public domain code. Tested with Scryer Prolog.

   Motivating example
   ==================

      Consider a set S that is closed under the binary operation *,
      satisfying the equations:

          1) e*X = X
          2) i(X)*X = e
          3) X*(Y*Z) = (X*Y)*Z

      The algebraic structure <S, *> is called a group.

   From these equations, we can infer additional identities, such as:

      e*X = (i(i(X))*i(X))*X =
          = i(i(X))*(i(X)*X) =
          = i(i(X))*e

   Other identities that follow from these equations are i(i(X)) = X,
   i(e) = e, and many others.

   However, it is not immediately clear which identities are implied
   by these equations. In many cases, new terms must be inserted into
   equations in order to derive further identities, and it is not
   clear how far an ongoing derivation must be extended to derive a
   new identity, or if that is possible at all.

   Under certain conditions, we can convert such a set of equations
   into a set of oriented rewrite rules that always terminate and
   reduce identical elements to the same normal form. We call such a
   set of rewrite rules a convergent term rewriting system (TRS).

   For example (see group/1 below):

      ?- group(Gs), equations_trs(Gs, Rs), maplist(portray_clause, Rs).

   yielding the convergent TRS:

      i(A*B)==>i(B)*i(A).
      A*i(A)==>e.
      i(i(A))==>A.
      A*e==>A.
      A*B*C==>A*(B*C).
      i(A)*A==>e.
      e*A==>A.
      i(A)*(A*B)==>B.
      i(e)==>e.
      A*(i(A)*B)==>B.

   From this, we see that i(i(X)) = X is one of the consequences of
   the equations above. To see whether two terms are identical under
   the given equations, we can now simply check whether they reduce to
   the same normal form under the computed rewrite rules:

      ?- group(Gs), equations_trs(Gs, Rs),
         normal_form(Rs, i(i(X)), NF),
         normal_form(Rs, i(i(i(i(X)))), NF).
      ...
      X = NF .
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(pairs)).
:- use_module(library(iso_ext)).
:- use_module(library(format)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Variables in equations and TRS are represented by Prolog variables.

   A major advantage of this representation is that efficient built-in
   Prolog predicates can be used for unification etc. The terms are
   also easier to read and type for users when specifying a TRS.
   However, care must be taken not to accidentally unify variables
   that are supposed to be different. copy_term/2 must be used when
   necessary to prevent this. Conversely, we also must retain all
   bindings that are supposed to hold.

   We use:

      Left ==> Right

   to denote a rewrite rule. A TRS is a list of such rules.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- op(800, xfx, ==>).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Perform one rewriting step at the root position, using the first
   matching rule, if any.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

step([L==>R|Rs], T0, T) :-
        (   subsumes_term(L, T0) ->
            copy_term(L-R, T0-T)
        ;   step(Rs, T0, T)
        ).

%?- step([f(a) ==> f(a), f(X) ==> b], f(a), T).
%?- step([g(f(X)) ==> X], g(Y), T).
%?- step([f(X) ==> b, f(a) ==> f(a)], f(a), T).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Reduce to normal form. May not terminate!
   For example: R = { a -> a, f(x) -> b },
   although f(a) does have a normal form!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- normal_form([f(X) ==> b, a ==> a], f(a), T).
%?- normal_form([a ==> a, f(X) ==> b], f(a), T).

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Critical Pairs
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- critical_pairs([X==>a, Y ==> b], Ps).

critical_pairs(Rs, CPs) :-
        phrase(critical_pairs_(Rs, Rs), CPs).

critical_pairs_([], _) --> [].
critical_pairs_([R|Rs], Rules) -->
        rule_cps(R, Rules, []),
        critical_pairs_(Rs, Rules).

rule_cps(T ==> R, Rules, Cs) -->
        (   { var(T) } -> []
        ;   roots_cps(Rules, T ==> R, Cs),
            { T =.. [F|Ts] },
            inner_cps(Ts, F, [], R, Rules, Cs)
        ).

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

inner_cps([], _, _, _, _, _) --> [].
inner_cps([T|Ts], F, Left0, R, Rules, Cs) -->
        { reverse(Left0, Left) },
        rule_cps(T ==> R, Rules, [conc(F,Left,Ts)|Cs]),
        inner_cps(Ts, F, [T|Left0], R, Rules, Cs).

context(conc(F,Ts1,Ts2), Arg, T) :-
        append(Ts1, [Arg|Ts2], Ts),
        T =.. [F|Ts].

%?- foldl(context, [conc(f,[x],[y]),conc(g,[a],[b])], -, R).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Lexicographic order.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- ord([a,b,c], b, a, Ord).

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Multiset order.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- foldl(subtract_element(ord([a,b,c])), [a], [a,a,b,c], Rs).
%?- multiset_diff(ord([a,b,c]), [a,a,b,b], [a,b,c], Ds).

multiset_diff(Cmp, Xs0, Ys, Xs) :-
        foldl(subtract_element(Cmp), Ys, Xs0, Xs).

subtract_element(Cmp, Y, Xs0, Xs) :- subtract_first(Xs0, Y, Cmp, Xs).

subtract_first([], _, _, []).
subtract_first([X|Xs], Y, Cmp, Rs) :-
        (   call(Cmp, X, Y, =) -> Rs = Xs
        ;   Rs = [X|Rest],
            subtract_first(Xs, Y, Cmp, Rest)
        ).

mul(Cmp, Ms, Ns, Ord) :-
        multiset_diff(Cmp, Ns, Ms, NMs),
        multiset_diff(Cmp, Ms, Ns, MNs),
        (   NMs == [], MNs == [] -> Ord = (=)
        ;   forall(member(N, NMs),
                   (   member(M, MNs), call(Cmp, M, N, >))) -> Ord = (>)
        ;   Ord = (<)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Recursive path order with status.

   Stats is a list of pairs [f-mul, g-lex] etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

rpo(Fs, Stats, S, T, Ord) :-
        (   var(T) ->
            (   S == T -> Ord = (=)
            ;   term_variables(S, Vs), member(V, Vs), V == T -> Ord = (>)
            ;   Ord = (<)
            )
        ;   var(S) -> Ord = (<)
        ;   S =.. [F|Ss], T =.. [G|Ts],
            (   forall(member(Si, Ss), rpo(Fs, Stats, Si, T, <)) ->
                ord(Fs, F, G, Ord0),
                (   Ord0 == (>) ->
                    (   forall(member(Ti, Ts), rpo(Fs, Stats, S, Ti, >)) ->
                        Ord = (>)
                    ;   Ord = (<)
                    )
                ;   Ord0 == (=) ->
                    (   forall(member(Ti, Ts), rpo(Fs, Stats, S, Ti, >)) ->
                        memberchk(F-Stat, Stats),
                        call(Stat, rpo(Fs, Stats), Ss, Ts, Ord)
                    ;   Ord = (<)
                    )
                ;   Ord0 == (<) -> Ord = (<)
                )
            ;   Ord = (>)
            )
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Huet / Knuth-Bendix Completion
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- rule_size(f(g(X),y), T).

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

%?- smallest_rule_first([f(g(X)) ==> c, f(X) ==> b], Rs).

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

completion(Es0, Cmp, Ss0, Rs0, Rs) :-
        orient(Es0, Cmp, Ss0, Ss1, Rs0, Rs1),
        (   Ss1 == [] -> Rs = Rs1
        ;   smallest_rule_first(Ss1, [R|Ss]),
            phrase((critical_pairs_([R], Rs1),
                    critical_pairs_(Rs1, [R]),
                    critical_pairs_([R], [R])), CPs),
            completion(CPs, Cmp, Ss, [R|Rs1], Rs)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Try to find a suitable order to create a convergent TRS from
   a list of equations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

equations_trs(Es, Rs) :-
        equations_order(Es, Cmp),
        equations_trs(Cmp, Es, Rs).

equations_trs(Cmp, Es, Rs) :-
        completion(Es, Cmp, [], [], Rs).

equations_order(Es, rpo(Sorted,Stats)) :-
        equations_functors(Es, Fs),
        pairs_keys_values(Stats, Fs, Values),
        maplist(ord_status, Values),
        permutation(Fs, Sorted).

ord_status(lex).
ord_status(mul).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Functors occurring in given equations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

equations_functors(Eqs, Fs) :-
        phrase(eqs_functors_(Eqs), Fs0),
        sort(Fs0, Fs).

eqs_functors_([]) --> [].
eqs_functors_([A=B|Es]) -->
        term_functors(A),
        term_functors(B),
        eqs_functors_(Es).

term_functors(Var) --> { var(Var) }, !.
term_functors(T) -->
        { T =.. [F|Args] },
        [F],
        functors_(Args).

functors_([]) --> [].
functors_([T|Ts]) -->
        term_functors(T),
        functors_(Ts).

%?- group(Gs), equations_functors(Gs, Fs).

%?- group(Gs), equations_trs(Gs, Rs).

%?- group(Gs), permutation([*,e,i], Ord), equations_trs(rpo(Ord, [(*)-lex,e-lex,i-lex]), Gs, Rs), maplist(portray_clause, Rs).
%?- group(Gs), equations_trs(rpo([*,e,i],[(*)-lex,e-lex,i-lex]), Gs, Rs), maplist(portray_clause, Rs).

%?- group(Gs), equations_trs(rpo([e,*,i],[(*)-lex,e-lex,i-lex]), Gs, Rs), maplist(portray_clause, Rs), length(Rs, L).

%?- group(Gs), equations_trs(rpo([*,i,e],[(*)-lex,e-lex,i-lex]), Gs, Rs), maplist(portray_clause, Rs), length(Rs, L).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Testing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

rules(1, [f(f(X)) ==> g(X)]).

rules(2, [f(f(X)) ==> f(X),
          g(g(X)) ==> f(X)]).

c(CPs) :-
        rules(_, Rules),
        critical_pairs(Rules, CPs).

group([e*X = X,
       i(X)*X = e,
       A*(B*C) = (A*B)*C]).

orient(A=B, A==>B).

%?- critical_pairs([f(X)*Y*Z==>X*Y*Z], Ps).

%?- critical_pairs([i(X) ==> e, A*B*C ==> (A*B)*C], Ps).

%?- critical_pairs([A*B*C ==> (A*B)*C], Ps).

%?- critical_pairs([A*B*D ==> A*B], Ps).

%?- group(Gs0), maplist(orient, Gs0, Gs), critical_pairs(Gs, Ps), maplist(portray_clause, Ps), length(Ps, L).

%?- critical_pairs([f(f(X)) ==> a, f(f(X))==>b], Ps).
%?- c(CPs).
%@    CPs = [g(_A)=g(_A),g(f(_B))=f(g(_B))]
%@ ;  CPs = [f(_A)=f(_A),f(f(_B))=f(f(_B)),f(_C)=f(_C),f(g(_D))=g(f(_D))].

%?- critical_pairs([f(X,a) ==> X, a ==> b], Ps).

%?- rules(1, Rs), critical_pairs(Rs, Ps).

%?- critical_pairs([f(X,f(X)) ==> a, f(Y,Y) ==> b], Ps).

/** <Examples>

?- group(Gs), equations_trs(Gs, Rs).

?- group(Gs), equations_order(Gs, Cmp), equations_trs(Cmp, Gs, Rs).

?- Es = [X*X = X^2, (X+Y)^2 = X^2 + 2*X*Y + Y^2],
   equations_order(Es, Cmp),
   call_with_inference_limit(equations_trs(Cmp, Es, Rs), 10000, !).
*/

