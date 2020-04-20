:- use_module(library(dcgs)).
:- use_module(library(lists)).

:- use_module(presprover).

%?- length(Ls, 4), prestmt([x,y,z], Expr, Ls, _), writeln(Expr), false.

%?- run(1).
%@ false.
%@ false.
%@ false.

%?- valid((x < 5 => exists(y, y = 2))).

run(N) :-
        Vs = [x,y],
        statement(N, Vs, S1),
        (   myvalid(S1) ->
            statement(N, Vs, S2),
            syntax_ok(S2),
            must_hold(myvalid(S2 ==> S1)),
            (   myvalid(S2) ->
                must_hold(myvalid(S1 /\ S2)),
                must_hold(myvalid(not(not(S1) \/ not(S2))))
            ;   must_hold(mysatisfiable(not(S1/\S2)))
            )
        ;   true
        ),
        false.

myvalid(S) :-
        (   syntax_ok(S) -> valid(S)
        ;   true
        ).

mysatisfiable(S) :-
        (   syntax_ok(S) -> satisfiable(S)
        ;   true
        ).

syntax_ok(S) :-
        catch(ignore(valid(S)),_,Exc = true),
        (   Exc == true -> false %writeln(invalid-S), false
        ;   true
        ).

must_hold(G) :-
        (   G -> true
        ;   throw('failed_but_must_hold'-G)
        ).

statement(Limit, Vs, S) :-
        length(Ls, Limit),
        phrase(prestmt(Vs,S), Ls).

prestmt(Vs, Expr) --> prexpr(Vs, Expr).
prestmt(Vs, A /\ B) --> [_], prestmt(Vs, A), prestmt(Vs, B).
prestmt(Vs, A \/ B) --> [_], prestmt(Vs, A), prestmt(Vs, B).
prestmt(Vs, A ==> B) --> [_], prestmt(Vs, A), prestmt(Vs, B).
prestmt(Vs, not(A)) --> [_], prestmt(Vs, A).
prestmt(Vs, exists(V, A)) --> [_], { member(V, Vs) }, prestmt(Vs, A).
prestmt(Vs, forall(V, A)) --> [_], { member(V, Vs) }, prestmt(Vs, A).


prexpr(Vs, V < 5) --> [_], { member(V, Vs) }.
prexpr(Vs, V > 3) --> [_], { member(V, Vs) }.
prexpr(Vs, V = 2) --> { member(V, Vs) }.
prexpr(Vs, V1 = V2) -->  { member(V1, Vs), member(V2, Vs), V1 @< V2 }.
prexpr(Vs, V1 + V2 = V3) -->
        { member(V1, Vs), member(V2, Vs), V1 @< V2, member(V3, Vs), V2 @< V3 }.
%prexpr(Vs, V1 < V2) -->  { member(V1, Vs), member(V2, Vs), V1 @< V2 }.


