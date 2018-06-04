/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Definitions for benchmarking.

   bench(+System, +B, +N, +Goal) produces the output for benchmark B(N),
   where Goal is either sat, sats or taut.

   latex_table/0 generates the table from the collected facts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpb)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

is_swi :- catch(current_prolog_flag(dialect, swi), _, fail).

:- if(is_swi).
:- set_prolog_flag(double_quotes, codes).
:- else.
:- use_module(library(between)).
pairs_keys_values(Ps, Ks, Vs) :-
        keys_and_values(Ps, Ks, Vs).
:- endif.

list_conj([], 1).
list_conj([L|Ls], L*Goal) :- list_conj(Ls, Goal).

bench(System, What, N, Goal) :-
        call(What, N, Sats),
        length(Sats, NumClauses),
        term_variables(Sats, Vars),
        length(Vars, NumVars),
        (   Goal == sat ->
            format("% ~w ~w\n", [What,N]),
            portray_clause(instance_attributes(System, What, N, num_vars_clauses(NumVars, NumClauses)))
        ;   true
        ),
        bench_(Goal, System, What, N, Sats, Time),
        portray_clause(timing(System, What, N, Goal, Time)),
        halt.

bench_(sat, System, What, N, Sats, Time) :-
        list_conj(Sats, Sat),
        goal_time(System, What, N, sat(Sat), Time).
bench_(sats, System, What, N, Sats, Time) :-
        goal_time(System, What, N, maplist(sat, Sats), Time).
bench_(taut, System, What, N, Sats, Time) :-
        list_conj(Sats, Sat),
        goal_time(System, What, N, taut(Sat, T), Time),
        (   nonvar(T) ->
            portray_clause(tautology(System, What, N, T))
        ;   true
        ).

%?- list_goal([5,3], G).

:- if(current_predicate(b_getval/2)).

num_nodes :-
        b_getval('$clpb_next_node', N0),
        N #= N0 - 1,
        format('num nodes: ~w\n', [N]).

:- else.
num_nodes.
:- endif.

goal_time(System, What, N, Goal, Time) :-
        statistics(runtime, [T0,_]),
        (   catch(Goal, error(resource_error(memory), _), E = memory) ->
            true
        ;   functor(Goal, F, A),
            portray_clause(failed(System, What, N, F/A))
        ),
        statistics(runtime, [T1,_]),
        T #= T1 - T0,
        (   nonvar(E) ->
            Time = exception(E, T)
        ;   Time = time(T)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Generating tables from collected results.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

instances(INs) :-
        setof(I-N, V^C^instance(I,N,V,C), INs).

instance(I, N, V, C) :-
        instance_attributes(swi, I, N, num_vars_clauses(V,C)).

timing_(System, What, N, Goal, T) :-
        (   timing(System, What, N, Goal, T) -> true
        ;   T = na
        ).

time0_time(time(T0), T) :-
        Seconds is T0 / 1000,
        (   Seconds > 1000 ->
            T1 is round(Seconds),
            T = integer(T1)
        ;   T = float(Seconds)
        ).
time0_time(exception(_,_), exception).
time0_time(na, na).

%?- instances(Is), maplist(writeln, Is).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   LaTeX
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

latex_table :-
        instances(Is),
        maplist(latex_instance, Is).

latex_instance(I-N) :-
        instance(I, N, V, C),
        format("\\texttt{~w~w} & ~w & ~w & ", [I,N,V,C]),
        maplist(timing_(swi, I, N), [sat,sats,taut], SWIs),
        maplist(timing_(sicstus, I, N), [sat,sats,taut], SICSs),
        append(SWIs, SICSs, Times0),
        maplist(time0_time, Times0, Times1),
        times_separators(Times1, Cols),
        maplist(latex_col, Cols),
        format("\\\\\n").

latex_col(sep)        :- format(" & ").
latex_col(integer(T)) :- format("$~w$", [T]).
latex_col(float(T))   :- format("$~2f$", [T]).
latex_col(na)         :- format("NA").
latex_col(exception)  :- format("--").

times_separators([], []).
times_separators([T], [T]).
times_separators([T1,T2|Ts0], [T1,sep|Ts]) :-
        times_separators([T2|Ts0], Ts).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ASCII table
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ascii_table :-
        format("~t~30|~tSWI~t~30+~tSICStus~t~30+\n"),
        format("~t~30|~tsat~t~10+~tsats~t~10+~ttaut~t~10+~tsat~t~10+~tsats~t~10+~ttaut~t~10+\n"),
        instances(Is),
        maplist(ascii_instance, Is).

ascii_instance(I-N) :-
        instance(I, N, V, C),
        maplist(timing_(swi, I, N), [sat,sats,taut], SWIs),
        maplist(timing_(sicstus, I, N), [sat,sats,taut], SICSs),
        append(SWIs, SICSs, Times0),
        maplist(time0_time, Times0, Times1),
        maplist(time_ascii, Times1, As),
        phrase(times_format(Times1), Cols),
        format("~w~w~15|~t~w~5+~t~w~5+", [I,N,V,C]),
        append("~30|", Cols, Format),
        format(Format, As),
        nl.

time_ascii(float(F), F).
time_ascii(integer(I), I).
time_ascii(exception, exception).
time_ascii(na, na).

times_format([]) --> [].
times_format([T|Ts]) -->
        time_format(T),
        times_format(Ts).

time_format(integer(_)) --> "~t~w~10+".
time_format(float(_))   --> "~t~2f~10+".
time_format(na)         --> "~tna~i~10+".
time_format(exception)  --> "~t--~i~10+".
