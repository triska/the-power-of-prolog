/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpreter and compiler for a simple imperative language.

   Written May 2006 by Markus Triska (triska@metalevel.at)
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(pio)).

:- initialization(set_prolog_flag(double_quotes, codes)).

% interpreter

run(AST) :-
        env_new(Env),
        interpret(AST, Env, _).


interpret(print(P), Env, Env) :-
        eval(P, Env, Value),
        format("~w\n", [Value]).
interpret(sequence(A,B), Env0, Env) :-
        interpret(A, Env0, Env1),
        (   A = return(_) ->
            Env = Env1
        ;   interpret(B, Env1, Env)
        ).
interpret(call(Name, Arg), Env0, Env0) :-
        eval(Arg, Env0, ArgVal),
        env_func_body(Env0, Name, ArgName, Body),
        env_clear_variables(Env0, Env1),
        env_put_var(ArgName, ArgVal, Env1, Env2),
        interpret(Body, Env2, _).
interpret(function(Name,Arg,Body), Env0, Env) :-
        env_put_func(Name, Arg, Body, Env0, Env).
interpret(if(Cond,Then,Else), Env0, Env) :-
        eval(Cond, Env0, Value),
        (   Value #\= 0 ->
            interpret(Then, Env0, Env)
        ;   interpret(Else, Env0, Env)
        ).
interpret(assign(Var, Expr), Env0, Env) :-
        eval(Expr, Env0, Value),
        env_put_var(Var, Value, Env0, Env).
interpret(while(Cond, Body), Env0, Env) :-
        eval(Cond, Env0, Value),
        (   Value #\= 0 ->
            interpret(Body, Env0, Env1),
            interpret(while(Cond, Body), Env1, Env)
        ;   Env = Env0
        ).
interpret(return(Expr), Env0, Value) :-
        eval(Expr, Env0, Value).
interpret(nop, Env, Env).


eval(bin(Op,A,B), Env, Value) :-
        eval(A, Env, VA),
        eval(B, Env, VB),
        eval_(Op, VA, VB, Value).
eval(v(V), Env, Value) :-
        env_get_var(Env, V, Value).
eval(n(N), _, N).
eval(call(Name, Arg), Env0, Value) :-
        eval(Arg, Env0, ArgVal),
        env_func_body(Env0, Name, ArgName, Body),
        env_clear_variables(Env0, Env1),
        env_put_var(ArgName, ArgVal, Env1, Env2),
        interpret(Body, Env2, Value).


eval_(+, A, B, V) :- V #= A + B.
eval_(-, A, B, V) :- V #= A - B.
eval_(*, A, B, V) :- V #= A * B.
eval_(/, A, B, V) :- V #= A // B.
eval_(=, A, B, V) :- goal_truth(A #= B, V).
eval_(>, A, B, V) :- goal_truth(A #> B, V).
eval_(<, A, B, V) :- goal_truth(A #< B, V).

goal_truth(Goal, V) :- ( Goal -> V = 1 ; V = 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% access and modify the environment

env_new(E-E) :- empty_assoc(E).

env_put_func(Name, Arg, Body, Vars0-Funcs0, Vars0-Funcs) :-
        put_assoc(Name, Funcs0, Arg-Body, Funcs).

env_func_body(_-Funcs, Name, ArgName, Body) :-
        get_assoc(Name, Funcs, ArgName-Body).

env_put_var(Name, Value, Vars0-Funcs0, Vars-Funcs0) :-
        put_assoc(Name, Vars0, Value, Vars).

env_get_var(Vars-_, Name, Value) :- get_assoc(Name, Vars, Value).

env_clear_variables(_-Funcs0, E-Funcs0) :- empty_assoc(E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compile AST to virtual machine instructions VMs

ast_vminstrs(AST, VMs) :-
        initial_state(S0),
        phrase(compilation(AST), [S0], [S]),
        state_vminstrs(S, VMs).

initial_state(s([],[],[],0)).

state_vminstrs(s(Is0,Fs,_,_), Is) :-
        reverse([halt|Is0], Is1),
        maplist(resolve_calls(Fs), Is1, Is).

resolve_calls(Fs, I0, I) :-
        (   I0 = call(Name) ->
            memberchk(Name-Adr, Fs),
            I = call(Adr)
        ;   I = I0
        ).

state(S), [S] --> [S].

state(S0, S), [S] --> [S0].


current_pc(PC) --> state(s(_,_,_,PC)).

vminstr(I) -->
        state(s(Is,Fs,Vs,PC0), s([I|Is],Fs,Vs,PC)),
        { I =.. Ls,
          length(Ls, L),   % length of instruction including arguments
          PC #= PC0 + L }.

start_function(Name, Arg) -->
        state(s(Is,Fs,_,PC), s(Is,[Name-PC|Fs],[Arg-0],PC)).

num_variables(Num) -->
        state(s(_,_,Vs,_)),
        { length(Vs, Num0),
          Num #= Num0 - 1 }.      % don't count parameter

variable_offset(Name, Offset) -->
        state(s(Is,Fs,Vs0,PC), s(Is,Fs,Vs,PC)),
        { (   memberchk(Name-Offset, Vs0) ->
              Vs = Vs0
          ;   Vs0 = [_-Curr|_],
              Offset #= Curr + 1,
              Vs = [Name-Offset|Vs0]
          ) }.

compilation(nop) --> [].
compilation(print(P)) -->
        compilation(P),
        vminstr(print).
compilation(sequence(A,B)) -->
        compilation(A),
        compilation(B).
compilation(call(Name,Arg)) -->
        compilation(Arg),
        vminstr(call(Name)).
compilation(function(Name,Arg,Body)) -->
        vminstr(jmp(Skip)),
        start_function(Name, Arg),
        vminstr(alloc(NumVars)),
        compilation(Body),
        num_variables(NumVars),
        current_pc(Skip).
compilation(if(Cond,Then,Else)) -->
        { Cond = bin(Op,A,B) },
        compilation(A),
        compilation(B),
        condition(Op, Adr1),
        compilation(Then),
        vminstr(jmp(Adr2)),
        current_pc(Adr1),
        compilation(Else),
        current_pc(Adr2).
compilation(assign(Var,Expr)) -->
        variable_offset(Var, Offset),
        compilation(Expr),
        vminstr(pop(Offset)).
compilation(while(Cond,Body)) -->
        current_pc(Head),
        { Cond = bin(Op,A,B) },
        compilation(A),
        compilation(B),
        condition(Op, Break),
        compilation(Body),
        vminstr(jmp(Head)),
        current_pc(Break).
compilation(return(Expr)) -->
        compilation(Expr),
        vminstr(ret).
compilation(bin(Op,A,B)) -->
        compilation(A),
        compilation(B),
        { op_vminstr(Op, VI) },
        vminstr(VI).
compilation(n(N)) -->
        vminstr(pushc(N)).
compilation(v(V)) -->
        variable_offset(V, Offset),
        vminstr(pushv(Offset)).


op_vminstr(+, add).
op_vminstr(-, sub).
op_vminstr(*, mul).
op_vminstr(/, div).

condition(=, Adr) --> vminstr(jne(Adr)).
condition(<, Adr) --> vminstr(jge(Adr)).
condition(>, Adr) --> vminstr(jle(Adr)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% map virtual machine instructions to a list of integer codes

vminstrs_ints([])     --> [].
vminstrs_ints([I|Is]) -->
        vminstr_ints(I),
        vminstrs_ints(Is).

vminstr_ints(halt)      --> [0].
vminstr_ints(alloc(A))  --> [1,A].
vminstr_ints(pushc(C))  --> [2,C].
vminstr_ints(pushv(V))  --> [3,V].
vminstr_ints(pop(V))    --> [4,V].
vminstr_ints(add)       --> [5].
vminstr_ints(sub)       --> [6].
vminstr_ints(mul)       --> [7].
vminstr_ints(div)       --> [8].
vminstr_ints(jmp(Adr))  --> [9,Adr].
vminstr_ints(jne(Adr))  --> [10,Adr].
vminstr_ints(jge(Adr))  --> [11,Adr].
vminstr_ints(jle(Adr))  --> [12,Adr].
vminstr_ints(call(Adr)) --> [13,Adr].
vminstr_ints(print)     --> [14].
vminstr_ints(ret)       --> [15].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% lexical analysis - split input sequence into tokens

tokens(Ts) -->
        whitespace,
        tokens(Ts).
tokens([T|Ts]) -->
        tok(T),
        !, % single solution: longest input match
        tokens(Ts).
tokens([]) --> "".


tok('{') --> "{".
tok('}') --> "}".
tok(';') --> ";".
tok(',') --> ",".
tok('(') --> "(".
tok(')') --> ")".

tok(rop(=)) --> "==".
tok(rop(<)) --> "<".
tok(rop(>)) --> ">".

tok(aop(+)) --> "+".
tok(aop(-)) --> "-".

tok(mop(*)) --> "*".
tok(mop(/)) --> "/".
tok(=) --> "=".

tok(ID_or_KW) -->
        ident(Cs),
        { name(I, Cs), ( keyword(I) -> ID_or_KW = I ; ID_or_KW = id(I) ) }.
tok(num(N)) --> number(Cs), { name(N, Cs) }.

ident([C|Cs]) --> letter(C), identr(Cs).

identr([C|Cs]) --> letter(C), identr(Cs).
identr([C|Cs]) --> digit(C), identr(Cs).
identr([])     --> [].

number([C|Cs]) --> digit(C), number(Cs).
number([C])    --> digit(C).

letter(C)  --> [C], { between(0'A, 0'Z, C) ; between(0'a, 0'z, C)}.
digit(C)   --> [C], { between(0'0, 0'9, C) }.
whitespace --> [C], {C =< 0' }. % close ' for syntax highlighting


keyword(K) :- memberchk(K, [if,else,while,return,print]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% syntax analysis - generate abstract syntax tree (AST) from tokens

tokens_ast(Tokens, AST) :-
        phrase(program(AST), Tokens).

program(nop) --> [].
program(P)   --> func_or_print(FP), program_r(FP, P).

program_r(P, P)                 --> [].
program_r(P0, sequence(P0, P1)) --> func_or_print(FP), program_r(FP, P1).

func_or_print(F) --> func(F).
func_or_print(print(P)) --> stm(print(P)).

func(function(Name,Arg,Body)) -->
    [id(Name)], ['('], [id(Arg)], [')'], block_(Body).

stms(S)   --> stm(S1), stmr(S1, S).
stms(nop) --> [].

stmr(S1, sequence(S1, S)) --> stm(S2), stmr(S2, S).
stmr(S, S)                --> [].

stm(call(Name, Arg)) --> [id(Name)], ['('], exp(Arg), [')'], [';'].
stm(assign(Id, E))   --> [id(Id)], ['='], exp(E), [';'].
stm(if(Cond,S1,S2))  --> [if], cond(Cond), stm(S1), [else], stm(S2).
stm(while(Cond, S))  --> [while], cond(Cond), stm(S).
stm(return(E))       --> [return], exp(E), [';'].
stm(print(E))        --> [print], exp(E), [';'].
stm(S)               --> block_(S).
stm(nop)             --> [';'].

block_(S) --> ['{'], stms(S), ['}'].

cond(bin(Op,A,B)) --> ['('], exp(A), [rop(Op)], exp(B), [')'].

exp(E)      --> term(E1), expr(E1, E).
expr(E1, E) --> [aop(Op)], term(E2), expr(bin(Op, E1, E2), E).
expr(E, E)  --> [].

term(E)      --> factor(E1), termr(E1, E).
termr(E1, E) --> [mop(Op)], factor(E2), termr(bin(Op, E1, E2), E).
termr(E, E)  --> [].

factor(n(N))            --> [num(N)].
factor(v(Id))           --> [id(Id)].
factor(call(Name, Arg)) --> [id(Name)], ['('], exp(Arg), [')'].
factor(E)               --> ['('], exp(E), [')'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% AST type definition

is_program(nop).
is_program(sequence(A,B)) :-
        ( (A = print(E), is_exp(E)) ; is_function(A) ),
        is_program(B).

is_function(function(Name,Arg,Body)) :-
        atom(Name),
        atom(Arg),
        is_stm(Body).

is_stm(print(E)) :-
        is_exp(E).
is_stm(sequence(S1,S2)) :-
        is_stm(S1),
        is_stm(S2).
is_stm(call(Name, Arg)) :-
        atom(Name),
        is_exp(Arg).
is_stm(if(Cond,Then,Else)) :-
        is_exp(Cond),
        is_stm(Then),
        is_stm(Else).
is_stm(while(Cond,Body)) :-
        is_exp(Cond),
        is_stm(Body).
is_stm(return(E)) :-
        is_exp(E).
is_stm(nop).
is_stm(assign(Id, E)) :-
        atom(Id),
        is_exp(E).


is_exp(n(N)) :-
        number(N).
is_exp(v(V)) :-
        atom(V).
is_exp(call(Id, E)) :-
        atom(Id),
        is_exp(E).
is_exp(bin(Op,E1,E2)) :-
        member(Op, [=,#,>,<,+,-,*,/]),
        is_exp(E1),
        is_exp(E2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

string_ast(String, AST) :-
        phrase(tokens(Tokens), String),
        tokens_ast(Tokens, AST).

run_file(File) :-
        (   phrase_from_file(tokens(Tokens), File) ->
            format("\n\ntokens:\n\n~w\n", [Tokens]),
            (   tokens_ast(Tokens, AST) ->
                % is_program(AST), % type check
                format("\nAST:\n\n~w\n", [AST]),
                ast_vminstrs(AST, VMs),
                format("\n\nVM code:\n\n"),
                foldl(display_vminstr, VMs, 0, _),
                phrase(vminstrs_ints(VMs), Ints),
                format("\nintcode:\n\n~w\n\n", [Ints]),
                format("program output:\n\n"),
                run(AST),
                halt
            ;   format("syntax error\n")
            )
        ;   format("lexical error")
        ).


display_vminstr(Cmd, N0, N1) :-
        format("~t~w~5|:   ", [N0]),
        Cmd =.. Ls,
        length(Ls, L),
        (   L = 1 ->
            format("~w\n", Ls)
        ;   format("~w ~w\n", Ls)
        ),
        N1 #= N0 + L.
