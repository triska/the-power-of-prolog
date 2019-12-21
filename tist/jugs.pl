:- use_module(library(clpfd)).

jug_capacity(a, 8).
jug_capacity(b, 5).
jug_capacity(c, 3).

moves(Jugs) -->
        { member(jug(a,4), Jugs),
          member(jug(b,4), Jugs) }.
moves(Jugs0) --> [from_to(From,To)],
        { select(jug(From,FromFill0), Jugs0, Jugs1),
          FromFill0 #> 0,
          select(jug(To,ToFill0), Jugs1, Jugs),
          jug_capacity(To, ToCapacity),
          ToFill0 #< ToCapacity,
          Move #= min(FromFill0, ToCapacity-ToFill0),
          FromFill #= FromFill0 - Move,
          ToFill #= ToFill0 + Move },
        moves([jug(From,FromFill),jug(To,ToFill)|Jugs]).

%?- length(Ms, _), phrase(moves([jug(a,8),jug(b,0),jug(c,0)]), Ms).
%@ Ms = [from_to(a,b),from_to(b,c),from_to(c,a),from_to(b,c),from_to(a,b),from_to(b,c),from_to(c,a)] .
