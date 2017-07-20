:- use_module(library(clpfd)).

jug_capacity(a, 8).
jug_capacity(b, 5).
jug_capacity(c, 3).

moves(Jugs) -->
        { memberchk(jug(a,4), Jugs),
          memberchk(jug(b,4), Jugs) }.
moves(Jugs0) -->
        { select(jug(AID,AF), Jugs0, Jugs1), AF #> 0,
          select(jug(BID,BF), Jugs1, Jugs2), jug_capacity(BID, BC), BF #< BC,
          Move #= min(AF,BC-BF),
          AF1 #= AF - Move,
          BF1 #= BF + Move },
        [from_to(AID,BID)],
        moves([jug(AID,AF1),jug(BID,BF1)|Jugs2]).

%?- length(Ms, _), phrase(moves([jug(a,8),jug(b,0),jug(c,0)]), Ms).
%@ Ms = [from_to(a,b),from_to(b,c),from_to(c,a),from_to(b,c),from_to(a,b),from_to(b,c),from_to(c,a)] .
