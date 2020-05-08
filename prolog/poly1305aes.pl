/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Prolog implementation of the Poly1305 message-authentication code
   that was introduced by Daniel J. Bernstein in his 2005 paper,
   "The Poly1305-AES message-authentication code".

   Written Sept. 2017 by Markus Triska (triska@metalevel.at)
   Public domain code. Tested with Scryer Prolog.

   More information about cryptography with Prolog is available from:

             https://www.metalevel.at/prolog/cryptography
             ============================================

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpz)).
:- use_module(library(lists)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The relation between a message Ms, a key Rs, the output Ss of a keyed
   function F that maps a nonce to a 16-byte string, and Ps which is
   the authenticator Poly1305_Rs(Ms, Ss) in little-endian representation.
   Each argument is a list of bytes, i.e., integers between 0 and 255.

   The authenticator can be used with any suitable function F. To obtain
   Poly1305-AES, see below for test cases that use it with AES.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

message_r_s_poly1305(Ms, Rs, Ss, Ps) :-
        length(Ms, L),
        Q0 #= L // 16,
        length(Qs, Q0),
        foldl(chunk, Qs, Ms-[], Rest-Cs0),
        rest_chunks(Rest, Cs0, Cs),
        P #= 2^130 - 5,
        little_endian_integer(Rs, R),
        foldl(poly1305(P, R), Cs, 0, Poly),
        little_endian_integer(Ss, S),
        Sum #= (Poly + S) mod 2^128,
        length(Ps, 16),
        once(little_endian_integer(Ps, Sum)).

chunk(_, Ls0-Cs0, Ls-[Chunk|Cs0]) :-
        length(Chunk, 16),
        append(Chunk, Ls, Ls0).

poly1305(P, R, Chunk0, Poly0, Poly) :-
        append(Chunk0, [1], Chunk),
        little_endian_integer(Chunk, C),
        Poly #= (Poly0 + C)*R mod P.

rest_chunks([], Cs0, Cs)     :- reverse(Cs0, Cs).
rest_chunks([R|Rs], Cs0, Cs) :- reverse([[R|Rs]|Cs0], Cs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Relation between an integer and its little-endian representation.
   Declarative integer arithmetic is used to make this relation usable
   in all directions. For more information about this approach, see:

                https://www.metalevel.at/prolog/clpz
                ====================================

   Examples:

       ?- little_endian_integer([0,1], I).
          I = 256
       ;  false.

       ?- little_endian_integer([1,2,3], I).
          I = 197121
       ;  false.

       ?- little_endian_integer(Ls, 300).
          Ls = [44,1]
       ;  Ls = [44,1,0]
       ;  Ls = [44,1,0,0]
       ;  Ls = [44,1,0,0,0]
       ;  Ls = [44,1,0,0,0,0]
       ;  ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

little_endian_integer(Ls, I) :-
        foldl(little_endian_, Ls, 0-0, _-I).

little_endian_(E, P0-I0, P-I) :-
        E in 0..255,
        I #= I0 + E*256^P0,
        P #= P0 + 1.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Test cases from Daniel J. Bernstein's paper, Appendix B. In these
   examples, the keyed function F is AES_k(n), where n is the nonce.

   Examples:

       ?- use_module(library(time)).
          true.

       ?- time(verification(1)).
       verifying(example-1).
       verified(poly1305).
          % CPU time: 0.298 seconds
          true

       ?- verification(_).
       verifying(example-1).
       verified(poly1305).
          true
       ;  verifying(example-2).
       verified(poly1305).
       true
       ;  ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(crypto)).
:- use_module(library(format)).

verification(N) :-
        example_parameters(N, Params),
        portray_clause(verifying(example-N)),
        maplist(parameter_bytes(Params), [m,r,k,n,aes,poly1305],
                                         [Ms,Rs,_,_,As,Ps]),
        length(IVs, 16), maplist(=(0), IVs),
        message_r_s_poly1305(Ms, Rs, As, Ps1),
        must_be_equal(poly1305, Ps, Ps1).

must_be_equal(Param, A, B) :-
        compare(Comp, A, B),
        equal_(Comp, Param, A, B),
        portray_clause(verified(Param)).

equal_(=, _, _, _).
equal_(<, Param, A, B) :- throw(param_different(Param, A, B)).
equal_(>, Param, A, B) :- throw(param_different(Param, A, B)).

parameter_bytes(Params, Param, Bs) :-
        memberchk(Param=P, Params),
        hex_bytes(P, Bs).

example_parameters(1, [m = "f3f6",
                       r = "851fc40c3467ac0be05cc20404f3f700",
                       k = "ec074c835580741701425b623235add6",
                       n = "fb447350c4e868c52ac3275cf9d4327e",
                       aes = "580b3b0f9447bb1e69d095b5928b6dbc",
                       poly1305 = "f4c633c3044fc145f84f335cb81953de"]).
example_parameters(2, [m = "",
                       r = "a0f3080000f46400d0c7e9076c834403",
                       k = "75deaa25c09f208e1dc4ce6b5cad3fbf",
                       n = "61ee09218d29b0aaed7e154a2c5509cc",
                       aes = "dd3fab2251f11ac759f0887129cc2ee7",
                       poly1305 = "dd3fab2251f11ac759f0887129cc2ee7"]).
example_parameters(3, [m = "663cea190ffb83d89593f3f476b6bc24d7e679107ea26adb8caf6652d0656136",
                       r = "48443d0bb0d21109c89a100b5ce2c208",
                       k = "6acb5f61a7176dd320c5c1eb2edcdc74",
                       n = "ae212a55399729595dea458bc621ff0e",
                       aes = "83149c69b561dd88298a1798b10716ef",
                       poly1305 = "0ee1c16bb73f0f4fd19881753c01cdbe"]).
example_parameters(4, [m = "ab0812724a7f1e342742cbed374d94d136c6b8795d45b3819830f2c04491faf0990c62e48b8018b2c3e4a0fa3134cb67fa83e158c994d961c4cb21095c1bf9",
                       r = "12976a08c4426d0ce8a82407c4f48207",
                       k = "e1a5668a4d5b66a5f68cc5424ed5982d",
                       n = "9ae831e743978d3a23527c7128149e3a",
                       aes = "80f8c20aa71202d1e29179cbcb555a57",
                       poly1305 ="5154ad0d2cb26e01274fc51148491f1b"]).
