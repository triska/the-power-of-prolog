/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ECClog: Elliptic Curve Cryptography with Prolog.

   Written June 2017 by Markus Triska (triska@metalevel.at)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(ecclog, [ecc_name_curve/2,
                   ecc_curve_generator/2,
                   ecc_curve_order/2,
                   ecc_curve_scalar_mult/4]).

:- use_module(library(clpfd)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sample use: Encryption and decryption of shared secret S.

    ?- ecc_name_curve(Name, C),
       ecc_curve_generator(C, Generator),
       PrivateKey = 10,
       ecc_curve_scalar_mult(C, PrivateKey, Generator, PublicKey),
       Random = 12,
       ecc_curve_scalar_mult(C, Random, Generator, R),
       ecc_curve_scalar_mult(C, Random, PublicKey, S),
       ecc_curve_scalar_mult(C, PrivateKey, R, S).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Modular multiplicative inverse.

   Compute Y = X^(-1) mod p, using the extended Euclidean algorithm.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

multiplicative_inverse_modulo_p(X, P, Y) :-
        eea(X, P, _, _, Y),
        R #= X*Y mod P,
        zcompare(C, 1, R),
        must_be_one(C, X, P, Y).

must_be_one(=, _, _, _).
must_be_one(>, X, P, Y) :- throw(multiplicative_inverse_modulo_p(X,P,Y)).
must_be_one(<, X, P, Y) :- throw(multiplicative_inverse_modulo_p(X,P,Y)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Extended Euclidean algorithm.

   Computes the GCD and the Bézout coefficients S and T.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

eea(I, J, G, S, T) :-
        State0 = state(1,0,0,1),
        eea_loop(I, J, State0, G, S, T).

eea_loop(I, J, State0, G, S, T) :-
        zcompare(C, 0, J),
        eea_(C, I, J, State0, G, S, T).

eea_(=, I, _, state(_,_,U,V), I, U, V).
eea_(<, I0, J0, state(S0,T0,U0,V0), I, U, V) :-
        Q #= I0 // J0,
        R #= I0 mod J0,
        S1 #= U0 - (Q*S0),
        T1 #= V0 - (Q*T0),
        eea_loop(J0, R, state(S1,T1,S0,T0), I, U, V).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Operations on Elliptic Curves
   =============================
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   An elliptic curve over a prime field F_p is represented as:

   curve(P,A,B,point(X,Y),Order,Cofactor).

   First, we define suitable accessors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

curve_p(curve(P,_,_,_,_,_), P).
curve_a(curve(_,A,_,_,_,_), A).
curve_b(curve(_,_,B,_,_,_), B).

ecc_curve_order(curve(_,_,_,_,Order,_), Order).
ecc_curve_generator(curve(_,_,_,G,_,_), G).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Scalar point multiplication.

   R = k*Q.

   The Montgomery ladder method is used to mitigate side-channel
   attacks such as timing attacks, since the number of multiplications
   and additions is independent of the private key K. This method does
   not even reveal the key's Hamming weight (number of 1s).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ecc_curve_scalar_mult(Curve, K, Q, R) :-
        Upper #= msb(K),
        scalar_multiplication(Curve, K, Upper, ml(null,Q)-R),
        must_be_on_curve(Curve, R).

scalar_multiplication(Curve, K, I, R0-R) :-
        zcompare(C, -1, I),
        scalar_mult_(C, Curve, K, I, R0-R).

scalar_mult_(=, _, _, _, ml(R,_)-R).
scalar_mult_(<, Curve, K, I0, ML0-R) :-
        BitSet #= K /\ (1 << I0),
        zcompare(C, 0, BitSet),
        montgomery_step(C, Curve, ML0, ML1),
        I1 #= I0 - 1,
        scalar_multiplication(Curve, K, I1, ML1-R).

montgomery_step(=, Curve, ml(R0,S0), ml(R,S)) :-
        curve_points_addition(Curve, R0, S0, S),
        curve_point_double(Curve, R0, R).
montgomery_step(<, Curve, ml(R0,S0), ml(R,S)) :-
        curve_points_addition(Curve, R0, S0, R),
        curve_point_double(Curve, S0, S).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Doubling a point: R = A + A.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

curve_point_double(_, null, null).
curve_point_double(Curve, point(AX,AY), R) :-
        curve_p(Curve, P),
        curve_a(Curve, A),
        Numerator #= (3*AX^2 + A) mod P,
        Denom0 #= 2*AY mod P,
        multiplicative_inverse_modulo_p(Denom0, P, Denom),
        S #= (Numerator*Denom) mod P,
        R = point(RX,RY),
        RX #= (S^2 - 2*AX) mod P,
        RY #= (S*(AX - RX) - AY) mod P,
        must_be_on_curve(Curve, R).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Adding two points.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

curve_points_addition(Curve, P, Q, R) :-
        curve_points_addition_(P, Curve, Q, R).

curve_points_addition_(null, _, P, P).
curve_points_addition_(P, _, null, P).
curve_points_addition_(point(AX,AY), Curve, point(BX,BY), R) :-
        curve_p(Curve, P),
        Numerator #= (AY - BY) mod P,
        Denom0 #= (AX - BX) mod P,
        multiplicative_inverse_modulo_p(Denom0, P, Denom),
        S #= (Numerator * Denom) mod P,
        R = point(RX,RY),
        RX #= (S^2 - AX - BX) mod P,
        RY #= (S*(AX - RX) - AY) mod P,
        must_be_on_curve(Curve, R).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Validation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

curve_contains_point(Curve, point(QX,QY)) :-
        curve_a(Curve, A),
        curve_b(Curve, B),
        curve_p(Curve, P),
        QY^2 mod P #= (QX^3 + A*QX + B) mod P.

must_be_on_curve(Curve, P) :-
        \+ curve_contains_point(Curve, P),
        throw(not_on_curve(P)).
must_be_on_curve(Curve, P) :- curve_contains_point(Curve, P).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Predefined curves
   =================

   List available curves:

   $ openssl ecparam -list_curves

   Show curve parameters for secp256k1:

   $ openssl ecparam -param_enc explicit -conv_form uncompressed \
                     -text -no_seed -name secp256k1

   You must remove the leading "04:" from the generator.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ecc_name_curve(secp112r1,
               curve(0x00db7c2abf62e35e668076bead208b,
                     0x00db7c2abf62e35e668076bead2088,
                     0x659ef8ba043916eede8911702b22,
                     point(0x09487239995a5ee76b55f9c2f098,
                           0xa89ce5af8724c0a23e0e0ff77500),
                     0x00db7c2abf62e35e7628dfac6561c5,
                     1)).
ecc_name_curve(secp256k1,
               curve(0x00fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f,
                     0x0,
                     0x7,
                     point(0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798,
                           0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8),
                     0x00fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141,
                     1)).
