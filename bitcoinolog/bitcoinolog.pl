/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Bitcoinolog: Reason about Bitcoin addresses with Prolog.

   Written 2017-2020 by Markus Triska (triska@metalevel.at)

   For more information, visit:

                https://www.metalevel.at/bitcoinolog/
                =====================================

   Tested with Scryer Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(bitcoinolog, [new_private_key/1,
                        private_key_to_public_key/2,
                        private_key_to_wif/2,
                        public_key_to_address/2,
                        hex_to_base58check/2,
                        base58check_to_integer/2
                       ]).

:- use_module(library(clpz)).
:- use_module(library(crypto)).
:- use_module(library(dcgs)).
:- use_module(library(error)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(between)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sample use: Offline generation of Bitcoin addresses.

   ?- repeat,
          new_private_key(PrivateKey),
          private_key_to_public_key(PrivateKey, PublicKey),
          public_key_to_address(PublicKey, Address),
          format:portray_clause(Address),
          false.
   "142zbV8APtULexQai6NtrcSZxV9MoPwPPR".
   "13wh2Bhhc3B7M2k1ybWtSywBftrQ7Wm2wY".
   "1JxcrPifBFTUjZzYeFuPsWoaaZd5VR9yc".
   "12GDcpqLvgJiyPJRMqDFoZXvmx5LTn9uaK".

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bitcoin_curve(Curve) :-
        crypto_name_curve(secp256k1, Curve).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Generate a cryptographically secure random integer between 0 and 2^256.

   Almost all such integers can be used as *private keys* for Bitcoin.

   If an integer outside the suitable range is generated, an exception
   is thrown. The chances of this happening are extremely low.

   Sample use:

   ?- new_private_key(K).
   %@ K = 39615274996621223576805938914018474719127698794300917857701300837868839491832.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

new_private_key(Key) :-
        crypto_n_random_bytes(32, Bytes),
        hex_bytes(Hex, Bytes),
        hex_to_integer(Hex, Key),
        bitcoin_curve(Curve),
        crypto_curve_order(Curve, Order),
        Upper #= Order - 1,
        (   between(1, Upper, Key) -> true
        ;   domain_error(private_key, Key, new_private_key/1)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   A public key is a point on the curve, with coordinates (X,Y). In
   ECDSA, the public key can be derived from the private key by
   multiplying the generator with the private key.

   Deriving a public key and address is described in:

   https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses

   We deviate from this description in step (2) in that we store
   public keys in *compressed* form. This means that we use Prefix++X,
   where Prefix is either 0x02 or 0x03 depending on whether Y is even
   or odd, respectively.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

private_key_to_public_key(PrivateKey, PublicKey) :-
        bitcoin_curve(Curve),
        crypto_curve_generator(Curve, Generator),
        % 1. Compute the public key as PrivateKey*Generator.
        crypto_curve_scalar_mult(Curve, PrivateKey, Generator, point(X,Y)),
        % 2. PublicKey in compressed form.
        Rem #= Y mod 2,
        zcompare(Cmp, 0, Rem),
        cmp0_prefix(Cmp, Prefix),
        phrase(format_("~w~|~`0t~16r~64+", [Prefix,X]), PublicKey).

cmp0_prefix(=, '02').
cmp0_prefix(<, '03').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Convert a private key to Wallet Import Format (WIF).

   This is how private keys are typically shown in wallets.

   The prefix 80 is used for private keys. 01 is appended to indicate
   that these private keys correspond to *compressed* public keys.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

private_key_to_wif(PrivateKey0, WIF) :-
        phrase(format_("80~|~`0t~16r~64+01", [PrivateKey0]), PrivateKey),
        % Twice perform SHA-256 on the hex encoding of the private key.
        hex_algorithm_hash(PrivateKey, sha256, HashPrivateKey1),
        hex_algorithm_hash(HashPrivateKey1, sha256, HashPrivateKey2),
        % Take the first four bytes, which are the checksum.
        hex_bytes(HashPrivateKey2, Bytes),
        Bytes = [B1,B2,B3,B4|_],
        hex_bytes(PrivateKey, PrivateKeyBytes),
        append(PrivateKeyBytes, [B1,B2,B3,B4], WIF0),
        % Convert the address to Base58Check encoding.
        hex_bytes(WIF1, WIF0),
        hex_to_base58check(WIF1, WIF).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Convert a public key to a Bitcoin address in Base58Check encoding.
   This encoding is commonly shown for Bitcoin addresses.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

public_key_to_address(PublicKey, Address) :-
        % 1. Compute public key from private key: already done!
        % 2. Perform SHA-256 on the hex encoding of the public key.
        hex_algorithm_hash(PublicKey, sha256, HashPublicKey),
        % 3. Perform RIPEMD-160 hashing on the result.
        hex_algorithm_hash(HashPublicKey, ripemd160, RIPEMD160),
        % 4. Add version byte in front of RIPEMD-160.
        %    0x00 denotes Main Network.
        append("00", RIPEMD160, ExtendedRIPE),
        % 5. Perform SHA-256 on the extended RIPEMD-160 result.
        hex_algorithm_hash(ExtendedRIPE, sha256, SHA256_1),
        % 6. Perform SHA-256 on the previous result.
        hex_algorithm_hash(SHA256_1, sha256, SHA256_2),
        % 7. Take the first four bytes, which are the checksum.
        hex_bytes(SHA256_2, Bytes),
        Bytes = [B1,B2,B3,B4|_],
        % 8. Add the 4 checksum bytes at the end of extended RIPEMD-160.
        %    This is the 25-byte binary Bitcoin Address.
        hex_bytes(ExtendedRIPE, ExtendedBytes),
        append(ExtendedBytes, [B1,B2,B3,B4], Address0),
        % 9. Convert the address to Base58Check encoding.
        hex_bytes(Address1, Address0),
        hex_to_base58check(Address1, Address).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Compute the hash of a hex code, using Algorithm.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

hex_algorithm_hash(Hex, Algorithm, Hash) :-
        hex_bytes(Hex, Bytes),
        crypto_data_hash(Bytes, Hash, [algorithm(Algorithm),
                                       encoding(octet)]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Base58Check format.

   The most important conversion predicate is hex_to_base58check/2.
   The predicate base58check_to_integer/2 can be useful too.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

base58_alphabet("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz").

hex_to_base58check(Hex, Base58Check) :-
        hex_to_integer(Hex, Integer),
        phrase(base58check(Integer), Bs0),
        reverse(Bs0, Bs),
        hex_bytes(Hex, Bytes),
        once(phrase(leading_zeroes(Bytes), Base58Check, Bs)).

leading_zeroes([0|Rest]) --> ['1'], leading_zeroes(Rest).
leading_zeroes([D|_]) --> { D #\= 0 }.

base58check(I0) -->
        { zcompare(C, 0, I0) },
        base58check_(C, I0).

base58check_(=, _) --> [].
base58check_(<, I0) --> [Char],
        { I #= I0 // 58,
          Remainder #= I0 mod 58,
          base58_alphabet(As),
          nth0(Remainder, As, Char) },
        base58check(I).

base58check_to_integer(B58, Integer) :-
        reverse(B58, Chars),
        foldl(pow58, Chars, 0-0, Integer-_).

pow58(Char, N0-I0, N-I) :-
        base58_alphabet(As),
        nth0(Value, As, Char),
        N #= N0 + Value*58^I0,
        I #= I0 + 1.

hex_to_integer(Hex, Integer) :-
        hex_bytes(Hex, Bytes0),
        reverse(Bytes0, Bytes),
        foldl(pow256, Bytes, 0-0, Integer-_).

pow256(Byte, N0-I0, N-I) :-
        N #= N0 + Byte*256^I0,
        I #= I0 + 1.
