:- module(pio,
   [ phrase_from_file/2,      % :Grammarbody, +File
     phrase_of_from_file/3    % :Grammarbody, :Reader(Stream, Xs0,Xs), +File
   ]).

/** <module> Pure, phrase based, Prolog I/O

This library provides side effect free I/O.  Files can be
processed in a pure side effect free manner with DCGs.  Many Prolog
predicates only read data from a file, those programs can be made side
effect free.

Recommended settings:
:- set_prolog_flag(double_quotes, chars).

@author Ulrich Neumerkel
*/

%% phrase_from_file(Grammar__0, +File) is nondet.
%
%  ==
%  ... --> [] | [_], ... .
%  ?- phrase_from_file((..., "searchstring", ...), myfile).

:- meta_predicate
      phrase_from_file(2, +),
      phrase_of_from_file(2, 3, +).

:- meta_predicate
      reader_to_lazy_list(3,+,?),
      reader_step(3,+,+,?).


:- use_module(library(error),[must_be/2]).

must_be(Value, Type, _Goal, _Arg) :-
   must_be(Type, Value).

phrase_from_file(NT__0, File) :-
   current_prolog_flag(double_quotes, Value),
   (  Value == chars -> R_3 = get_pending_chars % recommended
   ;  Value == codes -> R_3 = get_pending_codes % suboptimal
   ;  must_be(Value, oneof([chars,codes]), phrase_from_file(NT__0, File), 0)
   ),
   phrase_of_from_file(NT__0, R_3, File).

phrase_of_from_file(NT__0, R_3, File) :-
   setup_call_cleanup(
      open(File, read, Stream, [reposition(true)]),
      ( reader_to_lazy_list(R_3, Stream, Xs), phrase(NT__0, Xs) ),
      close(Stream)
   ).

reader_to_lazy_list(R_3, Stream, Xs) :-
   stream_property(Stream, position(Pos)),
   freeze(Xs, reader_step(R_3, Stream, Pos, Xs)).

reader_step(R_3, Stream, Pos, Xs0) :-
   set_stream_position(Stream, Pos),
   (   at_end_of_stream(Stream)
   ->  Xs0 = []
   ;   % phrase(call(call(R_3,Stream)), Xs0,Xs), % conforming call
       call(R_3, Stream, Xs0,Xs), % effective call
       reader_to_lazy_list(R_3, Stream, Xs)
   ).

get_pending_chars(Stream, Chs0,Chs) :-
   read_pending_chars(Stream, Chs0,Chs),
	( at_end_of_stream(Stream) -> Chs = []	; true ).

% Ideally, at_end_of_stream/1 is not used above, and Chs = []
% is only set when EOF has been reached while reading

get_pending_codes(Stream, Cos0,Cos) :-
   read_pending_codes(Stream, Cos0,Cos),
	( at_end_of_stream(Stream) -> Cos = []	; true ).

% In newer versions, below is defined as built-ins already.

:- if(\+current_predicate(read_pending_chars/3)).
read_pending_chars(Stream, Chs0,Chs) :-
   read_pending_input(Stream, Codes,[]),
	atom_codes(Atom, Codes),
	atom_chars(Atom, Chars),
	append(Chars, Chs,Chs0).
:- endif.

:- if(\+current_predicate(read_pending_codes/3)).
read_pending_codes(Stream, Cos0,Cos) :-
   read_pending_input(Stream, Cos0,Cos).
:- endif.

