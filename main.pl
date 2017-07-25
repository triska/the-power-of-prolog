/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The Power of Prolog
   ===================

   Copyright (C) 2005-2017 Markus Triska triska@metalevel.at

   Online version:

   https://www.metalevel.at/prolog
   ===============================

   Example usage to spawn a server on port 5050:

       $ swipl main.pl --port=5050 --interactive

   then direct your browser to:

   http://localhost:5050/prolog

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(main,
	  []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_log)).
:- use_module(library(http/mimetype)).
:- use_module(library(settings)).
:- use_module(library(http/http_unix_daemon)).

:- set_prolog_flag(default_mimetype, text/plain).

:- http_handler(/, handle_request, [prefix]).
:- http_handler('/prolog', http_reply_file('prolog/prolog.html', []), []).


handle_request(Request) :-
        memberchk(path(Path0), Request),
        atom_concat(., Path0, Path1),
        http_safe_file(Path1, []),
        absolute_file_name(Path1, Path),
        (   path_path0_file(Path, Path0, File),
            exists_file(File),
            http_reply_file(File, [unsafe(true)], Request)
        ;   atom_concat('https://www.metalevel.at', Path0, Metalevel),
            http_redirect(see_other, Metalevel, Request)
        ).

path_path0_file(Path, _, Path).
path_path0_file(Path, _, HTML) :- atom_concat(Path, '.html', HTML).
path_path0_file(Path, Path0, File) :-
        path_segments_atom(Segments, Path0),
        Segments = _/Last/'',
        atomic_list_concat([Path,Last,'.html'], File).
