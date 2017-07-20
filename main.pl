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
:- use_module(library(http/html_write)).
:- use_module(library(http/mimetype)).
:- use_module(library(settings)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).

:- set_prolog_flag(default_mimetype, text/plain).

:- http_handler(/, handle_request, [prefix]).
:- http_handler('/prolog', http_reply_file('prolog/prolog.html', []), []).


handle_request(Request) :-
        memberchk(path(Path0), Request),
        atom_concat(., Path0, Path1),
        http_safe_file(Path1, []),
        absolute_file_name(Path1, Path),
        (   exists_file(Path),
            http_reply_file(Path, [unsafe(true)], Request)
        ;   atom_concat(Path, '.html', HTML),
            exists_file(HTML),
            http_reply_file(HTML, [unsafe(true)], Request)
        ;   path_segments_atom(Segments, Path0),
            Segments = _/Last/'',
            atomic_list_concat([Path,Last,'.html'], File),
            debug(myhttp, "atom: ~q\n", [File]),
            exists_file(File),
            http_reply_file(File, [unsafe(true)], Request)
        ;   atom_concat('https://www.metalevel.at', Path0, Metalevel),
            http_redirect(see_other, Metalevel, Request)
        ).
