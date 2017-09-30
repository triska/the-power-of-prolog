/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The Power of Prolog
   ===================

   Copyright (C) 2005-2017 Markus Triska triska@metalevel.at

   Online version:

   https://www.metalevel.at/prolog
   ===============================

   Example usage to spawn a server on port 5051:

       $ swipl main.pl --port=5051

   then direct your browser to:

   http://localhost:5051/prolog

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(main,
	  []).

:- initialization main.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_log)).
:- use_module(library(http/mimetype)).
:- use_module(library(settings)).
:- use_module(library(optparse)).

:- set_prolog_flag(default_mimetype, text/plain).

:- http_handler(/, handle_request, [prefix]).
:- http_handler('/prolog', http_reply_file('prolog/prolog.html', []), []).

main :-
        OptSpec = [[opt(port),type(integer),default(5053),
                    shortflags([p]),longflags([port]),
                    help('port for launching the server')]],
        opt_arguments(OptSpec, [port(Port)], _),
        http_server(http_dispatch, [port(Port)]).

handle_request(Request) :-
        memberchk(path(Path0), Request),
        atom_concat(., Path0, Path1),
        http_safe_file(Path1, []),
        absolute_file_name(Path1, Path),
        (   path_file(Path, File),
            exists_file(File),
            http_reply_file(File, [unsafe(true)], Request)
        ;   atom_concat('https://www.metalevel.at', Path0, Metalevel),
            http_redirect(see_other, Metalevel, Request)
        ).

path_file(Path, Path).
path_file(Path, HTML) :- atom_concat(Path, '.html', HTML).
path_file(Path, File) :-
        path_segments_atom(_/Last/'', Path),
        atomic_list_concat([Path,Last,'.html'], File).
