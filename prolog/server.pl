:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_unix_daemon)).

:- http_handler(/, handle_request, []).

handle_request(_Request) :-
    format("Content-type: text/plain~n~n"),
    format("Hello!").
