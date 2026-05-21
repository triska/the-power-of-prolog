/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The Power of Prolog
   ===================

   Copyright (C) 2005-2023 Markus Triska triska@metalevel.at

   This Prolog program implements a simple web server that you can use
   to read the book.

   To launch the server on port 6012, install Scryer Prolog and run:

       $ scryer-prolog -g "server(6012)" server.pl

   Then direct your browser to:

       http://localhost:6012/prolog

   The latest version of the book is always available from:

       https://www.metalevel.at/prolog
       ===============================

   Enjoy!

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(sockets)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(iso_ext)).
:- use_module(library(dif)).
:- use_module(library(pio)).
:- use_module(library(reif)).
:- use_module(library(files)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- server(6012).

(let ((p (make-network-process :name "test" :buffer (current-buffer) :host 'local :service 6012)))
  (process-send-string p "GET /test.html HTTP/1.1\r\n"))

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

server(Port) :-
        server('127.0.0.1', Port).

server(IP, Port) :-
        format("Listening on http://~w:~d/~n", [IP, Port]),
        socket_server_open(IP:Port, Socket),
        accept_loop(Socket).

accept_loop(Socket) :-
        format("waiting for connections...~n", []),
        setup_call_cleanup(socket_server_accept(Socket, Client, Stream, [type(binary)]),
                           (   format("handling client ~q...~n", [Client]),
                               request_response(Stream)
                           ),
                           close(Stream)),
        accept_loop(Socket).

request_response(Stream) :-
        (   get_line_to_chars(Stream, Chars, []),
            phrase(("GET /",seq(Path)," ",...) , Chars) ->
            format("request is for ~q~n", [Path]),
            (   dif(Path, ""),
                path_file(Path, File),
                path_segments(Path, Segments),
                memberd_t("..", Segments, false),
                file_exists(File) ->
                format("sending ~q~n", [File]),
                phrase_from_file(seq(FileContents), File, [type(binary)]),
                phrase(http_header(FileContents), Response, FileContents)
            ;   append("https://www.metalevel.at/", Path, Redirect),
                see_other_chars(Redirect, Response),
                format("redirecting to: ~s~n", [Redirect])
            ),
            catch(format(Stream, "~s", [Response]),
                  Err,
                  portray_clause(caught(Err)))
        ;   true
        ).

rn --> "\r\n".

see_other_chars(Link, Chars) :-
        phrase(see_other_page(Link), Ps),
        length(Ps, L),
        phrase(("HTTP/1.1 303 See Other",rn,
                format_("Location: ~s", [Link]),rn,
                "Connection: close",rn,
                format_("Content-Length: ~d", [L]),rn,
                "Content-Type: text/html; charset=UTF-8",rn,
                rn,
                seq(Ps)), Chars).

%?- see_other_chars("https://www.metalevel.at", Cs).

see_other_page(Link) -->
"<html><head><title>303 See Other</title></head><body><h1>See Other</h1><p> \
See other document <a href=\"", format_("~s", [Link]), "\">",
format_("~s", [Link]), "</a></p></body></html>".


http_header(Bytes) -->
        "HTTP/1.1 200 OK",rn,
        "Connection: close",rn,
        { length(Bytes, L) },
        format_("Content-Length: ~d", [L]),rn,
        rn.

path_file("prolog", "prolog/prolog.html").
path_file(Path, HTML) :- append(Path, ".html", HTML).
path_file(Path, File) :-
        path_segments(Path, Segments),
        append(_, [Last,[]], Segments),
        phrase(format_("~s~s.html", [Path,Last]), File).
path_file("prolog/clpfd", "prolog/clpz.html").
path_file(Path, Path).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- socket_client_open('metalevel.at':80, Stream, []),
   maplist(put_char(Stream), "GET / HTTP/1.0\r\n\r\n"),
   repeat,
   get_char(Stream, C),
   write(C),
   false.
%@ HTTP/1.1 200 OK
%@ Date: Sun, 10 May 2020 18:52:09 GMT
%@ Connection: Keep-Alive
%@ Content-Length: 29743
%@ Content-Type: text/html; charset=UTF-8
%@ 
%@ <!DOCTYPE html>
%@ <html>
%@   <head>
%@     <title>Home Page of Markus Triska</title>

caught: error('$interrupt_thrown',repl)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


