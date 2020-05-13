/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The Power of Prolog
   ===================

   Copyright (C) 2005-2020 Markus Triska triska@metalevel.at

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
:- use_module(library(reif)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- server(6012).

(let ((p (make-network-process :name "test" :buffer (current-buffer) :host 'local :service 6012)))
  (process-send-string p "GET /test.html HTTP/1.1\r\n"))

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

server(Port) :-
        socket_server_open('127.0.0.1':Port, Socket),
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
        (   read_line(Stream, Chars) ->
            format("got line: ~w~n", [Chars]),
            append("GET /", Rest, Chars),
            phrase(path(Path), Rest, _),
            format("request is for ~q~n", [Path]),
            (   dif(Path, ""),
                path_file(Path, FileChars),
                path_segments(Path, Segments),
                memberd_t("..", Segments, false),
                atom_chars(File, FileChars),
                exists_file(File) ->
                format("sending ~q~n", [File]),
                file_to_bytes(File, Bytes),
                phrase(http_header(Bytes), Hs0),
                portray_clause(header(Hs0)),
                chars_to_bytes(Hs0, Hs),
                append(Hs, Bytes, Response)
            ;   append("https://www.metalevel.at/", Path, Redirect),
                see_other_chars(Redirect, Rs),
                portray_clause(rs(Rs)),
                chars_to_bytes(Rs, Response)
            ),
            catch(maplist(put_byte(Stream), Response),
                  Err,
                  portray_clause(caught(Err)))
        ;   true
        ).

chars_to_bytes(Chars, Bytes) :-
        atom_chars(A, Chars),
        atom_codes(A, Bytes).

see_other_chars(Link, Chars) :-
        phrase(see_other_page(Link), Ps),
        length(Ps, L),
        phrase(("HTTP/1.1 303 See Other\r\n",
                format_("Location: ~s\r\n", [Link]),
                "Connection: close\r\n",
                format_("Content-Length: ~d\r\n", [L]),
                "Content-Type: text/html; charset=UTF-8\r\n\r\n",
                Ps), Chars).

%?- see_other_chars("https://www.metalevel.at", Cs).

see_other_page(Link) -->
"<html><head><title>303 See Other</title></head><body><h1>See Other</h1><p> \
See other document <a href=\"", format_("~s", [Link]), "\">",
format_("~s", [Link]), "</a></p></body></html>".


http_header(Bytes) -->
        "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: ",
        { length(Bytes, L) },
        format_("~d", [L]),
        "\r\n\r\n".

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- append("GET /", Rest, "GET /test.html HTTP/1.1"),
   phrase(path(File), Rest, _).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

path([F|Fs]) --> [F],
        (   { char_type(F, alnum) }
        ;   { member(F, "./_") }
        ),
        !,
        path(Fs).
path([]) --> [].

read_line(Stream, Chars) :-
        get_byte(Stream, Byte),
        Byte >= 0,
        atom_codes(Char, [Byte]),
        (   member(Char, "\r\n") ->
            Chars = []
        ;   Chars = [Char|Rest],
            read_line(Stream, Rest)
        ).

%?- file_to_bytes('server.pl', Bs).
%@    Bs = [47,42,32,45,32,45,32,45,32,45|...].

file_to_bytes(File, Bytes) :-
        setup_call_cleanup(open(File, read, Stream, [type(binary)]),
                           stream_to_bytes(Stream, Bytes),
                           close(Stream)).

stream_to_bytes(Stream, Bytes) :-
        get_byte(Stream, Byte),
        (   Byte == -1 -> Bytes = []
        ;   Bytes = [Byte|Rest],
            stream_to_bytes(Stream, Rest)
        ).

exists_file(File) :-
        catch(open(File, read, S, []), error(existence_error(_,_),_), Err = true),
        Err \== true,
        close(S).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- path_segments("hello/declarative/world/", S).
%@    S = ["hello","declarative","world",[]].
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

path_segments(Path, Segments) :-
        (   append(Front, ['/'|Ps], Path) ->
            Segments = [Front|Rest],
            path_segments(Ps, Rest)
        ;   Segments = [Path]
        ).

path_file("prolog", "prolog/prolog.html").
path_file(Path, Path).
path_file(Path, HTML) :- append(Path, ".html", HTML).
path_file(Path, File) :-
        path_segments(Path, Segments),
        append(_, [Last,[]], Segments),
        phrase(format_("~s~s.html", [Path,Last]), File).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- exists_file(none).


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


