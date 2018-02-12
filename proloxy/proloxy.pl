/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Proloxy: HTTP reverse proxy written in Prolog.

   This lets you relay traffic to different web servers based on rules.

   See config.pl for a sample configuration.

   Proloxy needs SWI-Prolog >= 7.5.8.

   Copyright (c) 2015-2018 Markus Triska (triska@metalevel.at)

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(proloxy,
          [ proloxy/1,                  % +Port
            output_from_process/2       % +Exec, +Args
          ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/websocket)).

:- dynamic
        user:request_prefix_target/3,
        user:transmit_header_field/1,
        user:add_header/1.

:- multifile
        user:request_prefix_target/3,
        user:transmit_header_field/1.

:- http_handler(/, custom_target, [prefix]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Main logic. Relay requests based on the defined rules.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- debug(proloxy).

custom_target(Request) :-
        debug(proloxy, "request: ~q\n", [Request]),
        (   user:request_prefix_target(Request, Prefix, TargetURI) ->
            debug(proloxy, "target URI: ~q\n", [TargetURI]),
            % commit to first matching clause
            (   TargetURI == (-) -> true
            ;   websocket_connection(Request) ->
                debug(proloxy, "upgrading to websocket\n", []),
                proxy_websocket(Request, TargetURI)
            ;   memberchk(request_uri(URI), Request),
                memberchk(method(Method0), Request),
                method_pure(Method0, Method),
                proxy(Method, Prefix, URI, TargetURI, Request)
            )
        ;   throw(http_reply(unavailable(p([tt('request_prefix_target/3'),
                                            ': No matching rule for ~q'-[Request]]))))
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   WebSocket support.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

proxy_websocket(Request, TargetURI) :-
        % first, negotiate Protocol with the target ...
        option(sec_websocket_protocol(Protocols0), Request),
        split_string(Protocols0, ",", " ", Protocols1),
        maplist(atom_string, Protocols, Protocols1),
        debug(proloxy, "protocols: ~w\n", [Protocols]),
        http_open_websocket(TargetURI, TargetWS, [subprotocols(Protocols)]),
        % ... then use the negotiated protocol for the client
        ws_property(TargetWS, subprotocol(Protocol)),
        http_upgrade_to_websocket(websocket_loop(TargetWS), [subprotocols([Protocol])], Request).

websocket_loop(TargetWS, ClientWS) :-
        stream_pair(TargetWS, TargetIn, _),
        stream_pair(ClientWS, ClientIn, _),
        debug(proloxy, "waiting for input\n", []),
        wait_for_input([ClientIn,TargetIn], ReadyList, 10),
        catch((   ReadyList == [] -> true
              ;   ReadyList = [_,_] ->
                  ws_from_to(ClientWS, TargetWS),
                  ws_from_to(TargetWS, ClientWS)
              ;   ReadyList == [ClientIn] ->
                  ws_from_to(ClientWS, TargetWS)
              ;   ws_from_to(TargetWS, ClientWS)
              ), ws_proxy_done, Done=true),
        (   Done == true ->
            ws_close(TargetWS, 1000, "Goodbye"),
            ws_close(ClientWS, 1000, "Goodbye")
        ;   websocket_loop(TargetWS, ClientWS)
        ).

ws_from_to(FromWS, ToWS) :-
        ws_receive(FromWS, Message),
        string_length(Message.data, Len),
        debug(proloxy, "ws received: ~w (~w)\n", [Len,Message.opcode]),
        (   Message.opcode == close ->
            debug(proloxy, "websocket closed\n", []),
            throw(ws_proxy_done)
        ;   ws_send(ToWS, Message)
        ).

websocket_connection(Request) :-
        option(method(get), Request),
        option(upgrade(Upgrade), Request),
        downcase_atom(Upgrade, websocket),
        option(connection(Connection), Request),
        contains_upgrade(Connection).

contains_upgrade(Atom) :-
        split_string(Atom, ",", " ", Tokens),
        once((member(Token, Tokens),string_lower(Token, "upgrade"))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Relay request to TargetURI. Rewrite the response if necessary.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

proxy(data(Method), _, _, TargetURI, Request) :-
        read_data(Request, Data),
        http_open(TargetURI, In, [method(Method), post(Data),
                                  % cert_verify_hook(cert_accept_any),
                                  header(content_type, ContentType)]),
        call_cleanup(read_string(In, _, Bytes),
                     close(In)),
        throw(http_reply(bytes(ContentType, Bytes))).
proxy(other(Method), Prefix, URI, TargetURI, _) :-
        http_open(TargetURI, In, [method(Method),
                                  % cert_verify_hook(cert_accept_any),
                                  redirect(false),
                                  header(location, Location0),
                                  status_code(Code),
                                  header(content_type, ContentType),
                                  headers(Headers0)]),
        findall(H, user:add_header(H), AddHeaders),
        call_cleanup(read_string(In, _, Bytes),
                     close(In)),
        (   redirect_code(Code, Status) ->
            (   uri_is_global(Location0) ->
                Location = Location0
            ;   atom_concat(Prefix, Location0, Location)
            ),
            Reply =.. [Status,Location],
            throw(http_reply(Reply, AddHeaders))
        ;   Code == 404 ->
            throw(http_reply(not_found(URI), AddHeaders))
        ;   include(retain_header, Headers0, Headers1),
            append(Headers1, AddHeaders, Headers),
            throw(http_reply(bytes(ContentType, Bytes), Headers))
        ).

retain_header(Hdr) :-
        functor(Hdr, Name, _),
        user:transmit_header_field(Name).

redirect_code(301, moved).
redirect_code(302, moved_temporary).
redirect_code(303, see_other).

read_data(Request, bytes(ContentType, Bytes)) :-
        memberchk(input(In), Request),
        memberchk(content_type(ContentType), Request),
        (   memberchk(content_length(Len), Request) ->
            read_string(In, Len, Bytes)
        ;   read_string(In, _, Bytes)
        ).

method_pure(M0, M) :- once(defaulty_pure(M0, M)).

defaulty_pure(post, data(post)).
defaulty_pure(put, data(put)).
defaulty_pure(M, other(M)).

proloxy(Port) :-
        http_server(http_dispatch, [port(Port)]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Execute a process with arguments, emit stdout and stderr on stdout.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

output_from_process(Exec, Args) :-
        process_create(Exec, Args, [stdout(pipe(Stream)),
                                    stderr(pipe(Stream))]),
        copy_stream_data(Stream, current_output),
        % the process may terminate with any exit code.
        catch(close(Stream), error(process_error(_,exit(_)), _), true).

http:http_address -->
        html(address([a(href('https://www.metalevel.at/proloxy/'),
                        'Proloxy')])).
