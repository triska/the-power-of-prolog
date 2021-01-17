:- use_module(library(http/http_server)).

run(Port) :-
        http_listen(Port, [get('', request_response)]).

request_response(_, Response) :-
        http_status_code(Response, 200),
        http_body(Response, text("Hello!")).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- run(6080).
Listening at port 6080
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
