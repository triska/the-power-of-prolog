/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Proloxy sample configuration file.

   This simple example file contains 2 clauses of request_prefix_target/3.

   The first rule relays URIs that start with /rits to a (RITS) server
   on port 4040.

   The second and final rule relays all other requests to a different
   web server on port 3031.

   The order of rules is significant, because Proloxy commits to the
   *first* rule that succeeds. In this example, if the order of rules
   were exchanged, all requests would be relayed to port 3031.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Rule 1: Provide access to a RITS server via the prefix /rits.
   For example, accessing http://your-url.com/rits/demo.html
   fetches /demo.html from the RITS server running on port 4040.

   The prefix (second argument) is also /rits, so that redirects from
   the RITS server are rewritten in such a way that the following
   client request is again correctly delegated to the RITS server.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

request_prefix_target(Request, '/rits', Target) :-
        memberchk(request_uri(URI), Request),
        atom_concat('/rits', Rest, URI),
        atomic_list_concat(['http://localhost:4040',Rest], Target).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Rule 2: Relay everything else to a local web server on port 3031.
   Note that the original path is used, and Prefix is therefore empty.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

request_prefix_target(Request, '', Target) :-
        memberchk(request_uri(URI), Request),
        atomic_list_concat(['http://localhost:3031',URI], Target).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Recommended configuration for relaying response header fields.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

transmit_header_field(cache_control).
transmit_header_field(expires).
transmit_header_field(last_modified).
transmit_header_field(pragma).
