/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   Creating a photo gallery with Prolog.
   Written September 2021 by Markus Triska (triska@metalevel.at)

   Sample invocation, using Scryer Prolog:

       $ scryer-prolog -g run,halt gallery.pl

   Video:

       https://www.metalevel.at/prolog/videos/photo_gallery
       ====================================================

   To create thumbnails, evaluate the following code with Tcl:

        foreach f [lsort [glob {[PI]*.JPG}]] {
            puts "doing $f"
            if {![file exists t_$f]} {
                exec convert $f -resize 500x t_$f
            }
        }

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(files)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(time)).
:- use_module(library(format)).
:- use_module(library(pio)).

photo_file_name --> ("P"|"I"),...,".JPG".

photos([]) --> [].
photos([F|Fs]) -->
        (   { phrase(photo_file_name, F) } ->   % incompleteness
            [F]
        ;   []
        ),
        photos(Fs).

with_day_key(T-P, D-(T-P)) :-
        phrase(format_time("%Y%m%d", T), D).

write_day(D-TPs) :-
        append(D, ".html", File),
        portray_clause(writing(File)),
        phrase_to_file(day(TPs), File).

day(TPs) -->
        tag(html,
            tag(table,
                content(TPs))).

tag(T, GRBody) -->
        format_("<~w>", [T]),
        GRBody,
        format_("</~w>", [T]).

content([]) --> [].
content([_-P|TPs]) -->
        tag(tr,
            (   tag(td, format_("<a href=\"~s\"><img src=\"t_~s\"></a>", [P,P])),
                tag(td, format_("<a href=\"~s\"><pre>~s</pre></a>", [P,P]))
            )),
        "\n",
        content(TPs).

write_index(Groups) :-
        phrase_to_file(index(Groups), "index.html").

index(Groups) -->
        tag(html, tag(body, tag(pre, links_to_files(Groups)))).

links_to_files([]) --> [].
links_to_files([D-[T-_|_]|Groups]) -->
        { phrase(format_time("%a %d.%m.%Y", T), Link) },
        format_("<a href=\"~s.html\">~s</a>", [D,Link]),
        "<br>\n",
        links_to_files(Groups).

run :-
        directory_files(".", Fs),
        phrase(photos(Fs), Ps),
        maplist(file_modification_time, Ps, Ts),
        pairs_keys_values(TPs0, Ts, Ps),
        keysort(TPs0, TPs),
        maplist(with_day_key, TPs, DTPs),
        group_pairs_by_key(DTPs, Groups),
        maplist(write_day, Groups),
        write_index(Groups).
