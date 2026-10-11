:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).

video_title(intro, "Intro").
video_title(argument_indexing, "Argument Indexing").
video_title(bitcoinolog, "Bitcoinolog: Reasoning about Bitcoin Adresses with Prolog").
video_title(clauses, "Clauses, Rules and Facts").
video_title(clean_vs_defaulty, "Clean vs. Defaulty Representations").
video_title(collatz, "Collatz Conjecture").
video_title(conformity_testing, "Conformity Testing").
video_title(debugging, "Debugging Prolog Code").
video_title(ediprolog, "ediprolog: Emacs Does Interactive Prolog").
video_title(emacs, "Configuring Emacs for Prolog Development").
video_title(horn_clauses, "Horn Clauses").
video_title(integer_arithmetic, "Integer Arithmetic").
video_title(knights_and_knaves, "Knights and Knaves").
video_title(list_differences, "List Differences").
video_title(lists, "Lists").
video_title(logic, "Logic").
video_title(map_colouring, "Map Colouring").
video_title(metapredicates, "Meta-Predicates").
video_title(naming_predicates, "Naming Prolog Predicates").
video_title(n_queens, "N-Queens").
video_title(operators, "Operators").
video_title(predicate_logic, "Predicate Logic").
video_title(predicates, "Predicates").
video_title(queries, "Queries").
video_title(reading, "Reading Prolog Code").
video_title(representing_strings, "Representing Strings").
video_title(sparrows_on_eagles, "Sparrows on Eagles").
video_title(sudoku, "Sudoku").
video_title(term_rewriting, "Term Rewriting").
video_title(terms, "Terms").
video_title(timetabling, "School Timetabling").
video_title(type_tests, "Type Tests").
video_title(web_scraping, "Web Scraping").
video_title(reading_from_files, "Reading from Files").
video_title(writing, "Writing Prolog Code").
video_title(preparing_prolog, "Preparing Prolog").
video_title(pairs, "Pairs").
video_title(koch_method, "Applying the Koch method to Prolog").
video_title(memory_usage, "Memory usage of Prolog programs").
video_title(meta_interpreters, "Meta-interpreters").
video_title(social_golfer_problem, "Social Golfer Problem").
video_title(pondering_prolog, "Pondering Prolog").
video_title(antipatterns, "Prolog Antipatterns").
video_title(photo_gallery, "Photo Gallery").
video_title(cryptography, "Cryptography").
video_title(dragon_curve, "Dragon Curve").
video_title(tour, "A Tour of Prolog").
video_title(formatting_output, "Formatting Output").
video_title(dcgs, "Definite Clause Grammars (DCGs)").
video_title(toplevel, "The Prolog Toplevel").
video_title(datalog, "Datalog").
video_title(n_queens_faster_labeling, "Faster labeling for N-Queens").
video_title(knights_tour, "Knight's Tour").
video_title(domino_tiling, "Domino Tilings").
video_title(meta_predicate, "meta_predicate declarations").
video_title(term_and_goal_expansion, "Term and Goal Expansion").
video_title(representing_graphs, "Representing Graphs").
video_title(enscryerypt, "Encrypt and decrypt files").

topic_title_videos(logical_foundations, "Logical Foundations of Prolog",
                   [logic,
                    predicate_logic,
                    horn_clauses]).
topic_title_videos(syntax_and_semantics, "Prolog Syntax and Semantics",
                   [terms,
                    lists,
                    type_tests,
                    operators,
                    clauses,
                    predicates,
                    queries,
                    integer_arithmetic,
                    metapredicates,
                    list_differences,
                    pairs,
                    dcgs,
                    meta_predicate,
                    term_and_goal_expansion,
                    conformity_testing]).
topic_title_videos(applications, "Prolog Applications and Showcases",
                   [bitcoinolog,
                    collatz,
                    knights_and_knaves,
                    map_colouring,
                    sudoku,
                    n_queens,
                    term_rewriting,
                    timetabling,
                    web_scraping,
                    social_golfer_problem,
                    knights_tour,
                    photo_gallery,
                    cryptography,
                    domino_tiling,
                    enscryerypt]).
topic_title_videos(development_environment, "Prolog Development Environment",
                   [emacs,
                    ediprolog,
                    debugging]).
topic_title_videos(style_and_technique, "Prolog Style and Technique",
                   [naming_predicates,
                    reading,
                    writing,
                    clean_vs_defaulty,
                    argument_indexing,
                    representing_strings,
                    sparrows_on_eagles,
                    reading_from_files,
                    memory_usage,
                    meta_interpreters,
                    dragon_curve,
                    formatting_output,
                    n_queens_faster_labeling,
                    representing_graphs]).
topic_title_videos(discipline, "The Discipline of Prolog",
                   [tour,
                    datalog,
                    toplevel,
                    preparing_prolog,
                    koch_method,
                    pondering_prolog,
                    antipatterns]).

page -->
        "<center>\n",
        tables([logical_foundations,
                syntax_and_semantics,
                style_and_technique,
                applications,
                development_environment,
                discipline]),
        "</center>\n".

tables([]) --> [].
tables([T|Ts]) --> table(T), tables(Ts).

table(Topic) -->
        { topic_title_videos(Topic, Title, Videos) },
        format_("<br><br><h2>~s</h2><br>~n", [Title]),
        "<table>\n",
        videos(Videos),
        "</table>\n".


videos([]) --> [].
videos([V|Vs]) --> video(V), videos(Vs).

video(Video) -->
        { video_title(Video, Title) },
        format_("<tr><td><b>~s</b></td>~n\
              <td><a href=\"~w\"><img src=\"t_~w.png\" alt=\"~s\"></a></td></tr>~n~n",
               [Title,Video,Video,Title]).


?- phrase_to_stream(video(tour), user_output).

?- phrase_to_stream(table(applications), user_output).

?- phrase_to_stream(page, user_output).
