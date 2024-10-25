/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Menu show case.
   ===============

   Written 2016-2024 by Markus Triska (triska@metalevel.at).
   Public domain code. Tested with Scryer Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(charsio)).  % for get_single_char/1

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   There are two kinds of elements in a menu:

   *) submenus, represented by menu(Title,Key,Subs), where Subs are menus
   *) options, represented as option(Title,Key).

   For general points regarding data representation in Prolog, please see:

      https://www.metalevel.at/prolog/data
      ====================================

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

is_option(option(_,_)).

is_menu(menu(_,_,Subs)) :-
        maplist(is_element, Subs).

is_element(E) :- is_option(E).
is_element(E) :- is_menu(E).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sample menu.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

menu(menu('Main', _,
          [option(optA, a),
           menu('Submenu B', b,
                [option(optC, c),
                 menu('Submenu D', d,
                      [option(optE, e),
                       option(optF, f)]),
                 menu('Submenu G', g,
                      [option(optH, h),
                       option(optI, i)]),
                 option(optJ, j)]),
           option(optK, k),
           menu('Submenu J', j,
                [option(optK, k),
                 option(optL, l)])])).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Pure conversion of a menu to a list of characters that display it.

   See logical purity for more information why side-effects are better
   kept separate from the main logic:

      https://www.metalevel.at/prolog/purity
      ======================================

   Sample use:

   ?- use_module(library(lambda)).
   %@    true.
   ?- Cs+\(menu(M), phrase(menu_chars(M), Cs)).
   %@    Cs = "optA (a)   Submenu  ...".
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

menu_chars(menu(_,_,Subs))  --> format_menu(Subs).
menu_chars(option(Title,_)) --> format_("~w", [Title]).

format_menu([]) --> [].
format_menu([M|Ms]) --> format_menu_(Ms, M).

format_menu_([], M) --> format_element(M).
format_menu_([N|Ms], M) -->
        format_element(M),
        "   ",
        format_menu_(Ms, N).

format_element(option(Title,Key)) --> format_("~w (~w)", [Title,Key]).
format_element(menu(Title,_,_)) --> format_("~w", [Title]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Pressing a key when a menu is displayed yields a submenu.

   Declaratively, pressing a key when a menu is displayed is a
   relation between:

      *) the menu that is currently displayed
      *) the key that is pressed
      *) the submenu that is displayed next.

   So, it is a relation between Menu0, Key and Menu.

   See "Thinking in States" for more information about such state
   transitions:

      https://www.metalevel.at/tist/
      ==============================

   The nice thing is that such relations can be for example *tested*
   by simple Prolog queries:

   ?- menu(M0), menu0_key_menu(M0, b, M), M = menu('Submenu B', _, _).

   This is not possible, or at least *much harder*, if menus are only
   implicitly displayed on the system terminal, or such transitions
   cause global state modifications that complicate exhaustive tests.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

menu0_key_menu(menu(_,_,Subs), Key, Option) :-
        member(Option, Subs),
        Option = option(_,Key).
menu0_key_menu(menu(_,_,Subs), Key, Menu) :-
        member(Menu, Subs),
        Menu = menu(_,Key,_).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                          IMPURITIES AHEAD!
                          =================

   Up to this point in the source file, *everything* can be tested by
   pure methods. However, in this concrete use case, we also actually
   need to "do" something, and that is *displaying* output on the
   system terminal and reading user input. Note that this is much
   harder to *test*, because it essentially requires interaction with
   other parts of the system. So, we keep this as small as possible.

   The impure fragments are only about 30 lines of code!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Display a menu on the screen.

   Sample use:

   ?- menu(M), display_menu(M).
   %@ optA (a)   Submenu B   optK (k)   Submenu J
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(pio)).

display_menu(Menu) :-
        phrase_to_stream((menu_chars(Menu),"\n"), user_output).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interaction loop.
   =================

   Interacting with the menu is simply a matter of connecting the
   available components.

   Note that because almost everything is available for pure
   reasoning, we can easily go *back* to previous points in time. We
   do this by carrying around a *history* of previous menus.

   You can go back to the previous point by pressing ^.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

run :-
        menu(M),
        interaction(M, []).

interaction(M0, Hs0) :-
        display_menu(M0),
        get_single_char(C),
        (   C = (^) ->
            (   Hs0 = [Prev|Hs] ->
                interaction(Prev, Hs)
            ;   run % no remaining history --> return to beginning
            )
        ;   menu0_key_menu(M0, C, M) ->
            interaction(M, [M0|Hs0])
        ;   format("no entry found for \"~w\" -- please try again\n", [C]),
            interaction(M0, Hs0)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sample run:

   ?- run.
   %@ optA (a)   Submenu B   optK (k)   Submenu J
   %@ optA
   %@ optA (a)   Submenu B   optK (k)   Submenu J
   %@ optC (c)   Submenu D   Submenu G   optJ (j)
   %@ no entry found for "b" -- please try again
   %@ optC (c)   Submenu D   Submenu G   optJ (j)
   %@ optE (e)   optF (f)
   %@ optE
   %@ optE (e)   optF (f)
   %@ optC (c)   Submenu D   Submenu G   optJ (j)
   %@ no entry found for "a" -- please try again
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
