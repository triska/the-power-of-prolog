/*
consult this program and type "main." to start it.
You will then be presented with the top level menu.
Hitting Escape at this level will end the program.
Hitting it in a submenu will bring you up a level to it's parent menu.
To select an option or submenu just hit the letter key for the single Capital letter in each option/submenu.
That will invoke it's associated predicate...which you'll see proof of on the screen for one second
then the screen will clear and the program will wait for another selection.

Note I changed Id, Parent Id into a unique key list so that you can have identical options
that do different things because the list which reflects each option's position in the menu is unique.
This is how I did it in Tcl a while back.
doOptA...doOptL is where the meat of the application resides in the real situation

For completeness...here's what the menu list gets changed into.
?- menu_rec(Key,Options). gives...
[main_mnu] [optA,sub_mnuB,optK,sub_mnuJ]
[main_mnu,sub_mnuB] [optC,sub_mnuD,sub_mnuG,optJ]
[main_mnu,sub_mnuB,sub_mnuD] [optE,optF]
[main_mnu,sub_mnuB,sub_mnuG] [optH,optI]
[main_mnu,sub_mnuJ] [optK,optL]

I'd be very grateful for your comments/improvements.
I've just hacked this out at the moment with what little I know to let you see what I'm trying to do.
*/
waitkey(Ch) :- get_single_char(Ch). 
cls :- write('\e[2J').
:- dynamic mnu_rec/4. 

doOptA :- write("doing OptA"),nl.
doOptK :- write("doing OptK"),nl.
doOptC :- write("doing OptC"),nl.
doOptJ :- write("doing OptJ"),nl.
doOptE :- write("doing OptE"),nl.
doOptF :- write("doing OptF"),nl.
doOptH :- write("doing OptH"),nl.
doOptI :- write("doing OptI"),nl.
doOptK :- write("doing OptK"),nl.
doOptL :- write("doing OptL"),nl.

%need UNIQUE key for SAME last field in key list to call different predicates for same last field
%e.g. imagine option "Print" for different menus...it would probably be to print different things 
%and therefore need a different predicate each time eg. print_this :-, print_that :-
%This lets you do that.
mnu_option_pred_for_key([main_mnu,optA], doOptA).
mnu_option_pred_for_key([main_mnu,optK], doOptK).
mnu_option_pred_for_key([main_mnu,sub_mnuB,optC], doOptC).
mnu_option_pred_for_key([main_mnu,sub_mnuB,optJ], doOptJ).
mnu_option_pred_for_key([main_mnu,sub_mnuB,sub_mnuD,optE], doOptE).
mnu_option_pred_for_key([main_mnu,sub_mnuB,sub_mnuD,optF], doOptF).
mnu_option_pred_for_key([main_mnu,sub_mnuB,sub_mnuG,optH], doOptH).
mnu_option_pred_for_key([main_mnu,sub_mnuB,sub_mnuG,optI], doOptI).
mnu_option_pred_for_key([main_mnu,sub_mnuJ,optK], doOptK).
mnu_option_pred_for_key([main_mnu,sub_mnuJ,optL], doOptL).


option_for_ele(Ele,Option) :- atom(Ele), Option=Ele.
option_for_ele(Ele,Option) :- Ele=[Option|T].
    
list_to_mnu_recs(Key,L) :- 
    atom(L).
list_to_mnu_recs(Key,L) :-

    L = [H|T],
    Nm = H,
    maplist(option_for_ele,T,Options),
    append(Key,[Nm],New_key),
    assertz(mnu_rec(New_key, Options)),
    maplist(list_to_mnu_recs(New_key),L). 

find_ch_in_option(Ch,Option,Hit) :-
    string_codes(Needle, [Ch]),
    string_upper(Needle, UCNeedle),
    sub_string(Option, _, Len, _, UCNeedle),
    Hit = Len.
find_ch_in_option(Ch,Option,Hit) :-
    Hit = 0.
   
new_mnu_or_action_for_key(Key) :- 
    mnu_rec(Key, _), %rules out new menu if this fails
    mnu_for_key(Key).
new_mnu_or_action_for_key(Key) :-
    mnu_option_pred_for_key(Key, Mnu_option_pred),
    call(Mnu_option_pred),
    sleep(1),
    append(Key_without_last, [_], Key),
    mnu_for_key(Key_without_last).
    
indexof(Index, Item, List):-
  nth0(Index, List, Item).
indexof(-1, _, _).       

process_ch(Ch,Options,Key) :-
    Ch =:= 27,
    append(Key_without_last, [_], Key),
    Key_without_last = [],
    cls,
    write("closing app"),nl.
process_ch(Ch,Options,Key) :-
    Ch =:= 27,
    append(Key_without_last, [_], Key),
    mnu_for_key(Key_without_last).
process_ch(Ch,Options,Key) :-    
    maplist(find_ch_in_option(Ch),Options,Hits),
    indexof(Index, 1, Hits),
    nth0(Index,Options,Option),
    append(Key,[Option],New_key),
    new_mnu_or_action_for_key(New_key).
    
mnu_for_key(Key) :-
    mnu_rec(Key,Options),
    atomic_list_concat(Options,' ',Mnu_str),
    cls,
    write(Mnu_str),nl,
    waitkey(Ch),
    process_ch(Ch,Options,Key).    
    
main :-

    Mnu_tree = ['main_mnu', 
                            optA, 
                            ['sub_mnuB', 
                                optC, 
                                ['sub_mnuD', optE, optF], 
                                ['sub_mnuG', optH, optI], 
                                optJ
                            ], 
                            optK, 
                            ['sub_mnuJ', optK, optL]
                       ],
    list_to_mnu_recs([],Mnu_tree),
    mnu_for_key([main_mnu]).
%?- mnu_rec(A, B).
%?- main.