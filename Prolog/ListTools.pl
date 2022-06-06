/***** List tools module. *****/

/*Like the index function in Haskell. 
 * Input: An integer index, a list.
 * Output: The element of the list at the index specified.*/
index(_, [], _) :-
    throw(error(empty_input_list, index/3)).
index(0, [X], X).
index(0, [H|_], H).
index(N, [_], _) :-
    integer(N),
    N < 0,
    throw(error(negative_index, N, index/3)).
index(N, [_|T], E) :-
    integer(N),
    N1 is N - 1, 
    index(N1, T, E).

/*Like the head function in Haskell. 
 * Input: A list.
 * Output: The first element of list.*/
head([], _) :-
    throw(error(empty_input_list, head/2)).
head([H], H).
head([H|_], H).

/*Like the tail function in Haskell. 
 * Input: A list.
 * Output: A list without the first element.*/
tail([], _) :-
    throw(error(empty_input_list, tail/2)).
tail([T], T).
tail([_|T], T).

/*Like the drop function in Haskell. 
 * Input: An integer number, a list.
 * Output: A list without the number of elements specified from the head.*/
drop(_, [], _) :-
    throw(error(empty_input_list, drop/3)).
drop(0, List, Final_list) :-
    list(List),
    Final_list = List.
drop(1, List, Final_list) :-
    list(List),
    tail(List, Final_list).
drop(N, List, _) :-
    integer(N),
    list(List),
    N < 0,
    throw(error(negative_parameter, N, drop/3)).
drop(N, List, Final_list) :-
    integer(N),
    list(List),
    N1 is N - 1,
    tail(List, Rlist),
    drop(N1, Rlist, Final_list).

/*Like the init function in Haskell. 
 * Input: A list.
 * Output: A list without the last element.*/
init([], _) :-
    throw(error(empty_input_list, init/2)).
init([X], X).
init(List, Final_list) :-
    list(List),
    reverse(List, [_|List1]), 
    reverse(List1, Final_list).

/*Remove ns elements from the input list.
 * Input: An integer number, a List.
 * Output: A list without the number of elements specified from the tail.*/
remove_from_tail(_, [], _) :-
    throw(error(empty_input_list, remove_from_tail/3)).
remove_from_tail(_, [_], []).
remove_from_tail(0, List, Final_list) :-
    list(List),
    Final_list = List.
remove_from_tail(1, List, Final_list) :-
    list(List),
    init(List, Final_list).
remove_from_tail(N, List, _) :-
    integer(N),
    list(List),
    N < 0,
    throw(error(negative_parameter, N, remove_from_tail/3)).
remove_from_tail(N, List, Final_list) :-
    integer(N),
    list(List),
    N1 is N - 1,
    init(List, List1),
    remove_from_tail(N1, List1, Final_list).

/*Like the take function in Haskell. 
 * Input: An integer number, a list.
 * Output: A list with only the number of elements specified from the head.*/
take(_, [], _) :-
    throw(error(empty_input_list, take/3)).
take(_, [X], X).
take(N, List, _) :-
    integer(N),
    list(List),
    N < 0,
    throw(error(negative_parameter, N, take_list/3)).
take(N, List, Final_list) :-
    integer(N),
    list(List),
    length(List, Len),
    N1 is Len - N,
    remove_from_tail(N1, List, Final_list).

/***** End module. *****/