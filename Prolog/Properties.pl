/***** Properties module. *****/

/*Calculate the distance between two detections in decimal form.
 * Input: Two lists containing detections in decimal form.
 * Output: An integer number that represent the distance (in Km) between the two detections.*/
distance([], _, _) :-
    throw(error(empty_first_input_list, distance/3)).
distance(_, [], _) :-
    throw(error(empty_second_input_list, distance/3)).
distance(List1, List2, Return_num) :-
    list(List1),
    list(List2),
    nth(1,List1,Lat1),
    number(Lat1),
    nth(2,List1,Long1),
    number(Long1),
    nth(1,List2,Lat2),
    number(Lat2),
    nth(2,List2,Long2),
    number(Long2),
    A is pi / 180,
    B is Long1 - Long2,
    C is B * A,
    D is cos(C),
    Lat1n is Lat1 * A,
    Lat2n is Lat2 * A,
    E is cos(Lat2n),
    F is cos(Lat1n),
    G is D * E * F,
    H is sin(Lat2n),
    I is sin(Lat1n),
    L is H * I,
    M is L + G,
    N is acos(M),
    Return_num is 6372.795477598 * N.

/*Calculate the direction between two detections in decimal form.
 * Input: Two lists containing detections in decimal form.
 * Output: An integer number that represent the direction (in degrees) between the two detections.*/
direction([], _, _) :-
    throw(error(empty_first_input_list, direction/3)).
direction(_, [], _) :-
    throw(error(empty_second_input_list, direction/3)).
direction(List1, List2, Return_num) :-
    list(List1),
    list(List2),
    nth(1,List1,Lat1),
    number(Lat1),
    nth(2,List1,Long1),
    number(Long1),
    nth(1,List2,Lat2),
    number(Lat2),
    nth(2,List2,Long2),
    number(Long2),
    A is pi / 180,
    Lat1n is Lat1 * A,
    Lat2n is Lat2 * A,
    (Lat2 == Lat1 -> 
        Phi is pi / 180 * 0.000000001
    ;
        B is pi / 4,
        C is Lat1n / 2,
        D is C + B,
        E is tan(D),
        F is Lat2n / 2,
        G is F + B,
        H is tan(G),
        I is H / E,
        Phi is log(I)
    ),
    (Long2 == Long1 ->
        Lon is pi / 180 * 0.000000001
    ;
        L is Long1 - Long2,
        M is abs(L), 
        N is M * A,
        (N > 180 -> 
            Lon is N mod 180
        ;
            Lon = N
        )
    ),
    O is abs(Phi),
    P is atan2(Lon, O),
    Return_num is P / pi * 180.

/*Calculate the inverse direction between two detections in decimal form.
 * Input: Two lists containing detections in decimal form.
 * Output: An integer number that represent the inverse direction (in degrees) between the two detections.*/
inverse_direction([], _, _) :-
    throw(error(empty_first_input_list, inverse_direction/3)).
inverse_direction(_, [], _) :-
    throw(error(empty_second_input_list, inverse_direction/3)).
inverse_direction(List1, List2, Return_num) :-
    list(List1),
    list(List2),
    direction(List1, List2, Dir),
    Return_num is Dir + 180.

/***** End module. *****/