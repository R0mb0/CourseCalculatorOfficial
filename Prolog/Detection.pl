/***** Detection module. *****/

/*Verify if the input string has the right lenght.
 * Input: A list.
 * Output: "true" if the input string has the right lenght.*/
verify_lenght([]) :-
    throw(error(empty_input_list, verify_lenght/1)).
verify_lenght(List) :-
    list(List),
    length(List, N),
    \+(N < 32),
    \+(N > 32).

/*Verify if the input string is in the right format.
 * Input: A list.
 * Output: "true" if the input string is in the right format.*/
verify_format([]) :-
    throw(error(empty_input_list, verify_format/1)).
verify_format(List) :-
    list(List),
    \+(verify_lenght(List)),
    atom_chars(Print, List),
    throw(error(invalid_argument, Print, verify_format/1)).
verify_format(List) :-
    list(List),
    index(1, List, A),
    A == ' ',
    index(4, List, B),
    B == ' ',
    index(7, List, C),
    C == ' ',
    index(14, List, D),
    D == ' ',
    index(16, List, E),
    E == ' ',
    index(18, List, F),
    F == ' ',
    index(22, List, G),
    G == ' ',
    index(25, List, H),
    H == ' '.

/*Verify if the latitude degrees are real.
 * Input: An integer number.
 * Output: "true" if the degrees are real.*/
verify_lat_degrees(Num) :-
    integer(Num),
    \+(Num < 0),
    \+(Num > 89).

/*Verify if the longitude degrees are real.
 * Input: An integer number.
 * Output: "true" if the degrees are real.*/
verify_long_degrees(Num) :-
    integer(Num),
    \+(Num < 0),
    \+(Num > 179).

/*Verify if the primes of detection are real.
 * Input: An integer number.
 * Output: "true" if the primes are real.*/
verify_primes(Num) :-
    integer(Num),
    \+(Num < 0),
    \+(Num > 59).

/*Verify if the latters of detection are real.
 * Input: An integer or float number.
 * Output: "true" if the latters are real.*/
verify_latters(Num) :-
    number(Num),
    \+(Num < 0),
    \+(Num > 59).

/*Verify if the latitude sign is right.
 * Input: A character.
 * Output: "true" if the character is right.*/
verify_lat_sign('N').
verify_lat_sign('S').

/*Verify if the longitude sign of detection is right.
 * Input: A character.
 * Output: "true" if the character is right.*/
verify_long_sign('E').
verify_long_sign('W').

/*Remove the latitude string Part From the detection string, return the longitude string part.
 * Input: A list.
 * Output: A list containing the longitude string part.*/
split([], _) :-
    throw(error(empty_input_list, split/2)).
split(List, Final_list) :-
    list(List),
    length(List, Len),
    Len >= 17,
    drop(17, List, Final_list).
split(List, _) :-
    list(List),
    atom_chars(Print, List),
    throw(error(input_list_has_not_enought_elements, Print, split/2)).
    
/*Transform the input string containing the latitude part into a latitude list, [sign, degrees, primes, latters].
 * Input: A list.
 * Output: A list containing the latitude, an error otherwise.*/
get_latitude([], _) :-
    throw(error(empty_input_list, get_latitude/2)).
get_latitude(List, _) :-
    list(List),
    \+(verify_format(List)),
    atom_chars(Print, List),
    throw(error(invalid_argument, Print, getLatitude/2)).
get_latitude(List, Final_list) :-
    list(List),
    head(List, Sign),
    drop(2, List, A),
    take(2, A, B), 
    number_chars(Degrees, B),
    take(7, List, C),
    drop(5, C, D),
    number_chars(Primes, D),
    take(14, List, E),
    drop(8, E, F),
    number_chars(Latters, F),
    Final_list = [Sign, Degrees, Primes, Latters].

/*Transform the input string containing the longitude part into a longitude list, [sign, degrees, primes, latters].
 * Input: A list.
 * Output: A list containing the longitude, an error otherwise.*/
get_longitude([], _) :-
    throw(error(empty_input_list, get_longitude/2)).
get_longitude(List, _) :-
    list(List),
    \+(verify_format(List)) -> 
    atom_chars(Print, List),
    throw(error(invalid_argument, Print, get_longitude/2)).
get_longitude(List, Final_list) :-
    list(List),
    split(List, List1),
    head(List1, Sign),
    drop(2, List1, A),
    take(3, A, B), 
    number_chars(Degrees, B),
    take(8, List1, C),
    drop(6, C, D),
    number_chars(Primes, D),
    take(15, List1, E),
    drop(9, E, F),
    number_chars(Latters, F),
    Final_list = [Sign, Degrees, Primes, Latters].

/*Verify if the coordinate body is right,
   (in this case the body is the entire coordinate without the sign & degrees),
   this predicate is linked with get_point/2 to not write duplicate code.
 * Input: A list containing a latitude/longitude.
 * Output: "true" if the body is right, an error otherwise.*/
verify_coordinate_body([]) :-
    throw(error(empty_input_list, verify_coordinate_body/1)).
verify_coordinate_body(List) :-
    list(List),
    index(2, List, Primes),
    verify_primes(Primes),
    index(3, List, Latters),
    verify_latters(Latters).
verify_coordinate_body(List) :-
    list(List),
    index(2, List, Primes),
    index(3, List, Latters),
    (\+(verify_primes(Primes)),
    throw(error(wrong_primes_in, List, verify_coordinate_body/1)));
    (\+(verify_latters(Latters)),
    throw(error(wrong_primes_in, List, verify_coordinate_body/1))).

/*Convert the sign of the coordinate into a number for the decimal conversion the coordinate.
 * Input: A character.
 * Output: An integer number.*/
check_sign('S', (-1)).
check_sign('W', (-1)).
check_sign('N', 1).
check_sign('E', 1).

/*Convert a Coordinate in D.M.G. form into Decimal form.
 * Input: A list containing a coordinate.
 * Output: An integer number containing the coordinate in decimal form.*/
convert_to_decimal([], _) :-
    throw(error(empty_input_list, convert_to_decimal/2)).
convert_to_decimal(List, Return_num) :-
    list(List),
    index(0, List, Sign),
    check_sign(Sign, Sign1),
    index(1, List, Degrees),
    index(2, List, Primes),
    index(3, List, Latters),
    A is Latters / 60,
    B is Primes + A,
    C is B / 60,
    D is C + Degrees,
    Return_num is D * Sign1.

/*Merge two numbers into a list that contain both.
 * Input: two integer/float numbers.
 * Output: A list containing both the input numbers.*/
merge_coordinates(Num1, Num2, Final_list) :-
    number(Num1),
    number(Num2),
    Final_list = [Num1, Num2].

/*Convert a detection (in D.M.G. form) into decimal form.
 * Input: A list containing a detection.
 * Output: A list containing the detection in decimal form, an error otherwise.*/
get_point([], _) :-
    throw(error(empty_input_list, get_point/2)).
get_point(List, Final_list) :-
    list(List),
    get_latitude(List, Latitude),
    index(0, Latitude, Sign1),
    index(1, Latitude, Deg1),
    verify_coordinate_body(Latitude),
    get_longitude(List, Longitude),
    index(0, Longitude, Sign2),
    index(1, Longitude, Deg2),
    verify_coordinate_body(Longitude),
    verify_lat_sign(Sign1),
    verify_lat_degrees(Deg1),
    verify_long_sign(Sign2),
    verify_long_degrees(Deg2),
    convert_to_decimal(Latitude, Dlatitude),
    convert_to_decimal(Longitude, Dlongitude),
    merge_coordinates(Dlatitude, Dlongitude, Final_list).
get_point(List, _) :-
    list(List),
    get_latitude(List, Latitude),
    index(0, Latitude, Sign1),
    index(1, Latitude, Deg1),
    get_longitude(List, Longitude),
    index(0, Longitude, Sign2),
    index(1, Longitude, Deg2),
    (\+(verify_lat_sign(Sign1)),
    throw(error(wrong_sign_in, Latitude, get_point/2)));
    (\+(verify_lat_degrees(Deg1)),
    throw(error(wrong_degrees_in, Latitude, get_point/2)));
    (\+(verify_long_sign(Sign2)),
    throw(error(wrong_sign_in, Longitude, get_point/2)));
    (\+(verify_long_degrees(Deg2)),
    throw(error(wrong_degrees_in, Longitude, get_point/2))).