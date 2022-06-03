/***** Detection module. *****/

/*Verify if the input string has the right lenght.
 * Input: A list.
 * Output: A boolean that is "true" if the input string has the right lenght, "false" otherwise.*/
verify_lenght([], _) :-
    throw(error(empty_input_list, verify_lenght/2)).
verify_lenght(List, Return_bool) :-
    (list(List) -> 
        length(List, N),
        (N < 32 -> 
            Return_bool = false
        ;
            (N > 32 -> 
                Return_bool = false
            ;
                Return_bool = true
            )
        )  
    ;
        throw(error(wrong_input_list, List, verify_lenght/2))
    ).

/*Verify if the input string is in the right format.
 * Input: A list.
 * Output: A boolean that is "true" if the input string is in the right format, "false" otherwise.*/
verify_format([], _) :-
    throw(error(empty_input_list, verify_format/2)).
verify_format(List, Return_bool) :-
    (list(List) -> 
        verify_lenght(List, Bool),
        (Bool == true -> 
            index(1, List, A),
            index(4, List, B),
            index(7, List, C),
            index(14, List, D),
            index(16, List, E),
            index(18, List, F),
            index(22, List, G),
            index(25, List, H),
            (A == ' ' -> 
                (B == ' ' -> 
                    (C == ' ' -> 
                        (D == ' ' -> 
                            (E == ' ' -> 
                                (F == ' ' -> 
                                    (G == ' ' -> 
                                        (H == ' ' -> 
                                            Return_bool = true 
                                        ;
                                            Return_bool = false 
                                        )
                                    ;
                                        Return_bool = false 
                                    )
                                ;
                                    Return_bool = false 
                                )
                            ;
                                Return_bool = false 
                            )
                        ;
                            Return_bool = false 
                        )
                    ;
                        Return_bool = false 
                    )
                ;
                    Return_bool = false 
                )
            ;
                Return_bool = false 
            )
        ;
            atom_chars(Print, List),
            throw(error(invalid_argument, Print, verify_format/2))
        )
    ;
        throw(error(wrong_input_list, List, verify_format/2))
    ).

/*Verify if the latitude degrees are real.
 * Input: An integer number.
 * Output: A boolean that is "true" if the degrees are real, "false" otherwise.*/
verify_lat_degrees(Num, Return_bool) :-
   (integer(Num) ->
        (Num < 0 -> 
            Return_bool = false
        ;
            (Num > 89 -> 
                Return_bool = false
            ;
                Return_bool = true
            )
        )
    ;
        throw(error(wrong_input_number, Num, verify_lat_degrees/2))
    ).

/*Verify if the longitude degrees are real.
 * Input: An integer number.
 * Output: A boolean that is "true" if the degrees are real, "false" otherwise.*/
verify_long_degrees(Num, Return_bool) :-
   (integer(Num) ->
        (Num < 0 -> 
            Return_bool = false
        ;
            (Num > 179 -> 
                Return_bool = false
            ;
                Return_bool = true
            )
        )
    ;
        throw(error(wrong_input_number, Num, verify_long_degrees/2))
    ).

/*Verify if the primes of detection are real.
 * Input: An integer number.
 * Output: A boolean that is "true" if the primes are real, "false" otherwise.*/
verify_primes(Num, Return_bool) :-
    (integer(Num) -> 
        (Num < 0 -> 
            Return_bool = false
        ;
            (Num > 59 -> 
                Return_bool = false
            ;
                Return_bool = true
            )
        )  
    ;
        throw(error(wrong_input_number, Num, verify_primes/2))
    ).

/*Verify if the latters of detection are real.
 * Input: An integer or float number.
 * Output: A boolean that is "true" if the latters are real, "false" otherwise.*/
verify_latters(Num, Return_bool) :-
    (number(Num) ->
        (Num < 0 -> 
            Return_bool = false
        ;
            (Num > 59 -> 
                Return_bool = false
            ;
                Return_bool = true
            )
        )
    ;
        throw(error(wrong_input_number, Num, verify_latters/2))
    ).

/*Verify if the latitude sign is right.
 * Input: A character.
 * Output: A boolean that is "true" if the character is right, "false" otherwise.*/
verify_lat_sign(Character, Return_bool) :-
    (nonvar(Character) -> 
        (Character == 'N' -> 
            Return_bool = true
        ;
            (Character == 'S' -> 
                Return_bool = true
            ;
                Return_bool = false
            )
        )
    ;
        throw(error(no_input_character, verify_lat_sign/2))
    ).

/*Verify if the longitude sign of detection is right.
 * Input: A character.
 * Output: A boolean that is "true" if the character is right, "false" otherwise.*/
verify_long_sign(Character, Return_bool) :-
    (nonvar(Character) -> 
        (Character == 'E' -> 
            Return_bool = true
        ;
            (Character == 'W' -> 
                Return_bool = true
            ;
                Return_bool = false
            )
        )
    ;
        throw(error(no_input_character, verify_long_sign/2))
    ).

/*Remove the latitude string Part From the detection string, return the longitude string part.
 * Input: A list.
 * Output: A list containing the longitude string part.*/
split([], _) :-
    throw(error(empty_input_list, split/2)).
split(List, Final_list) :-
    (list(List) -> 
        length(List, Len),
        (Len >= 17 ->
            drop(17, List, Final_list)
        ;
            atom_chars(Print, List),
            throw(error(input_list_has_not_enought_elements, Print, split/2))
        )
    ;
        throw(error(wrong_input_list, List, split/2))
    ).    

/*Transform the input string containing the latitude part into a latitude list, [sign, degrees, primes, latters].
 * Input: A list.
 * Output: A list containing the latitude.*/
get_latitude([], _) :-
    throw(error(empty_input_list, get_latitude/2)).
get_latitude(List, Final_list) :-
    (list(List) -> 
        verify_format(List, Bool),
        (Bool == false ->
            atom_chars(Print, List),
            throw(error(invalid_argument, Print, getLatitude/2))
        ;
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
            Final_list = [Sign, Degrees, Primes, Latters]
        )
    ;
        throw(error(wrong_input_list, List, get_latitude/2))
    ).

/*Transform the input string containing the longitude part into a longitude list, [sign, degrees, primes, latters].
 * Input: A list.
 * Output: A list containing the longitude.*/
get_longitude([], _) :-
    throw(error(empty_input_list, get_longitude/2)).
get_longitude(List, Final_list) :-
    (list(List) -> 
        verify_format(List, Bool),
        (Bool == false ->
            atom_chars(Print, List),
            throw(error(invalid_argument, Print, get_longitude/2))
        ;
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
            Final_list = [Sign, Degrees, Primes, Latters]
        )
    ;
        throw(error(wrong_input_list, List, get_longitude/2))
    ).

/*Verify if the coordinate body is right,
   (in this case the body is the entire coordinate without the sign & degrees),
   this predicate is linked with get_point/2 to not write duplicate code.
 * Input: A list containing a latitude/longitude.
 * Output: A boolean that is "true" if the body is right, "false" otherwise.*/
verify_coordinate_body([], _) :-
    throw(error(empty_input_list, verify_coordinate_body/2)).
verify_coordinate_body(List, Return_bool) :-
    (list(List) -> 
        index(2, List, Primes),
        verify_primes(Primes, B1),
        (B1 == false ->
            throw(error(wrong_primes_in, List, verify_coordinate_body/2))
        ;
            index(3, List, Latters),
            verify_latters(Latters, B2),
            (B2 == false -> 
                throw(error(wrong_latters_in, List, verify_coordinate_body/2))
            ;
                Return_bool = true
            )
        )
    ;
        throw(error(wrong_input_list, List, verify_coordinate_body/2))
    ).

/*Convert the sign of the coordinate into a number for the decimal conversion the coordinate.
 * Input: A character.
 * Output: An integer number.*/
check_sign(Character, Return_num) :-
    (nonvar(Character) ->
       (Character == 'S' ->
            Return_num = (-1)
        ; 
            (Character == 'W' -> 
                Return_num = (-1)
            ;
                Return_num = 1
            )
        )
    ;
        throw(error(no_input_character, check_sign/2))
    ).

/*Convert a Coordinate in D.M.G. form into Decimal form.
 * Input: A list containing a coordinate.
 * Output: An integer number containing the coordinate in decimal form.*/
convert_to_decimal([], _) :-
    throw(error(empty_input_list, convert_to_decimal/2)).
convert_to_decimal(List, Return_num) :-
    (list(List) -> 
        index(0, List, Sign),
        check_sign(Sign, Sign1),
        index(1, List, Degrees),
        index(2, List, Primes),
        index(3, List, Latters),
        A is Latters / 60,
        B is Primes + A,
        C is B / 60,
        D is C + Degrees,
        Return_num is D * Sign1
    ;
        throw(error(wrong_input_list, List, convert_to_decimal/2))
    ).
    
/*Merge two numbers into a list that contain both.
 * Input: two integer/float numbers.
 * Output: A list containing both the input numbers.*/
merge_coordinates(Num1, Num2, Final_list) :-
    (number(Num1) -> 
        (number(Num2) -> 
            Final_list = [Num1, Num2]
        ;
            throw(error(wrong_input_second_number, Num2, merge_coordinates/3))
        )
    ;
        throw(error(wrong_input_first_number, Num1, merge_coordinates/3))
    ).

/*Convert a detection (in D.M.G. form) into decimal form.
 * Input: A list containing a detection.
 * Output: A list containing the detection in decimal form.*/
get_point([], _) :-
    throw(error(empty_input_list, get_point/2)).
get_point(List, Final_list) :-
    (list(List) -> 
        get_latitude(List, Latitude),
        index(0, Latitude, Sign1),
        verify_lat_sign(Sign1, B1),
        index(1, Latitude, Deg1),
        verify_lat_degrees(Deg1, B2),
        verify_coordinate_body(Latitude, _),
        get_longitude(List, Longitude),
        index(0, Longitude, Sign2),
        verify_long_sign(Sign2, B3),
        index(1, Longitude, Deg2),
        verify_long_degrees(Deg2, B4),
        verify_coordinate_body(Longitude, _),
        (B1 == false ->
            throw(error(wrong_sign_in, Latitude, get_point/2))
        ;
        	(B2 == false -> 
        		throw(error(wrong_degrees_in, Latitude, get_point/2))
        	;
        		(B3 == false ->
                	throw(error(wrong_sign_in, Longitude, get_point/2))
            	;
            		(B4 == false ->
            			throw(error(wrong_degrees_in, Longitude, get_point/2))
            		;
            			convert_to_decimal(Latitude, Dlatitude),
                		convert_to_decimal(Longitude, Dlongitude),
                		merge_coordinates(Dlatitude, Dlongitude, Final_list)
            		)
            	)
        	)
        )
    ;
        throw(error(wrong_input_list, List, get_point/2))
    ).
    
/***** End module. *****/