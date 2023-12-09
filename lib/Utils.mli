val string_of_opt : ('a -> string) -> 'a option -> string
(** [string_of_opt f s] converts a value of type ['a option] into its string
    representation. If the input value [s] is [Some x], [string_of_opt] applies
    function [f] to [x] to obtain the string representation. Otherwise, if the
    input is [None], it returns "None" as the string representation.

    @param f A function that converts a value of type ['a] to a string.

    @param s The optional value that is converted. *)

val string_of_float_opt : float option -> string
(** [string_of_float_opt n] converts a float option [n] to its string
    representation. If [n] is [Some x], [string_of_float_opt] converts [x] to a
    string. Otherwise, if the input is [None], "None" is returned as the string
    representation. *)

val string_of_list : ('a -> string) -> 'a list -> string
(** [string_of_list f lst] converts a list [n] of type ['a list] into its string
    representation.

    Example output for list [[1; 2; 3]]: "[1; 2; 3; ]"

    @param f A function that converts a value of type ['a] to a string.

    @param lst The list that is converted. *)

val float_of_int_opt : int option -> float option
(** [float_of_int_opt n] converts an integer option [n] into a float option. If
    [n] is [Some x], [float_of_int_opt] converts [x] to a float and returns it
    wrapped in [Some]. Otherwise, if the input is [None], [float_of_int_opt]
    returns [None]. *)

val format_float : ?prec:int -> float -> string
(** [format_float ?prec:precision n] formats a float [n] as a string
    representation with an optional precision [precision].

    @param ?prec
      An optional parameter specifying the precision of the output. Default
      precision is used if not provided.

    @param n The float value that is formatted. *)

val format_float_opt : ?prec:int -> float option -> float option
(** [format_float_opt ?prec:precision n] formats a float option [n] as a string
    representation with an optional precision [precision]. If [n] is [Some x],
    [format_float_opt] converts [x] to a formatted string representation using
    the provided precision. Otherwise, if the input is [None],
    [format_float_opt] returns [None].

    @param ?prec
      An optional parameter specifying the precision of the output. Default
      precision is used if not provided.

    @param n The optional float value that is formatted. *)
