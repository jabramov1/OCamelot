(** Manages conversions from strings to date floats and vice-versa. *)

exception InvalidDate of string

val string_to_date : date_type:string -> string -> float
(** Given a string [s] formatted according to [date_type],
    [string_to_date date_type s] converts [s] into a floating-point
    representation consistent with date-time rfc-3339 standards.

    The currently supported date types are:
    - ["YYYY-MM-DD"]
    - ["YYYY-DD-MM"]
    - ["DD-MM-YYYY"]
    - ["MM-DD-YYYY"]
    - ["MMM DD, YYYY"] (ex: "Dec 04, 2024" -> ["MMM DD, YYYY"])

    The dashes can also be replaced with slashes. Shorthand representation for
    years, like 23 for 2023, can be represented if YY is used instead of YYYY.
    Single digit days and months are also acceptable (ex: 9/2/2023 ->
    ["MM-DD-YYYY"]). *)

val date_to_string : float -> string
(** Given a float [f] consistent with the date-time rfc-3339 standards,
    [date_to_string f] converts f into the string representation with the format
    YYYY-MM-DD (ex: 2004-12-30). *)
