exception InvalidDate of string

val string_to_date : date_type:string -> string -> float

val date_to_string : float -> string
(**The supported date types are: "YYYY-MM-DD", "YYYY-DD-MM", "DD-MM-YYYY",
   "MM-DD-YYYY", and "MMM DD, YYYY" (ex: "Dec 04, 2024" -> "MMM DD, YYYY")
   Slashes instead of the dashes also represent valid date types. Two-digit
   years, like 23 for 2023 can be represented if YY is used instead of YYYY.
   Single digit days and months are also acceptable (ex: 9/2/2023 ->
   "MM-DD-YYYY"*)
