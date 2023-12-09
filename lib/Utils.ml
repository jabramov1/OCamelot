(* let rec string_to_float s index len result = if index = len then result else
   let c = Char.code s.[index] in if c >= 48 && c <= 57 then (* ASCII code for
   numbers 0-9 *) let numeric_val = float_of_int (c - 48) in let new_result =
   (result *. 10.) +. numeric_val in string_to_float s (index + 1) len
   new_result else invalid_arg "Non-numeric character in string" *)

(* let float_val = string_to_float clean_str 0 (String.length clean_str) 0.0 *)

let string_of_opt f s =
  Option.map (fun x -> "Some " ^ f x) s |> Option.value ~default:"None"

let string_of_float_opt n = string_of_opt string_of_float n

let string_of_list f lst =
  "[ " ^ List.fold_left (fun acc elem -> acc ^ f elem ^ "; ") "" lst ^ "]"

let float_of_int_opt n = Option.map float_of_int n
let format_float ?(prec = 3) f = Printf.sprintf "%.*f" prec f

let format_float_opt ?(prec = 3) n =
  Option.map (fun x -> format_float ~prec x |> float_of_string) n
