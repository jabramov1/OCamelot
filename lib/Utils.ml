let string_of_opt f s =
  Option.map (fun x -> "Some " ^ f x) s |> Option.value ~default:"None"

let string_of_float_opt n = string_of_opt string_of_float n

let string_of_list f lst =
  "[ " ^ List.fold_left (fun acc elem -> acc ^ f elem ^ "; ") "" lst ^ "]"

let float_of_int_opt n = Option.map float_of_int n
let format_float ?(prec = 3) f = Printf.sprintf "%.*f" prec f

let format_float_opt ?(prec = 3) n =
  Option.map (fun x -> format_float ~prec x |> float_of_string) n
