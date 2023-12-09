exception InvalidDate of string

let format_day_month n =
  let len = String.length n in
  match len with
  | 1 -> "0" ^ n
  | 2 -> n
  | _ -> raise (InvalidDate "Invalid date. Component is not of proper length.")

let month_to_number month =
  let month = String.lowercase_ascii month |> String.trim in
  let month_map =
    [
      ("jan", "01");
      ("feb", "02");
      ("mar", "03");
      ("apr", "04");
      ("may", "05");
      ("jun", "06");
      ("jul", "07");
      ("aug", "08");
      ("sep", "09");
      ("oct", "10");
      ("nov", "11");
      ("dec", "12");
    ]
  in
  let month_num = List.assoc_opt month month_map in
  match month_num with
  | Some n -> n
  | None -> raise (InvalidDate "Invalid date input.")

let split_date delim date =
  let date = String.split_on_char ' ' date |> List.hd in
  String.trim date |> String.split_on_char delim

let reconstruct year month day = year ^ "-" ^ month ^ "-" ^ day |> String.trim

let parse_date ~date_type date =
  let split_dash = split_date '-' date in
  let split_slash = split_date '/' date in

  match date_type with
  | "YYYY-MM-DD" -> date
  | "YY-MM-DD" -> "20" ^ date
  | "YYYY/MM/DD" | "YY/MM/DD" ->
      let year = List.nth split_slash 0 in
      let new_date =
        reconstruct year
          (List.nth split_slash 1 |> format_day_month)
          (List.nth split_slash 2 |> format_day_month)
      in
      if String.length year = 2 then "20" ^ new_date else new_date
  | "MM-DD-YYYY" ->
      reconstruct (List.nth split_dash 2)
        (List.nth split_dash 0 |> format_day_month)
        (List.nth split_dash 1 |> format_day_month)
  | "MM/DD/YY" ->
      "20"
      ^ reconstruct (List.nth split_slash 2)
          (List.nth split_slash 0 |> format_day_month)
          (List.nth split_slash 1 |> format_day_month)
  | "MM/DD/YYYY" ->
      reconstruct (List.nth split_slash 2)
        (List.nth split_slash 0 |> format_day_month)
        (List.nth split_slash 1 |> format_day_month)
  | "DD-MM-YYYY" ->
      reconstruct (List.nth split_dash 2)
        (List.nth split_dash 1 |> format_day_month)
        (List.nth split_dash 0 |> format_day_month)
  | "DD/MM/YYYY" ->
      reconstruct (List.nth split_slash 2)
        (List.nth split_slash 1 |> format_day_month)
        (List.nth split_slash 0 |> format_day_month)
  | "YYYY-DD-MM" ->
      reconstruct (List.nth split_dash 0)
        (List.nth split_dash 2 |> format_day_month)
        (List.nth split_dash 1 |> format_day_month)
  | "YYYY/DD/MM" ->
      reconstruct (List.nth split_slash 0)
        (List.nth split_slash 2 |> format_day_month)
        (List.nth split_slash 1 |> format_day_month)
  | "MMM DD, YYYY" ->
      let split_comma = String.split_on_char ',' date in
      let split_space = String.split_on_char ' ' (List.hd split_comma) in
      let month = List.hd split_space in
      let day = List.nth split_space 1 |> format_day_month in
      let year = List.nth split_comma 1 in
      reconstruct year (month_to_number month) day
  | _ -> raise (InvalidDate "Unsupported date format.")

let string_to_date ~date_type date =
  let date = parse_date ~date_type date ^ "T00:00:00Z" in
  match Ptime.of_rfc3339 date with
  | Ok (time, _, _) -> Ptime.to_float_s time
  | Error _ -> raise (InvalidDate ("Invalid date: " ^ date))

let date_to_string date =
  let time = Ptime.of_float_s date in
  match time with
  | Some t ->
      let (year, month, day), _ = Ptime.to_date_time t in
      Printf.sprintf "%04d-%02d-%02d" year month day
  | None -> raise (InvalidDate "Invalid date being converted.")
