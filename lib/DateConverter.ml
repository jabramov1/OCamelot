exception InvalidDate of string

(** Given a string [n] that represents a day or month, confirm that [n] contains
    two digits. If it does not, prepend ["0"] to [n]. *)
let format_day_month (n : string) : string =
  let len = String.length n in
  match len with
  | 1 -> "0" ^ n
  | 2 -> n
  | _ -> raise (InvalidDate "Invalid date. Component is not of proper length.")

(** [reorder_list lst indices] reorders [lst] based on [indices]. *)
let reorder_list (lst : 'a list) (indices : int list) : 'a list =
  [
    List.nth lst (List.nth indices 0);
    List.nth lst (List.nth indices 1);
    List.nth lst (List.nth indices 2);
  ]

(** [month_to_number m] takes a three-letter representation of a month [m] and
    converts it to its corresponding numerical month. *)
let month_to_number (month : string) : string =
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

(** [split_date delim date] extracts the date from a string, and splits it
    according to the [delim]*)
let split_date (delim : char) (date : string) : string list =
  let date = String.split_on_char ' ' date |> List.hd in
  String.trim date |> String.split_on_char delim

(** [reconstruct ymd] creates a standardized date representation in the format
    of "YEAR-MONTH-DAY". *)
let reconstruct (ymd : string list) : string =
  if List.length ymd != 3 then raise (InvalidDate "Invalid date.")
  else
    let year = List.nth ymd 0 in
    let month = List.nth ymd 1 |> format_day_month in
    let day = List.nth ymd 2 |> format_day_month in
    year ^ "-" ^ month ^ "-" ^ day |> String.trim

(** [parse_date ~date_type date] takes a string representing a date formatted
    according to [~date_type], converting it to the standardized format of
    "YEAR-MONTH-DAY" *)
let parse_date ~date_type date =
  let split_dash = split_date '-' date in
  let split_slash = split_date '/' date in

  match date_type with
  | "YYYY-MM-DD" -> date
  | "YY-MM-DD" -> "20" ^ date
  | "YYYY/MM/DD" | "YY/MM/DD" ->
      let new_date = reorder_list split_slash [ 0; 1; 2 ] in
      let year = List.hd new_date in
      let new_date = new_date |> reconstruct in
      if String.length year = 2 then "20" ^ new_date else new_date
  | "MM-DD-YYYY" | "MM-DD-YY" ->
      let new_date = reorder_list split_dash [ 2; 0; 1 ] in
      let year = List.hd new_date in
      let new_date = new_date |> reconstruct in
      if String.length year = 2 then "20" ^ new_date else new_date
  | "MM/DD/YYYY" | "MM/DD/YY" ->
      let new_date = reorder_list split_slash [ 2; 0; 1 ] in
      let year = List.hd new_date in
      let new_date = new_date |> reconstruct in
      if String.length year = 2 then "20" ^ new_date else new_date
  | "DD-MM-YYYY" | "DD-MM-YY" ->
      let new_date = reorder_list split_dash [ 2; 1; 0 ] in
      let year = List.hd new_date in
      let new_date = new_date |> reconstruct in
      if String.length year = 2 then "20" ^ new_date else new_date
  | "DD/MM/YYYY" | "DD/MM/YY" ->
      let new_date = reorder_list split_slash [ 2; 1; 0 ] in
      let year = List.hd new_date in
      let new_date = new_date |> reconstruct in
      if String.length year = 2 then "20" ^ new_date else new_date
  | "YYYY-DD-MM" | "YY-DD-MM" ->
      let new_date = reorder_list split_dash [ 0; 2; 1 ] in
      let year = List.hd new_date in
      let new_date = new_date |> reconstruct in
      if String.length year = 2 then "20" ^ new_date else new_date
  | "YYYY/DD/MM" | "YY/DD/MM" ->
      let new_date = reorder_list split_slash [ 0; 2; 1 ] in
      let year = List.hd new_date in
      let new_date = new_date |> reconstruct in
      if String.length year = 2 then "20" ^ new_date else new_date
  | "MMM DD, YYYY" ->
      let split_comma = String.split_on_char ',' date in
      let split_space = String.split_on_char ' ' (List.hd split_comma) in
      let month = List.hd split_space in
      let day = List.nth split_space 1 |> format_day_month in
      let year = List.nth split_comma 1 in
      reconstruct [ year; month_to_number month; day ]
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
