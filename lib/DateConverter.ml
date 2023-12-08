module type DateConverterType = sig
  exception InvalidDate of string

  val string_to_date : string -> float
  val date_to_string : float -> string
end

module DateConverter : DateConverterType = struct
  exception InvalidDate of string

  let format_component comp len =
    let comp' = String.trim comp in
    if String.length comp' = len then comp'
    else
      raise
        (InvalidDate
           ("Unsupported date format. Component " ^ comp
          ^ " has an invalid length."))

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

  let split_date delim date = String.split_on_char delim date
  let reconstruct month day year = month ^ "-" ^ day ^ "-" ^ year

  let parse_date date date_type =
    let split_dash = split_date '-' date in
    let split_slash = split_date '/' date in
    let split_comma = split_date ',' date in
    match date_type with
    | "MM-DD-YYYY" -> date
    | "MM/DD/YYYY" ->
        reconstruct (List.nth split_slash 0) (List.nth split_slash 1)
          (List.nth split_slash 2)
    | "DD-MM-YYYY" ->
        reconstruct (List.nth split_dash 1) (List.nth split_dash 0)
          (List.nth split_dash 2)
    | "DD/MM/YYYY" ->
        reconstruct (List.nth split_slash 1) (List.nth split_slash 0)
          (List.nth split_slash 2)
    | "YYYY-DD-MM" ->
        reconstruct (List.nth split_dash 2) (List.nth split_dash 1)
          (List.nth split_dash 0)
    | "YYYY/DD/MM" ->
        reconstruct (List.nth split_slash 2) (List.nth split_slash 1)
          (List.nth split_slash 0)
    | "YYYY-MM-DD" ->
        reconstruct (List.nth split_dash 1) (List.nth split_dash 2)
          (List.nth split_dash 0)
    | "YYYY/MM/DD" ->
        reconstruct (List.nth split_slash 1) (List.nth split_slash 2)
          (List.nth split_slash 0)
    | "MMM DD, YYYY" ->
        let split = split_date ' ' (List.hd split_comma) in
        let month = List.hd split in
        let day = List.nth split 1 in
        let year = List.nth split_comma 1 in
        reconstruct (month_to_number month) day year
    | _ -> raise (InvalidDate "Unsupported date format.")

  let string_to_date date_str =
    let date = date_str ^ "T00:00:00Z" in
    match Ptime.of_rfc3339 date with
    | Ok (time, _, _) -> Ptime.to_float_s time
    | Error _ -> raise (InvalidDate ("Invalid date: " ^ date))

  let date_to_string date =
    let time = Ptime.of_float_s date in
    match time with
    | Some t ->
        let (year, month, day), ((hour, minute, second), _) =
          Ptime.to_date_time t
        in
        Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" year month day hour
          minute second
    | None -> failwith "Invalid Unix timestamp"
end
