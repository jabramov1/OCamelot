module type DateConverterType = sig
  exception InvalidDate of string

  val string_to_date : string -> float
  val date_to_string : float -> string
end

module DateConverter : DateConverterType = struct
  exception InvalidDate of string

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
