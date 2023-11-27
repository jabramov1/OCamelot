open CsvReader

(** SMA for open/high etc.? *)

module type MovingAverageType = sig
  val simple_moving_avg : CsvReader.t -> int -> float list
  val exp_moving_avg : unit
  val weighted_moving_avg : unit
  val triangular_moving_avg : unit
  val variable_moving_avg : unit
  val mean : unit
end

module MovingAverages = struct
  let rec take n prices =
    match (prices, n) with
    | [], _ -> []
    | h :: _, 1 -> [ h ]
    | h :: t, n -> h :: take (n - 1) t

  let rec gen_windows size data =
    let prices = CsvReader.get_closing_prices data in
    let rec helper size prices =
      let len = List.length prices in
      if len < size then []
      else
        match prices with
        | [] -> []
        | _ :: t ->
            if len = size then [ prices ] else take size prices :: helper size t
    in
    helper size prices

  let valid_prices prices =
    List.filter
      (fun price -> if Option.is_some price then true else false)
      prices

  let window_sma window =
    let valid_prices = valid_prices window in
    let rec sum prices acc =
      match prices with
      | [] -> acc
      | Some price :: t -> sum t (acc +. price)
      | _ -> failwith "Impossible."
    in
    sum valid_prices 0. /. (List.length valid_prices |> float_of_int)

  let simple_moving_avg size data =
    let windows = gen_windows size data in
    List.fold_left (fun acc window -> window_sma window :: acc) [] windows
    |> List.rev
end
