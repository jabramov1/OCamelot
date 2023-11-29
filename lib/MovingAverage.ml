open CsvReader

module type MovingAverageType = sig
  val simple_moving_avg : CsvReader.t -> int -> float option list
  val exp_moving_avg : CsvReader.t -> int -> float option list
  val weighted_moving_avg : unit
  val triangular_moving_avg : unit
  val variable_moving_avg : unit
  val mean : unit
end

module MovingAverage : MovingAverageType = struct
  let rec take n prices =
    match (prices, n) with
    | [], _ -> []
    | h :: _, 1 -> [ h ]
    | h :: t, n -> h :: take (n - 1) t

  let gen_windows data size =
    let prices = CsvReader.get_closing_prices data in
    let rec helper prices size =
      let len = List.length prices in
      if len < size then []
      else
        match prices with
        | [] -> []
        | _ :: t ->
            if len = size then [ prices ] else take size prices :: helper t size
    in
    helper prices size

  let valid_prices prices =
    List.filter
      (fun price -> if Option.is_some price then true else false)
      prices

  let window_sma window =
    let valid_prices = valid_prices window in
    if List.length valid_prices = 0 then None
    else
      let rec sum prices acc =
        match prices with
        | [] -> acc
        | Some price :: t -> sum t (acc +. price)
        | _ -> failwith "Impossible."
      in
      let sma =
        sum valid_prices 0. /. (List.length valid_prices |> float_of_int)
      in
      Some sma

  let simple_moving_avg data size =
    if size <= 0 then []
    else
      let windows = gen_windows data size in
      List.fold_left (fun acc window -> window_sma window :: acc) [] windows
      |> List.rev

  let exp_moving_avg = failwith "Unimplemented"
  let weighted_moving_avg = failwith "Unimplemented"
  let triangular_moving_avg = failwith "Unimplemented"
  let variable_moving_avg = failwith "Unimplemented"
  let mean = failwith "Unimplemented"
end
