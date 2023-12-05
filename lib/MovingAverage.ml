open CsvReader

module type MovingAverageType = sig
  val simple_moving_avg : CsvReader.t -> int -> float option list
  val exp_moving_avg : CsvReader.t -> int -> float option list
  val weighted_moving_avg : unit
  val triangular_moving_avg : unit
  val variable_moving_avg : unit
end

module MovingAverage = struct
  let rec take n prices =
    match (prices, n) with
    | [], _ -> []
    | h :: _, 1 -> [ h ]
    | h :: t, n -> h :: take (n - 1) t

  let rec divide_windows size prices =
    let len = List.length prices in
    if len < size then []
    else
      match prices with
      | [] -> []
      | _ :: t ->
          if len = size then [ prices ]
          else take size prices :: divide_windows size t

  let gen_windows data size =
    CsvReader.get_closing_prices data |> divide_windows size

  let rec valid_prices prices =
    let rec helper prices acc =
      match prices with
      | [] -> acc |> List.rev
      | None :: t -> helper t acc
      | Some price :: t -> helper t (price :: acc)
    in
    helper prices []

  let rec sum prices acc =
    match prices with
    | [] -> acc
    | price :: t -> sum t (acc +. price)

  let single_sma window =
    let valid_prices = valid_prices window in
    if List.length valid_prices = 0 then None
    else
      let sma =
        sum valid_prices 0. /. (List.length valid_prices |> float_of_int)
      in
      Some sma

  let simple_moving_avg data size =
    if size <= 0 then []
    else
      let windows = gen_windows data size in
      List.fold_left (fun acc window -> single_sma window :: acc) [] windows
      |> List.rev

  let single_ema window prev =
    let valid_prices = valid_prices window in
    if List.length valid_prices = 0 then None
    else
      let n = List.length window in
      let alpha = 2. /. float_of_int (n + 1) in
      let rec helper prices prev acc =
        match prices with
        | [] -> acc
        | h :: t ->
            let curr_ema = (h *. alpha) +. (prev *. (1. -. alpha)) in
            helper t curr_ema (curr_ema +. acc)
      in
      Some (helper valid_prices prev 0.)

  let exp_moving_avg data n =
    if n <= 0 then []
    else
      let windows = gen_windows data n in
      List.fold_left
        (fun acc window ->
          let init_ema = single_sma window in
          begin
            match init_ema with
            | None -> None :: acc
            | Some init_ema ->
                let ema = single_ema window init_ema in
                ema :: acc
          end)
        [] windows
      |> List.rev

  let single_wma window =
    let valid_prices = valid_prices window in
    if List.length valid_prices = 0 then None
    else
      let n = List.length valid_prices |> float_of_int in
      let den = n *. (n +. 1.) /. 2. in
      let rec w_sum prices n acc =
        match prices with
        | [] -> acc
        | h :: t ->
            let new_acc = (h *. n) +. acc in
            w_sum t (n -. 1.) new_acc
      in
      let sum = w_sum valid_prices n 0. in
      Some (sum /. den)

  let weighted_moving_avg data n =
    if n <= 0 then []
    else
      let windows = gen_windows data n in
      List.fold_left (fun acc window -> single_wma window :: acc) [] windows
      |> List.rev

  let triangular_moving_avg = failwith "Unimplemented"
  let variable_moving_avg = failwith "Unimplemented"
end
