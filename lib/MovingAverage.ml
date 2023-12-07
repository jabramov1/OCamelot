open CsvReader
(** https://www.fmlabs.com/reference/default.htm?url=VariableMA.htm *)

module type MovingAverageType = sig
  val simple_moving_avg : CsvReader.t -> int -> float option list
  val exp_moving_avg : CsvReader.t -> int -> float option list
  val weighted_moving_avg : CsvReader.t -> int -> float option list
  val triangular_moving_avg : CsvReader.t -> int -> float option list
  val vol_adj_moving_avg : CsvReader.t -> int -> float option list
end

module MovingAverage : MovingAverageType = struct
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

  let float_of_int_opt n =
    match n with
    | None -> None
    | Some n -> Some (float_of_int n)

  let price_windows data size =
    CsvReader.get_closing_prices data |> divide_windows size

  let volume_windows data size =
    CsvReader.get_volumes data |> divide_windows size

  let valid_entries entries =
    let rec helper entries acc =
      match entries with
      | [] -> acc |> List.rev
      | None :: t -> helper t acc
      | Some price :: t -> helper t (price :: acc)
    in
    helper entries []

  let sum entries =
    let rec helper entries acc =
      match entries with
      | [] -> acc
      | entry :: t -> helper t (acc +. entry)
    in
    helper entries 0.

  let single_sma window =
    let valid_prices = valid_entries window in
    if List.length valid_prices = 0 then None
    else
      let sma =
        sum valid_prices /. (List.length valid_prices |> float_of_int)
      in
      Some sma

  let simple_moving_avg data size =
    let size = min size (CsvReader.get_closing_prices data |> List.length) in
    if size <= 0 then []
    else
      let windows = price_windows data size in
      List.fold_left (fun acc window -> single_sma window :: acc) [] windows
      |> List.rev

  let single_ema window prev multiplier =
    let w_size = List.length window in
    let curr_price = List.nth window (w_size - 1) in
    match curr_price with
    | None -> None
    | Some price ->
        let ema = (price *. multiplier) +. (prev *. (1. -. multiplier)) in
        Some ema

  let exp_moving_avg data n =
    let n = min n (CsvReader.get_closing_prices data |> List.length) in
    if n <= 0 then []
    else
      let windows = price_windows data n in
      let multiplier = 2. /. (float_of_int n +. 1.) in
      let init_ema = List.hd windows |> single_sma in

      let rec calculate_ema windows prev multiplier acc =
        match windows with
        | [] -> List.rev acc
        | h :: t ->
            let ema = single_ema h prev multiplier in
            if Option.is_some ema then
              calculate_ema t
                (Option.value ema ~default:0.)
                multiplier (ema :: acc)
            else calculate_ema t prev multiplier (ema :: acc)
      in
      if Option.is_none init_ema then []
      else
        calculate_ema windows (Option.value init_ema ~default:0.) multiplier []

  let single_wma window =
    let valid_prices = valid_entries window in
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
    let n = min n (CsvReader.get_closing_prices data |> List.length) in
    if n <= 0 then []
    else
      let windows = price_windows data n in
      List.fold_left (fun acc window -> single_wma window :: acc) [] windows
      |> List.rev

  let triangular_moving_avg data n =
    if n <= 0 then []
    else if n mod 2 = 0 then
      let size = (n / 2) + 1 in
      let size = min size (CsvReader.get_closing_prices data |> List.length) in
      let windows = price_windows data size in
      let sma =
        List.fold_left (fun acc window -> single_sma window :: acc) [] windows
        |> List.rev
      in
      let new_size = min (size - 1) (List.length sma) in
      let new_windows = divide_windows new_size sma in
      List.fold_left (fun acc window -> single_sma window :: acc) [] new_windows
      |> List.rev
    else
      let size = (n + 1) / 2 in
      let size = min size (CsvReader.get_closing_prices data |> List.length) in
      let windows = price_windows data size in
      let sma =
        List.fold_left (fun acc window -> single_sma window :: acc) [] windows
        |> List.rev
      in
      let new_size = min size (List.length sma) in
      let new_windows = divide_windows new_size sma in
      List.fold_left (fun acc window -> single_sma window :: acc) [] new_windows
      |> List.rev

  let single_vama p_wind v_wind =
    let valid_prices = valid_entries p_wind in
    let valid_volumes = valid_entries v_wind in
    if List.length valid_prices = 0 || List.length valid_volumes = 0 then None
    else
      let den = sum valid_volumes in
      let rec calculate_vama p_wind v_wind acc =
        match (p_wind, v_wind) with
        | [], [] -> acc /. den
        | Some p :: t1, Some v :: t2 -> calculate_vama t1 t2 ((p *. v) +. acc)
        | _ -> failwith "Invalid input."
      in
      let vama = calculate_vama p_wind v_wind 0. in
      Some vama

  let vol_adj_moving_avg data n =
    let n = min n (CsvReader.get_closing_prices data |> List.length) in
    if n <= 0 then []
    else
      let p_windows = price_windows data n in
      let v_windows =
        volume_windows data n |> List.map (List.map float_of_int_opt)
      in
      List.fold_left2
        (fun acc p_wind v_wind -> single_vama p_wind v_wind :: acc)
        [] p_windows v_windows
      |> List.rev
end
