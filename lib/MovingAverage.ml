open CsvReader

module type MovingAverageType = sig
  val simple_moving_avg : CsvReader.t -> int -> float option list
  val exp_moving_avg : CsvReader.t -> int -> float option list
  val weighted_moving_avg : CsvReader.t -> int -> float option list
  val triangular_moving_avg : CsvReader.t -> int -> float option list
  val vol_adj_moving_avg : CsvReader.t -> int -> float option list
end

module MovingAverage : MovingAverageType = struct
  (** Given an integer [n] and a list of items [l], produce a list containing
      elements of the list [l] up to index [n]. If [n] is bigger than [l],
      return the whole list. If [n < 0], return the empty list. *)
  let rec take n prices =
    match (prices, n) with
    | [], _ -> []
    | _, n when n < 0 -> []
    | h :: _, 1 -> [ h ]
    | h :: t, n -> h :: take (n - 1) t

  (** Given a positive integer [n] and a list [prices], produce a list of all
      contiguous sublists of length [n] of the input list. If [n] is bigger than
      [l], return the original [prices] list. If [n < 0] then return the empty
      list. *)
  let rec divide_windows n prices =
    let len = List.length prices in
    if len < n then []
    else
      match prices with
      | [] -> []
      | _ :: t ->
          if len = n then [ prices ] else take n prices :: divide_windows n t

  (** Given a CSV data representation [data], extract the closing prices and
      divide them into a list of all contiguous sublists of length [n] using
      [divide_windows]. *)
  let price_windows data n =
    CsvReader.get_closing_prices data |> divide_windows n

  (** Given a CSV data representation [data], extract the volumes and divide
      them into a list of all contiguous sublists of length [n] using
      [divide_windows]. *)
  let volume_windows data n = CsvReader.get_volumes data |> divide_windows n

  (** Given a list of optionals [entries], return a version of [entries]
      containing all the values [x] in [Some x], removing any values of [None]. *)
  let valid_entries entries =
    let rec helper entries acc =
      match entries with
      | [] -> acc |> List.rev
      | None :: t -> helper t acc
      | Some price :: t -> helper t (price :: acc)
    in
    helper entries []

  (** [sum lst] returns the sum of all the elements in [lst], where the elements
      of [lst] are floats. *)
  let sum lst =
    let rec helper lst acc =
      match lst with
      | [] -> acc
      | entry :: t -> helper t (acc +. entry)
    in
    helper lst 0.

  (** [single_sma] calculates the simple moving average for a singular window of
      time. *)
  let single_sma window =
    let valid_prices = valid_entries window in
    if List.length valid_prices = 0 then None
    else
      let sma =
        sum valid_prices /. (List.length valid_prices |> float_of_int)
      in
      Some sma

  let simple_moving_avg data n =
    let n = min n (CsvReader.get_closing_prices data |> List.length) in
    if n <= 0 then []
    else
      let windows = price_windows data n in
      List.fold_left (fun acc window -> single_sma window :: acc) [] windows
      |> List.rev

  (** [single_ema] calculates the exponential moving average for a singular
      window of time.

      @param prev
        Represents the EMA for the previous window. Use the SMA for the current
        window if there is no previous window.

      @param multiplier
        Smoothing constant calculated by [2 / (n + 1)], where n is the length of
        the window. *)
  let single_ema window prev multiplier =
    let w_n = List.length window in
    let curr_price = List.nth window (w_n - 1) in
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

  (** [single_wma] calculates the weighted moving average for a single window in
      time. *)
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
      let n = (n / 2) + 1 in
      let n = min n (CsvReader.get_closing_prices data |> List.length) in
      let windows = price_windows data n in
      let sma =
        List.fold_left (fun acc window -> single_sma window :: acc) [] windows
        |> List.rev
      in
      let new_n = min (n - 1) (List.length sma) in
      let new_windows = divide_windows new_n sma in
      List.fold_left (fun acc window -> single_sma window :: acc) [] new_windows
      |> List.rev
    else
      let n = (n + 1) / 2 in
      let n = min n (CsvReader.get_closing_prices data |> List.length) in
      let windows = price_windows data n in
      let sma =
        List.fold_left (fun acc window -> single_sma window :: acc) [] windows
        |> List.rev
      in
      let new_n = min n (List.length sma) in
      let new_windows = divide_windows new_n sma in
      List.fold_left (fun acc window -> single_sma window :: acc) [] new_windows
      |> List.rev

  (** [single_vama] calculates the volume adjusted weighted moving average for a
      singular window of time.

      @param p_wind
        A window containing closing prices. Each entry at index [i] of [p_wind]
        corresponds to the volume at index [i] of [v_wind].
      @param v_wind
        A window containing volumes. Each entry at index [i] in [v_wind]
        corresponds to the closing price at index [i] of [p_wind]. *)
  let single_vama p_wind v_wind =
    let valid_prices = valid_entries p_wind in
    let valid_volumes = valid_entries v_wind in
    if List.length valid_prices = 0 || List.length valid_volumes = 0 then None
    else
      let den = sum valid_volumes in
      let rec calculate_vama p_wind v_wind acc =
        match (p_wind, v_wind) with
        | [], [] -> acc /. den
        | None :: t1, _ :: t2 | _ :: t1, None :: t2 -> calculate_vama t1 t2 acc
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
        volume_windows data n |> List.map (List.map Utils.float_of_int_opt)
      in
      List.fold_left2
        (fun acc p_wind v_wind -> single_vama p_wind v_wind :: acc)
        [] p_windows v_windows
      |> List.rev
end
