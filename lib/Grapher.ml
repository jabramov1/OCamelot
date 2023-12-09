open CsvReader
open MovingAverage
module Gp = Gnuplot

module type GrapherType = sig
  val graph :
    ?m_avg:((CsvReader.t -> int -> float option list) * int) list ->
    CsvReader.t ->
    unit
end

module Grapher = struct
  let convert_data data =
    let convert_row row =
      let date = CsvReader.get_date row in
      let op = Option.value (CsvReader.get_open_price row) ~default:0. in
      let hi = Option.value (CsvReader.get_high_price row) ~default:0. in
      let lo = Option.value (CsvReader.get_low_price row) ~default:0. in
      let cl = Option.value (CsvReader.get_closing_price row) ~default:0. in
      (date, (op, hi, lo, cl))
    in
    List.map convert_row data

  let rec tuple_lists l1 l2 =
    if List.length l1 > List.length l2 then
      match l1 with
      | _ :: t -> tuple_lists t l2
      | _ -> failwith "error"
    else if List.length l1 < List.length l2 then
      match l2 with
      | _ :: t -> tuple_lists l1 t
      | _ -> failwith "error"
    else
      match (l1, l2) with
      | h1 :: t1, h2 :: t2 -> (h1, h2) :: tuple_lists t1 t2
      | _ -> []

  let filter_float_list (data : float option list) : float list =
    List.filter_map (fun x -> x) data

  let graph ?(m_avg = [ (MovingAverage.simple_moving_avg, 5) ]) data =
    let plot_data = convert_data data in
    let dates = List.map fst plot_data in
    Printf.printf "Number of Dates: %d\n" (List.length dates);
    List.iter (fun d -> Printf.printf "Date: %f\n" d) dates;

    let moving_avg_fun, period = List.hd m_avg in
    let moving_avg_data = filter_float_list (moving_avg_fun data period) in
    Printf.printf "Number of Moving Average Points: %d\n"
      (List.length moving_avg_data);
    List.iter
      (fun ma -> Printf.printf "Moving Average: %f\n" ma)
      moving_avg_data;

    let data_ma = tuple_lists dates moving_avg_data in
    Printf.printf "Number of Combined Points: %d\n" (List.length data_ma);
    (* List.iter (fun (d, ma) -> Printf.printf "Combined Point: %f, %f\n" d ma)
       data_ma; *)
    if List.length data_ma = 0 then
      Printf.printf "Error: No data points to plot.\n"
    else
      let line_s = Gp.Series.lines_datey ~color:`Magenta data_ma in
      let candle_data = Gp.Series.candles_date_ohlc ~color:`Blue plot_data in
      let gp = Gp.create () in

      Gp.plot_many gp
        ~range:(Gp.Date (List.hd dates, List.hd (List.rev dates)))
        ~format:"%b %d'%y" [ candle_data; line_s ];

      Unix.sleep 1000
end
