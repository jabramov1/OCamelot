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

  let graph ?(m_avg = [ (MovingAverage.simple_moving_avg, 0) ]) data =
    print_endline (List.nth m_avg 0 |> snd |> string_of_int);
    let plot_data = convert_data data in
    let dates = List.map fst plot_data in
    let start = List.hd dates in
    let stop = List.hd (List.rev dates) in
    let candle_data = Gp.Series.candles_date_ohlc plot_data in
    let gp = Gp.create () in
    Gp.plot gp
      ~range:(Gp.Date (start -. (3600. *. 24.), stop +. (3600. *. 24.)))
      ~format:"%b %d'%y" candle_data;
    Unix.sleep 1000
end
