module Gp = Gnuplot

module type OwlGraphType = sig
  val input_csv : string -> string list list
  (** Read a csv file with by calling read_csv in csv_reader.ml *)

  val candle_graph : string list list -> unit
  (** Given a csv [csv], return the candle box plot graph showing the high, low,
      open, and close prices. *)
end

(** This module contains functions for taking in a CSV file and producing
    multiple graphs based on the data, including a candle box, 5-day moving
    average, and 50-day moving average graphs. *)
module OwlGraph : OwlGraphType = struct
  let input_csv (filename : string) : string list list =
    Csv_reader.CsvReader.read_csv filename

  (* Generate random candlestick bars. *)
  let gen_bars ~num_bars =
    let next_bar cl =
      let op = cl +. ((Random.float 1. -. 0.5) /. 2.) in
      let hi = op +. (Random.float 1. /. 5.) in
      let lo = op -. (Random.float 1. /. 5.) in
      let cl = (lo +. hi) /. 2. in
      (op, hi, lo, cl)
    in
    let rec loop n_bars bars ((_, _, _, cl) as bar) =
      if n_bars = 0 then bars else loop (n_bars - 1) (bar :: bars) (next_bar cl)
    in
    let op = 100. in
    let hi = op +. (Random.float 1. /. 5.) in
    let lo = op -. (Random.float 1. /. 5.) in
    let cl = (lo +. hi) /. 2. in
    List.rev (loop num_bars [] (op, hi, lo, cl))

  let gen_data ~range =
    Base.List.zip_exn range (gen_bars ~num_bars:(List.length range))

  let rec gen_range acc stop num_days =
    if num_days = 0 then (stop, stop :: acc)
    else gen_range (stop :: acc) (stop -. (3600. *. 24.)) (num_days - 1)

  let candle_graph (data : string list list) =
    let num_days = 100 in
    let stop = Unix.gettimeofday () in
    let start, date_range = gen_range [] stop num_days in
    let gp = Gp.create () in
    (* Plot a random candlestick chart. *)
    Gp.plot gp
      ~range:(Gp.Date (start -. (3600. *. 24.), stop +. (3600. *. 24.)))
      ~format:"%b %d'%y"
      (Gp.Series.candles_date_ohlc (gen_data ~range:date_range));
    Unix.sleep 10;
    Gp.close gp
end

(** OWL IMPLEMENTATION *)

(** Signature *)
(* val five_day_graph : string list list -> unit (** Given a csv [csv], return
   the 5 day moving average line using the close price to give an average value
   to the 5 day period. *)

   val fifty_day_graph : string list list -> unit (** Given a csv [csv], return
   the 50 day moving average line using the close price to give an average value
   to the 50 day period. *)

   val overlay_graph : string list list -> unit (** Given a csv [csv], return an
   overlayed graph of 3 graphs: candle, 5 day moving average, and 50 day moving
   average graphs. *) *)

(** Struct*)
(* let five_day_graph (data : string list list) = let y2 = Mat.uniform 10 100 in
   let h = Plot.create ~m:1 ~n:2 "" in Plot.(boxplot ~h ~spec:[ RGB (0, 153, 51)
   ] y2); Plot.output h

   let fifty_day_graph (data : string list list) = let y2 = Mat.uniform 10 100
   in let h = Plot.create ~m:1 ~n:2 "" in Plot.(boxplot ~h ~spec:[ RGB (0, 153,
   51) ] y2); Plot.output h

   let overlay_graph (data : string list list) = let y2 = Mat.uniform 10 100 in
   let h = Plot.create ~m:1 ~n:2 "" in Plot.(boxplot ~h ~spec:[ RGB (0, 153, 51)
   ] y2); Plot.output h *)
