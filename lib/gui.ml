open Owl

module type OwlGraphType = sig
  val input_csv : string -> string list list
  (** Read a csv file with by calling read_csv in csv_reader.ml *)

  val candle_graph : string list list -> Unit
  (** Given a csv [csv], return the candle box plot graph showing the high, low, 
      open, and close prices. *)

  val five_day_graph : string list list -> Unit
  (** Given a csv [csv], return the 5 day moving average line using the close 
      price to give an average value to the 5 day period. *)

  val fifty_day_graph : string list list -> Unit
  (** Given a csv [csv], return the 50 day moving average line using the close 
      price to give an average value to the 50 day period. *)
  
  val overlay_graph : string list list -> Unit
  (** Given a csv [csv], return an overlayed graph of 3 graphs: candle, 5 day 
      moving average, and 50 day moving average graphs. *)
end

(** This module contains functions for taking in a CSV file and producing 
    multiple graphs based on the data, including a candle box, 5-day moving 
    average, and 50-day moving average graphs. *)
module OwlGraph : OwlGraphType = struct
  let input_csv (filename : string) : string list list =
    csv_reader read_csv filename

  let candle_graph (data : string list list) =
    let y2 = Mat.uniform 10 100 in
    let h = Plot.create ~m:1 ~n:2 "" in 
    Plot.(boxplot ~h ~spec:[ RGB (0,153,51) ] y2);
    Plot.output h;;

  let five_day_graph (data : string list list) =
    let y2 = Mat.uniform 10 100 in
    let h = Plot.create ~m:1 ~n:2 "" in 
    Plot.(boxplot ~h ~spec:[ RGB (0,153,51) ] y2);
    Plot.output h;;
end
