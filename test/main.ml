open Gnuplot
open Ocamelot.Csv_reader
open Ptime
module Gp = Gnuplot

(* Convert a date string in the format "YYYY-MM-DD" to a UNIX timestamp *)
let date_to_unix_timestamp date_str =
  match Ptime.of_rfc3339 date_str with
  | Ok (time, _, _) -> Ptime.to_float_s time
  | Error _ -> failwith ("Invalid date: " ^ date_str)

let () =
  (* Read data using CsvReader *)
  let filename = "SPY.csv" in
  let csv_data =
    CsvReader.read_csv filename ~date:"Date" ~open_price:"Open"
      ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
      ~adj_price:"Adj Close" ~volume:"Volume"
  in

  (* Convert CsvReader data to the format used by Gnuplot *)
  let convert_data row =
    (* Helper function to get the first item of a list *)
    let hd lst =
      match lst with
      | [] -> failwith "Expected non-empty list"
      | h :: _ -> h
    in

    (* Use the helper function to get the first item from the list returned by
       each getter *)
    let date = date_to_unix_timestamp (hd (CsvReader.get_dates [ row ])) in
    let op = float_of_string (hd (CsvReader.get_open_prices [ row ])) in
    let hi = float_of_string (hd (CsvReader.get_high_prices [ row ])) in
    let lo = float_of_string (hd (CsvReader.get_low_prices [ row ])) in
    let cl = float_of_string (hd (CsvReader.get_closing_prices [ row ])) in

    (date, (op, hi, lo, cl))
  in

  let plot_data = List.map convert_data csv_data in
  let dates = List.map fst plot_data in
  let start = List.hd dates in
  let stop = List.hd (List.rev dates) in

  let gp = Gp.create () in

  (* Plot the data *)
  Gp.plot gp
    ~range:(Gp.Date (start -. (3600. *. 24.), stop +. (3600. *. 24.)))
    ~format:"%b %d'%y"
    (Gp.Series.candles_date_ohlc plot_data);

  Unix.sleep 10;
  Gp.close gp
