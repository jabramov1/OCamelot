open Gnuplot
open OCamelot.Csv_reader
open Ptime

(* Convert a date string in the format "YYYY-MM-DD" to a UNIX timestamp *)
let date_to_unix_timestamp date_str =
  match Ptime.of_rfc3339 date_str with
  | Ok (time, _, _) -> Ptime.to_float_s time
  | Error _ -> failwith ("Invalid date: " ^ date_str)

let () =
  (* Read data using CsvReader *)
  let filename = "path_to_your_data.csv" in
  let csv_data =
    CsvReader.read_csv filename ~date:"Date" ~open_price:"Open"
      ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
      ~adj_price:"Adj Close" ~volume:"Volume"
  in

  (* Convert CsvReader data to the format used by Gnuplot *)
  let convert_data row =
    let date = date_to_unix_timestamp row.CsvReader.date in
    let op = float_of_string row.CsvReader.open_price in
    let hi = float_of_string row.CsvReader.high_price in
    let lo = float_of_string row.CsvReader.low_price in
    let cl = float_of_string row.CsvReader.close_price in
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
