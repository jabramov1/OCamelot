open Ocamelot
open Ocamelot.BackTester

(** OTHER CSV FILE OPTIONS:

    let csv_data = CsvReader.read_csv ~date:"Date" ~open_price:"open"
    ~high_price:"high" ~low_price:"low" ~close_price:"close" ~volume:"volume"
    ~date_type:"MM/DD/YYYY" "data/StockData.csv"

    let csv_data = CsvReader.read_csv ~date:" the date " ~open_price:"Open"
    ~high_price:"hi" ~low_price:"Low" ~close_price:" Close" ~volume:"vol1234 "
    "data/messy.csv" *)
let csv_data =
  CsvReader.read_csv ~date:"Date" ~open_price:"Open" ~high_price:"High"
    ~low_price:"Low" ~close_price:"Close" ~volume:"Volume" "./data/SPY.csv"

let demo data =
  (* Print size of CSV data. *)
  print_endline "Size:";
  let size = CsvReader.size data in
  print_endline (string_of_int size);

  (* Index a row and print that row. *)
  let row_index = 5 in
  let row = CsvReader.get_row data row_index in
  print_endline ("Row at index " ^ string_of_int row_index);
  CsvReader.print_row row;

  (* Get the head and print it. *)
  let head_size = 3 in
  print_endline "First 3 elements:";
  CsvReader.print_data (CsvReader.head data head_size);

  (* Get the tail and print it. *)
  let tail_size = 4 in
  print_endline "Last 4 elements:";
  CsvReader.print_data (CsvReader.tail data tail_size);

  (* Perform a strategy and evaluate its performance. *)
  let strategy = Strategy.create_strategy ~moving_average_window:5 in
  let result = backtest strategy data in
  Printf.printf "Backtest Result:\n";
  Printf.printf "Number of Trades: %d\n" (List.length result.trades);
  Printf.printf "Annualized Returns: %.2f%%\n"
    (result.annualized_returns *. 100.0);
  Printf.printf "Sharpe Ratio: %.4f\n" result.sharpe_ratio;

  (* Graph the candlestick chart and moving averages. *)
  Grapher.graph ~m_averages:[ (Grapher.Triangular, 3) ] data

let _ = demo csv_data
