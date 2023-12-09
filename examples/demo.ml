open Ocamelot
open Ocamelot.BackTester

let data_1 =
  CsvReader.read_csv "./data/SPY.csv" ~date:"Date" ~open_price:"Open"
    ~high_price:"High" ~low_price:"Low" ~close_price:"Close" ~volume:"Volume"

let data_2 =
  CsvReader.read_csv "./data/StockData.csv" ~date:"Date" ~open_price:"open"
    ~high_price:"high" ~low_price:"low" ~close_price:"close" ~volume:"volume"
    ~date_type:"MM/DD/YY"

let messy_data =
  CsvReader.read_csv ~date:" the date " ~open_price:"Open" ~high_price:"hi"
    ~low_price:"Low" ~close_price:"\n       Close" ~volume:"vol1234 "
    "./data/test/general.csv"

let data_list = [ data_1; data_2; messy_data ]

let demo data =
  let size = CsvReader.size data in
  print_endline (string_of_int size);

  let row_index = 5 in
  let row = CsvReader.get_row data row_index in
  print_endline ("Row at index " ^ string_of_int row_index);
  CsvReader.print_row row;

  let head_size = 3 in
  print_endline "First 3 elements:";
  CsvReader.print_data (CsvReader.head data head_size);

  let tail_size = 4 in
  print_endline "Last 4 elements:";
  CsvReader.print_data (CsvReader.tail data tail_size);

  let strategy = Strategy.create_strategy ~moving_average_window:5 in
  let result = backtest strategy data in
  Printf.printf "Backtest Result:\n";
  Printf.printf "Number of Trades: %d\n" (List.length result.trades);
  Printf.printf "Annualized Returns: %.2f%%\n"
    (result.annualized_returns *. 100.0);
  Printf.printf "Sharpe Ratio: %.4f\n" result.sharpe_ratio;
  Grapher.graph data

let _ = demo (List.nth data_list 0)
