open Ocamelot
open Ocamelot.BackTester

let clean_csv =
  CsvReader.read_csv ~date:"Date" ~open_price:"open" ~high_price:"high"
    ~low_price:"low" ~close_price:"close" ~volume:"volume" ~date_type:"MM/DD/YY"
    "./data/StockData.csv"

let spy_csv =
  CsvReader.read_csv ~date:"Date" ~open_price:"Open" ~high_price:"High"
    ~low_price:"Low" ~close_price:"Close" ~volume:"Volume" "./data/SPY.csv"

let messy_csv =
  CsvReader.read_csv ~date:" the date " ~open_price:"Open" ~high_price:"hi"
    ~low_price:"Low" ~close_price:"Close" ~volume:"vol1234 "
    "./data/test/general.csv"

let data_list = [ clean_csv; spy_csv; messy_csv ]

let demo_csv data =
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
  CsvReader.print_data (CsvReader.tail data tail_size)

let demo_backtest data =
  let strategy = Strategy.create_strategy ~moving_average_window:5 in
  let result = backtest strategy data in
  Printf.printf "Backtest Result:\n";
  Printf.printf "Number of Trades: %d\n" (List.length result.trades);
  Printf.printf "Annualized Returns: %.2f%%\n"
    (result.annualized_returns *. 100.0);
  Printf.printf "Sharpe Ratio: %.4f\n" result.sharpe_ratio

let demo_graph data = Grapher.graph ~m_averages:[ (Grapher.Triangular, 3) ] data

let demo data =
  demo_csv data;
  demo_backtest data;
  demo_graph data

let _ = demo (List.nth data_list 2)
