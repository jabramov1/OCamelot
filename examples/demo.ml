open Ocamelot.Grapher
open Ocamelot.CsvReader
open Ocamelot.BackTester
open Ocamelot.Strategy

let _ =
  let csv_data =
    CsvReader.read_csv "./data/SPY.csv" ~date:"Date" ~open_price:"Open"
      ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
      ~adj_price:"Adj Close" ~volume:"Volume"
  in
  Grapher.graph csv_data

let () =
  let csv_data =
    CsvReader.read_csv "./data/SPY.csv" ~date:"Date" ~open_price:"Open"
      ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
      ~adj_price:"Adj Close" ~volume:"Volume"
  in
  (* Create a strategy with a moving average window of 10 *)
  let strategy = Strategy.create_strategy ~moving_average_window:10 in

  (* Perform a backtest using the strategy and historical data *)
  let result = BackTester.backtest strategy csv_data in

  (* Print the backtest result *)
  Printf.printf "Backtest Result:\n";
  Printf.printf "Number of Trades: %d\n" (List.length result.trades);
  Printf.printf "Annualized Returns: %.2f%%\n"
    (result.annualized_returns *. 100.0);
  Printf.printf "Sharpe Ratio: %.4f\n" result.sharpe_ratio
