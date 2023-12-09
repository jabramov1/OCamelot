open CsvReader
open Strategy

(* Type representing the result of a backtest *)
type backtest_result = {
  trades : trade list; (* List of trades executed during the backtest *)
  annualized_returns : float; (* Annualized returns based on executed trades *)
  sharpe_ratio : float; (* Sharpe ratio based on executed trades *)
}

(* Function to perform a backtest using a strategy on historical market data *)
let backtest (strategy : t) (data : CsvReader.row list) : backtest_result =
  (* Execute the strategy on the historical data to get a list of decisions *)
  let decisions = execute strategy data in

  (* Initialize variables for tracking trades and state *)
  let rec backtest_aux rows prev_trades =
    match (rows, prev_trades) with
    | [], trades -> { trades; annualized_returns = 0.0; sharpe_ratio = 0.0 }
    | row :: rest_rows, trades ->
        (* Execute the strategy to get the current decision *)
        let decision = List.hd decisions in
        (* Update the list of trades based on the current decision and row *)
        let updated_trades = execute_trades decision row trades in
        (* Recursively continue the backtest with the remaining rows and updated
           trades *)
        backtest_aux rest_rows updated_trades
  in

  (* Start the backtest with the provided data and an empty list of trades *)
  let result = backtest_aux data [] in

  (* Calculate and set the annualized returns and Sharpe ratio in the result *)
  let annualized_returns = calculate_annualized_returns result.trades in
  let sharpe_ratio = calculate_sharpe_ratio result.trades in
  { result with annualized_returns; sharpe_ratio }

(* Example usage of the backtest function *)
let () =
  (* Load historical market data from a CSV file *)
  let data = CsvReader.load_csv "historical_data.csv" in

  (* Create a strategy with a moving average window of 10 *)
  let strategy = create_strategy ~moving_average_window:10 in

  (* Perform a backtest using the strategy and historical data *)
  let result = backtest strategy data in

  (* Print the backtest result *)
  Printf.printf "Backtest Result:\n";
  Printf.printf "Number of Trades: %d\n" (List.length result.trades);
  Printf.printf "Annualized Returns: %.2f%%\n"
    (result.annualized_returns *. 100.0);
  Printf.printf "Sharpe Ratio: %.4f\n" result.sharpe_ratio
