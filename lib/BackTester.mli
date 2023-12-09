open CsvReader
open Strategy

(** Type representing the result of a backtest *)
type backtest_result = {
  trades: trade list;               (* List of trades executed during the backtest *)
  annualized_returns: float;        (* Annualized returns based on executed trades *)
  sharpe_ratio: float;              (* Sharpe ratio based on executed trades *)
}

(** [backtest strategy data] performs a backtest using the provided strategy on historical market data.
    It returns a [backtest_result] containing information about executed trades, annualized returns, and Sharpe ratio. *)
val backtest : t -> CsvReader.row list -> backtest_result

(** Example usage of the backtest function.
    This function loads historical market data from a CSV file, creates a strategy with a moving average window of 10,
    performs a backtest using the strategy and historical data, and prints the backtest result. *)
val () : unit
