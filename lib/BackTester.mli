open Strategy

type backtest_result = {
  trades : trade list; (* List of trades executed during the backtest *)
  annualized_returns : float; (* Annualized returns based on executed trades *)
  sharpe_ratio : float; (* Sharpe ratio based on executed trades *)
}
(** Type representing the result of a backtest *)

val backtest : t -> CsvReader.row list -> backtest_result
(** [backtest strategy data] performs a backtest using the provided strategy on
    historical market data. It returns a [backtest_result] containing
    information about executed trades, annualized returns, and Sharpe ratio. *)
