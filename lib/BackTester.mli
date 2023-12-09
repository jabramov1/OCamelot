open Strategy

type backtest_result = {
  trades : trade list;
  annualized_returns : float;
  sharpe_ratio : float;
}
(** Type representing the result of a backtest *)

val backtest : t -> CsvReader.row list -> backtest_result
(** [backtest strategy data] performs a backtest using the provided strategy on
    historical market data. It returns a [backtest_result] containing
    information about executed trades, annualized returns, and Sharpe ratio. *)
