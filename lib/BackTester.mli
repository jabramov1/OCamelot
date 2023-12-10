open Strategy

type backtest_result = {
  trades : trade list;
  annualized_returns : float;
  sharpe_ratio : float;
}
(** Type representing the result of a backtest *)

val backtest : t -> CsvReader.row list -> backtest_result
(** [backtest s d] performs a backtest using the provided strategy [s] on
    historical market data, stored in CSV data representation [d]. It returns a
    [backtest_result] containing information about executed trades, annualized
    returns, and Sharpe ratio. Note that if your data has large amounts of empty
    values, the calculations may not be able to perform properly. *)
