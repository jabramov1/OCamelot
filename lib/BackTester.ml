open Strategy

(* Type representing the result of a backtest *)
type backtest_result = {
  trades : trade list;
  annualized_returns : float;
  sharpe_ratio : float;
}

(* Function to calculate annualized returns based on executed trades *)
let calculate_annualized_returns (trades : trade list) : float =
  let total_returns =
    List.fold_left
      (fun acc trade ->
        match (trade.exit_price, trade.entry_price) with
        | Some exit_price, Some entry_price ->
            acc +. ((exit_price -. entry_price) /. entry_price)
        | _ -> acc)
      0.0 trades
  in
  let num_trades = List.length trades in
  if num_trades > 0 then
    ((total_returns +. 1.0) ** (1.0 /. float_of_int num_trades)) -. 1.0
  else 0.0

(* Custom standard deviation calculation *)
let std_deviation (lst : float list) : float =
  let n = List.length lst in
  if n <= 1 then
    failwith "Standard deviation requires at least two data points."
  else
    let mean = List.fold_left ( +. ) 0.0 lst /. float_of_int n in
    let sum_squares =
      List.fold_left (fun acc x -> acc +. ((x -. mean) ** 2.0)) 0.0 lst
    in
    sqrt (sum_squares /. float_of_int (n - 1))

(* Function to calculate Sharpe ratio based on executed trades *)
let calculate_sharpe_ratio (trades : trade list) : float =
  let annualized_returns = calculate_annualized_returns trades in
  let risk_free_rate = 0.02 in
  let daily_returns =
    List.map
      (fun trade ->
        (Option.value trade.exit_price ~default:0.0
        -. Option.value trade.entry_price ~default:0.0)
        /. Option.value trade.entry_price ~default:1.0)
      trades
  in
  let daily_volatility = std_deviation daily_returns in
  let sharpe_ratio =
    (annualized_returns -. risk_free_rate) /. daily_volatility
  in
  sharpe_ratio

(* Function to perform a backtest using a strategy on historical market data *)
let backtest (strategy : t) (data : CsvReader.row list) : backtest_result =
  let decisions = execute strategy data in

  let rec backtest_aux rows remaining_decisions prev_trades =
    match (rows, remaining_decisions, prev_trades) with
    | [], _, trades -> { trades; annualized_returns = 0.0; sharpe_ratio = 0.0 }
    | row :: rest_rows, decision :: rest_decisions, trades ->
        let updated_trades = execute_trades decision row trades in
        backtest_aux rest_rows rest_decisions updated_trades
    | _, [], trades -> { trades; annualized_returns = 0.0; sharpe_ratio = 0.0 }
  in

  let result = backtest_aux data decisions [] in
  let annualized_returns = calculate_annualized_returns result.trades in
  let sharpe_ratio = calculate_sharpe_ratio result.trades in

  { result with annualized_returns; sharpe_ratio }
