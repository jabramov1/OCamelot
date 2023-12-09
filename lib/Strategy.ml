open CsvReader
open MovingAverage

(* Type representing the decision to Buy, Sell, or Hold *)
type decision =
  | Buy
  | Sell
  | Hold

(* Configuration type for the strategy, specifying the moving average window
   size *)
type t = { moving_average_window : int }

(* Function to create a strategy with a specified moving average window *)
let create_strategy ~moving_average_window = { moving_average_window }

(* Function to execute the strategy on a list of rows, producing a list of
   decisions *)
let execute (strategy : t) (data : CsvReader.row list) =
  let rec execute_aux rows previous_rows acc_decisions =
    match rows with
    | [] -> List.rev acc_decisions
    | row :: rest_rows ->
        let previous_moving_averages =
          MovingAverage.simple_moving_avg (row :: previous_rows)
            strategy.moving_average_window
        in
        let last_moving_average =
          match List.rev previous_moving_averages with
          | [] -> None
          | hd :: _ -> Some hd
        in
        let decision =
          match (CsvReader.get_closing_price row, last_moving_average) with
          | Some _, None -> Hold
          | None, Some _ -> Hold
          | None, None -> Hold
          | Some current_price, Some prev_avg -> (
              match prev_avg with
              | Some sm_prev_avg ->
                  if current_price > sm_prev_avg then Buy
                  else if current_price < sm_prev_avg then Sell
                  else Hold
              | None -> Hold)
        in
        execute_aux rest_rows (row :: previous_rows) (decision :: acc_decisions)
  in
  execute_aux data [] []

(* Type representing a trading action with details such as entry/exit date,
   prices, and decision *)
type trade = {
  entry_date : float;
  exit_date : float option;
  entry_price : float option;
  exit_price : float option;
  decision : decision;
}

(* Function to execute trades based on decisions, updating the list of existing
   trades *)
let execute_trades (decision : decision) (_row : CsvReader.row)
    (trades : 'a list) =
  match decision with
  | Buy ->
      {
        entry_date = CsvReader.get_date _row;
        exit_date = None;
        entry_price = CsvReader.get_closing_price _row;
        exit_price = None;
        decision;
      }
      :: trades
  | Sell -> (
      match trades with
      | [] -> trades (* No open position to sell *)
      | buy_trade :: rest_trades ->
          {
            buy_trade with
            exit_date = Some (CsvReader.get_date _row);
            exit_price =
              Some
                (Option.value (CsvReader.get_closing_price _row) ~default:0.0);
            decision;
          }
          :: rest_trades)
  | Hold -> trades (* No new trades, just hold the existing positions *)
