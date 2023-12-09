(** Type representing the decision to Buy, Sell, or Hold *)
type decision =
  | Buy
  | Sell
  | Hold

type t = { moving_average_window : int }
(** Configuration type for the strategy, specifying the moving average window
    size *)

val create_strategy : moving_average_window:int -> t
(** [create_strategy ~moving_average_window] creates a new strategy with the
    specified moving average window.
    @param moving_average_window The size of the moving average window.
    @return A new strategy. *)

val execute : t -> CsvReader.row list -> decision list
(** [execute strategy data] executes the strategy on the given list of rows,
    producing a list of decisions.
    @param strategy The strategy configuration.
    @param data The list of rows to execute the strategy on.
    @return A list of decisions. *)

type trade = {
  entry_date : float; (* Entry date of the trade *)
  exit_date : float option; (* Exit date of the trade, if applicable *)
  entry_price : float option; (* Entry price of the trade, if applicable *)
  exit_price : float option; (* Exit price of the trade, if applicable *)
  decision : decision; (* Decision associated with the trade *)
}
(** Type representing a trading action with details such as entry/exit date,
    prices, and decision *)

val execute_trades : decision -> CsvReader.row -> trade list -> trade list
(** [execute_trades decision row trades] executes trades based on the provided
    decision, updating the list of existing trades.
    @param decision The decision to execute.
    @param row The current row of data.
    @param trades The list of existing trades.
    @return The updated list of trades. *)
