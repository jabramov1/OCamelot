(** Trading strategies to perform on CSV data representation. *)

type decision =
  | Buy
  | Sell
  | Hold  (** Type representing the decision to Buy, Sell, or Hold *)

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
  entry_date : float;
  exit_date : float option;
  entry_price : float option;
  exit_price : float option;
  decision : decision;
}
(** Type representing a trading action including details on entry/exit date,
    entry/exit prices, and the decision *)

val execute_trades : decision -> CsvReader.row -> trade list -> trade list
(** [execute_trades decision row trades] executes trades based on the provided
    decision, updating the list of existing trades.

    @param decision The decision to execute.

    @param row The current row of data.

    @param trades The list of existing trades.

    @return The updated list of trades. *)
