module type CsvReaderType = sig
  type elt
  (** Representation type for each row in the CSV file. *)

  type t = elt list
  (** Representation type for entire CSV data file. Each row has type [elt] and
      is ordered chronologically within a list. *)

  val read_csv :
    date:string ->
    open_price:string ->
    high_price:string ->
    low_price:string ->
    close_price:string ->
    adj_price:string ->
    volume:string ->
    ?separator:string ->
    string ->
    t
  (** [read_csv] loads a CSV file into a format suitable for financial
      calculations. An entry x is stored as [Some x], while empty entries are
      stored as [None].

      @raise Sys_error if the given file can't be found.

      @param date
        The title of the column (within the header) that stores the dates.

      @param open_price
        The title of the column (within the header) that stores the open prices.

      @param high_price
        The title of the column (within the header) that stores the high prices.

      @param low_price
        The title of the column (within the header) that stores the low prices.

      @param close_price
        The title of the column (within the header) that stores the closing
        prices.

      @param adj_price
        The title of the column (within the header) that stores adjusted prices.

      @param volume
        The title of the column (within the header) that stores the volumes.

      @param separator
        The character of the separator between entries in the CSV file. The
        default is [',']. *)

  val size : t -> int
  (** [size d] returns the number of rows stored in the CSV data representation
      [d]. *)

  val get_row : t -> int -> elt
  (** [get_row d n] returns the row in [d] located at index [n]. Indices begin
      at 0.

      @raise Not_found if index is out of bounds. *)

  val get_date : elt -> string option
  (** [get_date r] returns the date for the given row [r]. *)

  val get_open_price : elt -> float option
  (** [get_open_price r] returns the open price for the given row [r]. *)

  val get_high_price : elt -> float option
  (** [get_high_price r] returns the high price for the given row [r]. *)

  val get_low_price : elt -> float option
  (** [get_low_price r] returns the low price for the given row [r]. *)

  val get_closing_price : elt -> float option
  (** [get_closing_price r] returns the closing price for the given row [r]. *)

  val get_adj_price : elt -> float option
  (** [get_adj_price r] returns the adjusted price for the given row [r]. *)

  val get_volume_price : elt -> int option
  (** [get_date row] returns the date for the given [row]. *)

  val get_dates : t -> string option list
  (** [get_dates d] returns the dates column in the CSV data representation [d]. *)

  val get_open_prices : t -> float option list
  (** [get_open_prices d] returns the open prices column in the CSV data
      representation [d] *)

  val get_high_prices : t -> float option list
  (** [get_high_prices d] returns the high prices column in the CSV data
      representation [d]. *)

  val get_low_prices : t -> float option list
  (** [get_low_prices d] returns the low prices column in the CSV data
      representation [d]. *)

  val get_closing_prices : t -> float option list
  (** [get_closing_prices d] returns the closing prices column in the CSV data
      representation [d]. *)

  val get_adj_prices : t -> float option list
  (** [get_adj_prices d] returns the adjusted prices column in the CSV data
      representation [d]. *)

  val get_volume : t -> int option list
  (** [get_volume_prices d] returns the volume column in the CSV data
      representation [d]. *)

  val head : t -> int -> elt list
  (** [head d n] returns the first [n] elements of the CSV data representation
      [d]. If [n <= 0] then return the empty list. If [n] is greater than the
      number of rows in [d], then return the entirety of [d]. *)

  val tail : t -> int -> elt list
  (** [tail d n] returns the last [n] elements of the CSV data representation
      [d]. If [n <= 0] then return the empty list. If [n] is greater than the
      number of rows in [d], then return the entirety of [d]. *)

  val print_data : t -> unit
  (** [print_data d] prints the data [d] in the form ... *)

  val print_row : elt -> unit
  (** [print_row r] prints a given row [r] in*)
end
