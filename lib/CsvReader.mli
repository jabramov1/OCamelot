module type CsvReaderType = sig
  type t

  val read_csv :
    string ->
    date:string ->
    open_price:string ->
    high_price:string ->
    low_price:string ->
    close_price:string ->
    adj_price:string ->
    volume:string ->
    t list

  val get_row : int -> t list -> t
  val get_date : t -> string
  val get_open_price : t -> string
  val get_high_price : t -> string
  val get_low_price : t -> string
  val get_closing_price : t -> string
  val get_adj_price : t -> string
  val get_volume_price : t -> string
  val get_dates : t list -> string list
  val get_open_prices : t list -> string list
  val get_high_prices : t list -> string list
  val get_low_prices : t list -> string list
  val get_closing_prices : t list -> string list
  val get_adj_prices : t list -> string list
  val get_volume_prices : t list -> string list

  (* Make optional*)
  val head : t list -> int -> t list
  val tail : t list -> int -> t list
  val print_data : t list -> unit
  val print_row : t -> unit
  val print_string_list : string list -> unit
end

module CsvReader : CsvReaderType
