(** TODO: header function + handle cases with no date + different date formats*)
module type CsvReaderType = sig
  type row
  type t = row list

  val read_csv :
    date:string ->
    open_price:string ->
    high_price:string ->
    low_price:string ->
    close_price:string ->
    adj_price:string ->
    volume:string ->
    ?separator:char ->
    string ->
    t

  val size : t -> int
  val get_row : t -> int -> row
  val get_date : row -> string option
  val get_open_price : row -> float option
  val get_high_price : row -> float option
  val get_low_price : row -> float option
  val get_closing_price : row -> float option
  val get_adj_price : row -> float option
  val get_volume : row -> int option
  val get_dates : t -> string option list
  val get_open_prices : t -> float option list
  val get_high_prices : t -> float option list
  val get_low_prices : t -> float option list
  val get_closing_prices : t -> float option list
  val get_adj_prices : t -> float option list
  val get_volumes : t -> int option list
  val head : t -> int -> row list
  val tail : t -> int -> row list
  val string_of_row : row -> string
  val print_row : row -> unit
  val string_of_data : t -> string
  val print_data : t -> unit
end

module CsvReader : CsvReaderType = struct
  type row = {
    date : string option;
    open_price : float option;
    high_price : float option;
    low_price : float option;
    close_price : float option;
    adj_price : float option;
    volume : int option;
  }

  type t = row list

  type row_field_type =
    | String of string
    | Int of int
    | Float of float

  let size data = List.length data

  let get_row (data : t) i =
    if i >= 0 && i < List.length data then List.nth data i else raise Not_found

  let get_date row = row.date
  let get_open_price row = row.open_price
  let get_high_price row = row.high_price
  let get_low_price row = row.low_price
  let get_closing_price row = row.close_price
  let get_adj_price row = row.adj_price
  let get_volume row = row.volume
  let get_dates data = List.map (fun row -> row.date) data
  let get_open_prices data = List.map (fun row -> row.open_price) data
  let get_high_prices data = List.map (fun row -> row.high_price) data
  let get_low_prices data = List.map (fun row -> row.low_price) data
  let get_closing_prices data = List.map (fun row -> row.close_price) data
  let get_adj_prices data = List.map (fun row -> row.adj_price) data
  let get_volumes data = List.map (fun row -> row.volume) data

  let index_of (elem : string) (lst : string list) : int =
    let rec helper elem lst index =
      match lst with
      | [] -> -1
      | h :: t -> if h = elem then index else helper elem t (index + 1)
    in
    helper elem lst 0

  let filter_header header keys =
    List.map
      (fun col_name ->
        print_endline col_name;
        match index_of col_name header with
        | -1 ->
            raise
              (Sys_error ("Column " ^ col_name ^ " not found in the CSV file."))
        | index -> index)
      keys

  (* Given a given row [r] and index [idx], process the entry at [idx] into a
     optional based on the required row field type. [extract_type] returns
     [None] if a given entry raises an error during string conversion. *)
  let extract_type (r : string list) (t : row_field_type) (idx : int) =
    let elem = List.nth r idx |> String.trim in
    try
      match t with
      | Int _ -> Some (Int (int_of_string elem))
      | Float _ -> Some (Float (float_of_string elem))
      | String _ -> Some (String elem)
    with _ -> None

  (* Given a row field type optional storing an int, return an optional
     containing just the int itself. *)
  let extract_int (elem : row_field_type option) =
    match elem with
    | Some (Int i) -> Some i
    | _ -> None

  (* Given a row field type optional storing a float, return an optional
     containing just the float itself.*)
  let extract_float (elem : row_field_type option) =
    match elem with
    | Some (Float f) -> Some f
    | _ -> None

  (* Given a row field type optional storing a string, return an optional
     containing just the string itself. *)
  let extract_string (elem : row_field_type option) =
    match elem with
    | Some (String s) ->
        if String.length (String.trim s) > 0 then Some s else None
    | _ -> None

  let read_csv ~date ~open_price ~high_price ~low_price ~close_price ~adj_price
      ~volume ?(separator = ',') filename : t =
    let csv = Csv.load filename ~separator in
    let keys =
      [
        date; open_price; high_price; low_price; close_price; adj_price; volume;
      ]
      |> List.map (fun s -> String.trim s)
    in
    let header = List.hd csv in
    let selected_indices = filter_header header keys in
    let filtered_rows = List.tl csv in

    let extract_row row =
      {
        date =
          List.nth selected_indices 0
          |> extract_type row (String "")
          |> extract_string;
        open_price =
          List.nth selected_indices 1
          |> extract_type row (Float 0.)
          |> extract_float;
        high_price =
          List.nth selected_indices 2
          |> extract_type row (Float 0.)
          |> extract_float;
        low_price =
          List.nth selected_indices 3
          |> extract_type row (Float 0.)
          |> extract_float;
        close_price =
          List.nth selected_indices 4
          |> extract_type row (Float 0.)
          |> extract_float;
        adj_price =
          List.nth selected_indices 5
          |> extract_type row (Float 0.)
          |> extract_float;
        volume =
          List.nth selected_indices 6 |> extract_type row (Int 0) |> extract_int;
      }
    in
    List.map extract_row filtered_rows

  let head data n =
    let len = List.length data in

    let rec head_aux data n =
      match (data, n) with
      | [], _ | _, 0 -> []
      | h :: t, n -> h :: head_aux t (n - 1)
    in
    if n <= 0 then []
    else if n > len then head_aux data (List.length data)
    else head_aux data n

  let tail data n = head (List.rev data) n |> List.rev

  let string_of_field name value =
    let unwrap_opt =
      match value with
      | Some v -> v
      | None -> "N/A"
    in
    if name = "Volume" then Printf.sprintf "%s: %s" name unwrap_opt
    else Printf.sprintf "%s: %s, " name unwrap_opt

  let string_of_row row =
    string_of_field "Date" (Option.map (fun s -> s) row.date)
    ^ string_of_field "Open Price" (Option.map string_of_float row.open_price)
    ^ string_of_field "High Price" (Option.map string_of_float row.high_price)
    ^ string_of_field "Low Price" (Option.map string_of_float row.low_price)
    ^ string_of_field "Close Price" (Option.map string_of_float row.close_price)
    ^ string_of_field "Adj Price" (Option.map string_of_float row.adj_price)
    ^ string_of_field "Volume" (Option.map string_of_int row.volume)

  let print_row row = string_of_row row |> print_endline

  let string_of_data data =
    Printf.sprintf
      "Date, Open Price, High Price, Low Price, Close Price, Adj Price, Volume\n"
    ^ List.fold_left (fun acc row -> acc ^ string_of_row row) "" data

  let print_data data = string_of_data data |> print_endline
end

module Main = struct
  open CsvReader

  let main () =
    let filename = "data/test/clean.csv" in

    let data =
      read_csv ~date:"Date" ~open_price:"Open" ~high_price:"High"
        ~low_price:"Low" ~close_price:"Close" ~adj_price:"Adj Close"
        ~volume:"Volume" filename
    in

    print_data data;

    let row_index = 0 in
    let specific_row = get_row data row_index in
    print_row specific_row;

    let head_size = 0 in
    let tail_size = 0 in
    print_data (head data head_size);
    print_data (tail data tail_size)
end

let () = Main.main ()
