module type CsvReaderType = sig
  type elt
  type t = elt list

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

  val size : t -> int
  val get_row : t -> int -> elt
  val get_date : elt -> string option
  val get_open_price : elt -> float option
  val get_high_price : elt -> float option
  val get_low_price : elt -> float option
  val get_closing_price : elt -> float option
  val get_adj_price : elt -> float option
  val get_volume_price : elt -> int option
  val get_dates : t -> string option list
  val get_open_prices : t -> float option list
  val get_high_prices : t -> float option list
  val get_low_prices : t -> float option list
  val get_closing_prices : t -> float option list
  val get_adj_prices : t -> float option list
  val get_volume : t -> int option list
  val head : t -> int -> elt list
  val tail : t -> int -> elt list
  val print_data : t -> unit
  val print_row : elt -> unit
end

module CsvReader = struct
  type elt = {
    date : string option;
    open_price : string option;
    high_price : string option;
    low_price : string option;
    close_price : string option;
    adj_price : string option;
    volume : string option;
  }

  type t = elt list

  let get_row i data =
    if i >= 0 && i < Array.length data then Some data.(i) else None

  let get_date elt = elt.date
  let get_open_price elt = elt.open_price
  let get_high_price elt = elt.high_price
  let get_low_price elt = elt.low_price
  let get_closing_price elt = elt.close_price
  let get_adj_price elt = elt.adj_price
  let get_volume_price elt = elt.volume
  let get_dates data = List.map (fun elt -> elt.date) data
  let get_open_prices data = List.map (fun elt -> elt.open_price) data
  let get_high_prices data = List.map (fun elt -> elt.high_price) data
  let get_low_prices data = List.map (fun elt -> elt.low_price) data
  let get_closing_prices data = List.map (fun elt -> elt.close_price) data
  let get_adj_prices data = List.map (fun elt -> elt.adj_price) data
  let get_volume_prices data = List.map (fun elt -> elt.volume) data

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
        match index_of col_name header with
        | -1 ->
            failwith
              (Printf.sprintf "Column '%s' not found in the CSV file." col_name)
        | index -> index)
      keys

  let read_csv filename ~date ~open_price ~high_price ~low_price ~close_price
      ~adj_price ~volume ~separator:char =
    let csv = Csv.load filename in
    let keys =
      [
        date; open_price; high_price; low_price; close_price; adj_price; volume;
      ]
    in
    let header = List.hd csv in
    let selected_indices = filter_header header keys in
    let filtered_rows = List.tl csv |> Array.of_list in

    let extract_row row =
      let extract_option idx =
        if idx < List.length row then Some (List.nth row idx) else None
      in
      {
        date = extract_option (List.nth selected_indices 0);
        open_price = extract_option (List.nth selected_indices 1);
        high_price = extract_option (List.nth selected_indices 2);
        low_price = extract_option (List.nth selected_indices 3);
        close_price = extract_option (List.nth selected_indices 4);
        adj_price = extract_option (List.nth selected_indices 5);
        volume = extract_option (List.nth selected_indices 6);
      }
    in
    Array.map extract_row filtered_rows

  let head data n =
    let len = Array.length data in
    if n <= 0 || n > len then None else Some (Array.sub data 0 n)

  let tail data n =
    let len = Array.length data in
    if n <= 0 || n > len then None else Some (Array.sub data (len - n) n)

  let print_string_list (data : string option array) =
    let print_item item =
      match item with
      | Some s -> print_string s
      | None -> print_string "N/A"
    in
    Array.iter
      (fun item ->
        print_item item;
        print_string ", ")
      data;
    print_newline ()

  let print_row row =
    Printf.printf "Date: %s, "
      (match row.date with
      | Some d -> d
      | None -> "N/A");
    Printf.printf "Open Price: %s, "
      (match row.open_price with
      | Some p -> p
      | None -> "N/A");
    Printf.printf "High Price: %s, "
      (match row.high_price with
      | Some p -> p
      | None -> "N/A");
    Printf.printf "Low Price: %s, "
      (match row.low_price with
      | Some p -> p
      | None -> "N/A");
    Printf.printf "Close Price: %s, "
      (match row.close_price with
      | Some p -> p
      | None -> "N/A");
    Printf.printf "Adj Price: %s, "
      (match row.adj_price with
      | Some p -> p
      | None -> "N/A");
    Printf.printf "Volume: %s\n"
      (match row.volume with
      | Some v -> v
      | None -> "N/A")

  let print_data data =
    print_endline
      "Date, Open Price, High Price, Low Price, Close Price, Adj Price, Volume";
    Array.iter
      (fun row ->
        Printf.printf "%s, %s, %s, %s, %s, %s, %s\n"
          (match row.date with
          | Some d -> d
          | None -> "N/A")
          (match row.open_price with
          | Some p -> p
          | None -> "N/A")
          (match row.high_price with
          | Some p -> p
          | None -> "N/A")
          (match row.low_price with
          | Some p -> p
          | None -> "N/A")
          (match row.close_price with
          | Some p -> p
          | None -> "N/A")
          (match row.adj_price with
          | Some p -> p
          | None -> "N/A")
          (match row.volume with
          | Some v -> v
          | None -> "N/A"))
      data
end

let () =
  let filename = "data/test.csv" in
  let csv =
    CsvReader.read_csv filename ~date:"Date" ~open_price:"Open"
      ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
      ~adj_price:"Adj Close" ~volume:"Volume"
  in

  let first_row = CsvReader.get_row 0 csv in
  let dates = CsvReader.get_dates csv in
  let open_prices = CsvReader.get_open_prices csv in
  let high_prices = CsvReader.get_high_prices csv in
  let low_prices = CsvReader.get_low_prices csv in
  let closing_prices = CsvReader.get_closing_prices csv in
  let adj_prices = CsvReader.get_adj_prices csv in
  let volume_prices = CsvReader.get_volume_prices csv in

  CsvReader.print_row first_row;
  CsvReader.print_string_list dates;
  CsvReader.print_string_list open_prices;
  CsvReader.print_string_list high_prices;
  CsvReader.print_string_list low_prices;
  CsvReader.print_string_list closing_prices;
  CsvReader.print_string_list adj_prices;
  CsvReader.print_string_list volume_prices;

  CsvReader.print_data csv;
  let first_5_rows = CsvReader.head csv 5 in
  let last_5_rows = CsvReader.tail csv 5 in
  CsvReader.print_data first_5_rows;
  CsvReader.print_data last_5_rows
