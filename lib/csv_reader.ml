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

module CsvReader : CsvReaderType = struct
  type t = {
    date : string;
    open_price : string;
    high_price : string;
    low_price : string;
    close_price : string;
    adj_price : string;
    volume : string;
  }

  let get_row i data : t = List.nth data i
  let get_date pb = pb.date
  let get_open_price pb = pb.open_price
  let get_high_price pb = pb.high_price
  let get_low_price pb = pb.low_price
  let get_closing_price pb = pb.close_price
  let get_adj_price pb = pb.adj_price
  let get_volume_price pb = pb.volume
  let get_dates data = List.map (fun pb -> pb.date) data
  let get_open_prices data = List.map (fun pb -> pb.open_price) data
  let get_high_prices data = List.map (fun pb -> pb.high_price) data
  let get_low_prices data = List.map (fun pb -> pb.low_price) data
  let get_closing_prices data = List.map (fun pb -> pb.close_price) data
  let get_adj_prices data = List.map (fun pb -> pb.adj_price) data
  let get_volume_prices data = List.map (fun pb -> pb.volume) data

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
      ~adj_price ~volume =
    let csv = Csv.load filename in
    let keys =
      [
        date; open_price; high_price; low_price; close_price; adj_price; volume;
      ]
    in
    let header = List.hd csv in
    let selected_indices = filter_header header keys in
    let filtered_rows = List.tl csv in

    let extract_row row =
      {
        date = List.nth row (List.nth selected_indices 0);
        open_price = List.nth row (List.nth selected_indices 1);
        high_price = List.nth row (List.nth selected_indices 2);
        low_price = List.nth row (List.nth selected_indices 3);
        close_price = List.nth row (List.nth selected_indices 4);
        adj_price = List.nth row (List.nth selected_indices 5);
        volume = List.nth row (List.nth selected_indices 6);
      }
    in
    List.map extract_row filtered_rows

  let head data n =
    let rec head_helper acc rest n =
      match (rest, n) with
      | _, 0 | [], _ -> List.rev acc
      | h :: t, n when n > 0 -> head_helper (h :: acc) t (n - 1)
      | _ -> invalid_arg "n must be a non-negative integer"
    in
    head_helper [ List.hd data ] (List.tl data) n

  let tail data n = head (List.rev data) n

  let print_string_list (data : string list) =
    print_endline (String.concat ", " data)

  let print_row row =
    Printf.printf
      "Date: %s, Open Price: %s, High\n\
      \     Price: %s, Low Price: %s, Close Price:  %s, Adj Price: %s, Volume: \
       %s\n"
      row.date row.open_price row.high_price row.low_price row.close_price
      row.adj_price row.volume

  let print_data data =
    print_endline
      "Date, Open Price, High Price, Low Price, Close Price, Adj Price, Volume";
    List.iter
      (fun row ->
        Printf.printf "%s, %s, %s, %s, %s, %s, %s\n" row.date row.open_price
          row.high_price row.low_price row.close_price row.adj_price row.volume)
      data
end

let () =
  let filename = "../data/test.csv" in
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
