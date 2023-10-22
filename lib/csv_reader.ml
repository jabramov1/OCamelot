(* module type CsvReaderType = sig type t

   val read_csv : unit val get_row : unit val get_col : unit val head : unit val
   tail : unit val print_csv : unit val print_row : unit val print_col : unit
   end *)

module CsvReader = struct
  type col = {
    date : string list;
    open_price : string list;
    high_price : string list;
    low_price : string list;
    close_price : string list;
    adj_price : string list;
    volume : string list;
  }

  type row = {
    date : string;
    open_price : string;
    high_price : string;
    low_price : string;
    close_price : string;
    adj_price : string;
    volume : string;
  }

  type t = {
    col_based : col;
    row_based : row list;
  }

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

  let extract_column index rows =
    List.map
      (fun row ->
        match List.nth_opt row index with
        | Some value -> value
        | None -> failwith (Printf.sprintf "Index out of bounds: %d" index))
      rows

  let read_csv filename ~date ~open_price ~high_price ~low_price ~close_price
      ~adj_price ~volume : t =
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

    let row_based_data = List.map extract_row filtered_rows in

    {
      col_based =
        {
          date = extract_column (List.nth selected_indices 0) filtered_rows;
          open_price =
            extract_column (List.nth selected_indices 1) filtered_rows;
          high_price =
            extract_column (List.nth selected_indices 2) filtered_rows;
          low_price = extract_column (List.nth selected_indices 3) filtered_rows;
          close_price =
            extract_column (List.nth selected_indices 4) filtered_rows;
          adj_price = extract_column (List.nth selected_indices 5) filtered_rows;
          volume = extract_column (List.nth selected_indices 6) filtered_rows;
        };
      row_based = row_based_data;
    }

  let print_string_list (data : string list) =
    print_endline (String.concat ", " data)

  let print_row (row : row) =
    Printf.printf
      "Date: %s, Open Price: %s, High Price: %s, Low Price: %s, Close Price: \
       %s, Adj Price: %s, Volume: %s\n"
      row.date row.open_price row.high_price row.low_price row.close_price
      row.adj_price row.volume

  let print_row_list (rows : row list) = List.iter print_row rows
end

let () =
  let filename = "../data/test.csv" in
  let csv =
    CsvReader.read_csv filename ~date:"Date" ~open_price:"Open"
      ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
      ~adj_price:"Adj Close" ~volume:"Volume"
  in

  CsvReader.print_row (List.hd csv.row_based);
  CsvReader.print_row_list csv.row_based;
  CsvReader.print_string_list csv.col_based.date;
  CsvReader.print_string_list csv.col_based.open_price;
  CsvReader.print_string_list csv.col_based.high_price;
  CsvReader.print_string_list csv.col_based.low_price;
  CsvReader.print_string_list csv.col_based.close_price;
  CsvReader.print_string_list csv.col_based.adj_price;
  CsvReader.print_string_list csv.col_based.volume
