type row = {
  date : float;
  open_price : float option;
  high_price : float option;
  low_price : float option;
  close_price : float option;
  volume : float option;
}

type t = row list

let size data = List.length data

let make_row lst =
  if List.length lst > 6 then raise (Invalid_argument "Invalid row format")
  else
    match lst with
    | date :: open_p :: high_p :: low_p :: close_p :: volume :: _ ->
        {
          date = float_of_string date;
          open_price = Some (float_of_string open_p);
          high_price = Some (float_of_string high_p);
          low_price = Some (float_of_string low_p);
          close_price = Some (float_of_string close_p);
          volume = Some (float_of_string volume);
        }
    | _ -> raise (Invalid_argument "Invalid row format")

let make_csv lst = List.map make_row lst

let get_row (data : t) i =
  if i >= 0 && i < List.length data then List.nth data i else raise Not_found

let get_date row = row.date
let get_open_price row = row.open_price
let get_high_price row = row.high_price
let get_low_price row = row.low_price
let get_closing_price row = row.close_price
let get_volume row = row.volume
let get_dates data = List.map (fun row -> row.date) data
let get_open_prices data = List.map (fun row -> row.open_price) data
let get_high_prices data = List.map (fun row -> row.high_price) data
let get_low_prices data = List.map (fun row -> row.low_price) data
let get_closing_prices data = List.map (fun row -> row.close_price) data
let get_volumes data = List.map (fun row -> row.volume) data

let index_of (elem : string) (lst : string list) : int =
  let rec helper elem lst index =
    match lst with
    | [] -> -1
    | h :: t -> if h = elem then index else helper elem t (index + 1)
  in
  helper elem lst 0

let filter_header (header : string list) (keys : string list) : int list =
  List.map
    (fun col_name ->
      match index_of col_name header with
      | -1 ->
          raise
            (Sys_error ("Column " ^ col_name ^ " not found in the CSV file."))
      | index -> index)
    keys

let float_opt_of_string s = try Some (float_of_string s) with _ -> None

let read_csv ~date ~open_price ~high_price ~low_price ~close_price ~volume
    ?(date_type = "YYYY-MM-DD") ?(separator = ',') filename : t =
  let csv = Csv.load filename ~separator in
  let keys =
    [ date; open_price; high_price; low_price; close_price; volume ]
    |> List.map (fun s -> String.trim s)
    |> List.map (fun s -> String.map (fun c -> if c = ',' then ' ' else c) s)
  in
  let header = List.hd csv in
  let selected_indices = filter_header header keys in
  let rest = List.tl csv in

  let extract_row row =
    {
      date =
        List.nth selected_indices 0
        |> List.nth row
        |> DateConverter.string_to_date ~date_type;
      open_price =
        List.nth selected_indices 1 |> List.nth row |> float_opt_of_string;
      high_price =
        List.nth selected_indices 2 |> List.nth row |> float_opt_of_string;
      low_price =
        List.nth selected_indices 3 |> List.nth row |> float_opt_of_string;
      close_price =
        List.nth selected_indices 4 |> List.nth row |> float_opt_of_string;
      volume =
        List.nth selected_indices 5 |> List.nth row |> float_opt_of_string;
    }
  in
  List.fold_left
    (fun acc row ->
      try extract_row row :: acc with DateConverter.InvalidDate _ -> acc)
    [] rest
  |> List.rev

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

let string_of_field (name : string) (value : float option) : string =
  match value with
  | Some v when name = "Volume" ->
      Printf.sprintf "%s: %s" name (string_of_float v)
  | None when name = "Volume" -> Printf.sprintf "Volume: N/A"
  | Some v when name = "Date" ->
      Printf.sprintf "%s: %s, " name (DateConverter.date_to_string v)
  | Some v -> Printf.sprintf "%s: %s, " name (string_of_float v)
  | None -> Printf.sprintf "%s: N/A, " name

let string_of_row row =
  let fields =
    [
      ("Date", Some row.date);
      ("Open Price", row.open_price);
      ("High Price", row.high_price);
      ("Low Price", row.low_price);
      ("Close Price", row.close_price);
      ("Volume", row.volume);
    ]
  in
  List.fold_left
    (fun acc (name, value) -> acc ^ string_of_field name value)
    "" fields

let print_row row = string_of_row row ^ "\n" |> print_endline

let string_of_data data =
  List.fold_left (fun acc row -> acc ^ string_of_row row ^ "\n") "" data

let print_data data = string_of_data data |> print_endline
