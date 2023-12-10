module Gp = Gnuplot

type moving_avg_type =
  | Simple
  | Exponential
  | Weighted
  | Triangular
  | VolumeAdjusted

(** [prompt_for_moving_average] prompts the user to enter a moving average type
    and period. *)
let prompt_for_moving_average () =
  print_endline
    "Enter moving average type (Simple, Exponential, Weighted, Triangular, \
     VolumeAdjusted):";
  let ma_type_str = read_line () in
  let ma_type =
    match ma_type_str |> String.lowercase_ascii with
    | "simple" -> Simple
    | "exponential" -> Exponential
    | "weighted" -> Weighted
    | "triangular" -> Triangular
    | "volumeadjusted" -> VolumeAdjusted
    | _ -> failwith "Invalid moving average type"
  in
  print_endline "Enter moving average period:";
  let period = read_int () in
  (ma_type, period)

(** [ma_label] converts a given moving average type and the period length to its
    string representation. *)
let ma_label (ma_type, period) =
  let ma_type_label =
    match ma_type with
    | Simple -> "SMA"
    | Exponential -> "EMA"
    | Weighted -> "WMA"
    | Triangular -> "TMA"
    | VolumeAdjusted -> "VAMA"
  in
  string_of_int period ^ "-day " ^ ma_type_label

(** [convert_data d] converts each row of the CSV data representation [d] to a
    readable format for GNUPlot, in the form (date, (opening price, high price,
    low price, closing price)). *)
let convert_data data =
  let convert_row row =
    let date = CsvReader.get_date row in
    let op = Option.value (CsvReader.get_open_price row) ~default:0. in
    let hi = Option.value (CsvReader.get_high_price row) ~default:0. in
    let lo = Option.value (CsvReader.get_low_price row) ~default:0. in
    let cl = Option.value (CsvReader.get_closing_price row) ~default:0. in
    (date, (op, hi, lo, cl))
  in
  List.map convert_row data

(** [match_moving_avg ma_type] matches a moving average type to its
    corresponding function. *)
let match_moving_avg (ma_type : moving_avg_type) :
    CsvReader.t -> int -> float option list =
  match ma_type with
  | Simple -> MovingAverage.simple_moving_avg
  | Exponential -> MovingAverage.exp_moving_avg
  | Weighted -> MovingAverage.weighted_moving_avg
  | Triangular -> MovingAverage.triangular_moving_avg
  | VolumeAdjusted -> MovingAverage.vol_adj_moving_avg

(** [tuple_lists l1 l2] two lists to a singular list of tuples, such that the
    first element of each tuple is an element in [l1] and the second element of
    each tupole is an element in [l2]*)
let rec tuple_lists l1 l2 =
  if List.length l1 > List.length l2 then
    match l1 with
    | _ :: t -> tuple_lists t l2
    | _ -> failwith "error"
  else if List.length l1 < List.length l2 then
    match l2 with
    | _ :: t -> tuple_lists l1 t
    | _ -> failwith "error"
  else
    match (l1, l2) with
    | h1 :: t1, h2 :: t2 -> (h1, h2) :: tuple_lists t1 t2
    | _ -> []

(** [colors] is a list of polymorphic algebraic types representing the types of
    colors that can be used in the moving average graph for GNUplot. *)
let colors = [ `Magenta; `Green; `Red; `Cyan; `Yellow ]

(** [filter_float_list d] converts a float option list to a float list by
    eliminating all [None] values and converting values of [Some x] to [x]. *)
let filter_float_list (data : float option list) : float list =
  List.filter_map (fun x -> x) data

(** [string_of_ma_list ma_list] converts a list of moving average types and
    their periods to a string representation. *)
let string_of_ma_list ma_list =
  let indexed_list = List.mapi (fun i ma -> (i + 1, ma)) ma_list in
  List.fold_left
    (fun acc (i, ma) -> acc ^ string_of_int i ^ " : " ^ ma_label ma ^ "\n")
    "" indexed_list

(** [interactive_ma_input m_averages] takes a list of moving averages and their
    periods, and accepts user input to interact with this list. If the user
    inputs "add" then the user will be prompted to add a new moving average to
    [m_averages]. If the user inputs "remove", then the user will be prompted to
    enter the number of the moving average to remove. If the user inputs "done",
    then the corresponding graph GUI will appear. Invalid inputs will cause the
    user input dialog to continue on again. *)
let rec interactive_ma_input m_averages =
  print_endline
    ("Here are indicators: \n"
    ^ string_of_ma_list m_averages
    ^ "Enter command (Add, Remove, Done):");
  match read_line () |> String.lowercase_ascii with
  | "add" ->
      let ma = prompt_for_moving_average () in
      interactive_ma_input (ma :: m_averages)
  | "remove" ->
      print_endline (string_of_ma_list m_averages);
      print_endline "Enter the number of the MA to remove:";
      let index = read_int () in
      let new_m_averages =
        List.mapi (fun i ma -> (i, ma)) m_averages
        |> List.filter (fun (i, _) -> i <> index - 1)
        |> List.map snd
      in
      interactive_ma_input new_m_averages
  | "done" -> m_averages
  | _ ->
      print_endline "Invalid command";
      interactive_ma_input m_averages

let graph ?(m_averages = [ (Simple, 50); (Simple, 100); (Simple, 200) ]) data =
  let m_averages = interactive_ma_input m_averages in
  let plot_data = convert_data data in
  let dates = List.map fst plot_data in
  let moving_avg_data_lists =
    List.map
      (fun (ma_type, ma_period) ->
        let ma_fun = match_moving_avg ma_type in
        filter_float_list (ma_fun data ma_period))
      m_averages
  in
  let data_mas =
    List.map (fun ma_data -> tuple_lists dates ma_data) moving_avg_data_lists
  in
  let ma_labels = List.map ma_label m_averages in
  let gp = Gp.create () in
  let candle_data = Gp.Series.candles_date_ohlc ~color:`Blue plot_data in
  let ma_series =
    List.mapi
      (fun i ma_data ->
        Gp.Series.lines_datey ~title:(List.nth ma_labels i)
          ~color:(List.nth colors (i mod List.length colors))
          ma_data)
      data_mas
  in
  Gp.plot_many gp
    ~range:(Gp.Date (List.hd dates, List.hd (List.rev dates)))
    ~format:"%b %d'%y" (candle_data :: ma_series);
  Unix.sleep 1000
