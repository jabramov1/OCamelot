open Ocamelot
open Ocamelot.Utils
open OUnit2

(** TODO: make_row tests*)

let general_csv =
  CsvReader.read_csv ~date:" the date " ~open_price:"Open" ~high_price:"hi"
    ~low_price:"Low" ~close_price:" Close" ~volume:"vol1234 "
    "data/test/general.csv"

module DateConverterTester = struct
  let test_convert ~date_type out date =
    "date conversion test" >:: fun _ ->
    assert_equal
      ~printer:(fun s -> s)
      out
      (DateConverter.string_to_date ~date_type date
      |> DateConverter.date_to_string)

  let test_convert_raises ~date_type ~err_str date =
    "date conversion raises test" >:: fun _ ->
    assert_raises (DateConverter.InvalidDate err_str) (fun _ ->
        DateConverter.string_to_date ~date_type date)

  let all_tests =
    [
      test_convert ~date_type:"YYYY-MM-DD" "2018-10-01" "2018-10-01";
      test_convert ~date_type:"YY-MM-DD" "2018-10-01" "18-10-01";
      test_convert ~date_type:"YYYY/MM/DD" "2020-12-12" "2020/12/12";
      test_convert ~date_type:"YY/MM/DD" "2020-12-12" "20/12/12";
      test_convert ~date_type:"MM-DD-YYYY" "2003-09-10" "09-10-2003";
      test_convert ~date_type:"MM/DD/YYYY" "1923-09-26" "9/26/1923 16:00";
      test_convert ~date_type:"MM/DD/YY" "2023-09-20" "9/20/23 16:00";
      test_convert ~date_type:"DD-MM-YYYY" "1972-04-02" "2-4-1972 ";
      test_convert ~date_type:"DD/MM/YYYY" "2024-09-12" "12/09/2024";
      test_convert ~date_type:"YYYY-DD-MM" "2023-01-03" "2023-03-1 4:00";
      test_convert ~date_type:"YYYY/DD/MM" "1923-12-12" "1923/12/12";
      test_convert ~date_type:"MMM DD, YYYY" "2004-12-04" "DEC 4, 2004";
      test_convert ~date_type:"MMM DD, YYYY" "2004-01-04" "jaN 04, 2004";
      test_convert_raises ~date_type:"MM/DD/YYYY"
        ~err_str:"Invalid date. Component is not of proper length."
        "04/ 15/    1923";
      test_convert_raises ~date_type:"YYYY-MM-DD"
        ~err_str:"Invalid date: 20180-10-01T00:00:00Z" "20180-10-01";
      test_convert_raises ~date_type:"MMM DD, YYYY"
        ~err_str:"Invalid date input." "dece 4, 2023";
    ]
end

module CsvReaderTester = struct
  let date = DateConverter.string_to_date ~date_type:"YYYY-MM-DD"
  let date_to_string = DateConverter.date_to_string

  let test_size out csv =
    "size test" >:: fun _ ->
    assert_equal ~printer:string_of_int out (CsvReader.size csv)

  let test_get_row ~n out csv =
    let size = CsvReader.size csv in
    if n < 0 || n >= size then
      "get row test, invalid index" >:: fun _ ->
      assert_raises Not_found (fun _ -> CsvReader.get_row csv n)
    else
      "get row test" >:: fun _ ->
      assert_equal
        ~printer:(fun s -> s)
        out
        (CsvReader.get_row csv n |> CsvReader.string_of_row)

  let test_row_getter ~p ~f ~fname ~idx out csv =
    let size = CsvReader.size csv in
    let row = List.nth (CsvReader.head csv size) idx in
    fname ^ " test" >:: fun _ -> assert_equal ~printer:p out (f row)

  let test_col_getter ~p ~f ~fname ~fst ~lst csv =
    let col = f csv in
    let size = List.length col in
    let first = List.nth col 0 in
    let last = List.nth col (size - 1) in
    [
      ( fname ^ " test size" >:: fun _ ->
        assert_equal ~printer:string_of_int (CsvReader.size csv) size );
      (fname ^ " test first" >:: fun _ -> assert_equal ~printer:p fst first);
      (fname ^ " test last" >:: fun _ -> assert_equal ~printer:p lst last);
    ]

  let test_head_tail ~p ~n ~f csv =
    let rows =
      if f = "head" then CsvReader.head csv n else CsvReader.tail csv n
    in
    let rec test_gen ~p ~f rows acc =
      match rows with
      | [] -> acc
      | h :: t ->
          ( f ^ " test" >:: fun _ ->
            assert_equal ~printer:p (CsvReader.get_row rows (List.length acc)) h
          )
          :: test_gen ~p ~f t acc
    in
    test_gen ~p ~f rows []

  let size_tests =
    [
      test_size 8 general_csv;
      test_size 0 (CsvReader.head general_csv 0);
      test_size 3 (CsvReader.head general_csv 3);
      test_size 8 (CsvReader.head general_csv 9);
    ]

  let get_row_tests =
    [
      test_get_row ~n:(-1) "" general_csv;
      test_get_row ~n:0
        "Date: 2018-10-01, Open Price: 292.109985, High Price: 292.929993, Low \
         Price: 290.980011, Close Price: 291.730011, Volume: 62078900."
        general_csv;
      test_get_row ~n:7
        "Date: 2018-10-11, Open Price: N/A, High Price: 278.899994, Low Price: \
         270.359985, Close Price: 272.170013, Volume: 274840500."
        general_csv;
      test_get_row ~n:100 "" general_csv;
    ]

  let get_date_tests =
    [
      test_row_getter ~p:date_to_string ~f:CsvReader.get_date
        ~fname:"date getter" ~idx:0 (date "2018-10-01") general_csv;
      test_row_getter ~p:date_to_string ~f:CsvReader.get_date
        ~fname:"date getter" ~idx:4 (date "2018-10-05") general_csv;
      test_row_getter ~p:date_to_string ~f:CsvReader.get_date
        ~fname:"date getter" ~idx:5 (date "2018-10-06") general_csv;
    ]

  let get_open_price_tests =
    [
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_open_price ~fname:"open price getter" ~idx:0
        (Some 292.109985) general_csv;
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_open_price ~fname:"open price getter" ~idx:5 None
        general_csv;
    ]

  let get_high_price_tests =
    [
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_high_price ~fname:"high price getter" ~idx:1
        (Some 292.359985) general_csv;
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_high_price ~fname:"high price getter" ~idx:2 None
        general_csv;
    ]

  let get_low_price_tests =
    [
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_low_price ~fname:"low price getter" ~idx:1
        (Some 291.140015) general_csv;
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_low_price ~fname:"low price getter" ~idx:6 None
        general_csv;
    ]

  let get_closing_price_tests =
    [
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_closing_price ~fname:"closing price getter" ~idx:0
        (Some 291.730011) general_csv;
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_closing_price ~fname:"closing price getter" ~idx:6 None
        general_csv;
    ]

  let get_volume_tests =
    [
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_volume ~fname:"volume getter" ~idx:0 (Some 62078900.)
        general_csv;
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_volume ~fname:"volume getter" ~idx:2 None general_csv;
    ]

  let row_getter_tests =
    List.flatten
      [
        get_date_tests;
        get_open_price_tests;
        get_high_price_tests;
        get_low_price_tests;
        get_closing_price_tests;
        get_volume_tests;
      ]

  let col_getter_tests =
    List.flatten
      [
        test_col_getter ~p:date_to_string ~f:CsvReader.get_dates
          ~fname:"dates col getter" ~fst:(date "2018-10-01")
          ~lst:(date "2018-10-11") general_csv;
        test_col_getter
          ~p:(string_of_opt string_of_float)
          ~f:CsvReader.get_open_prices ~fname:"open prices col getter"
          ~fst:(Some 292.109985) ~lst:None general_csv;
        test_col_getter
          ~p:(string_of_opt string_of_float)
          ~f:CsvReader.get_high_prices ~fname:"high\n prices col getter"
          ~fst:(Some 292.929993) ~lst:(Some 278.899994) general_csv;
        test_col_getter
          ~p:(string_of_opt string_of_float)
          ~f:CsvReader.get_low_prices ~fname:"low prices col getter"
          ~fst:(Some 290.980011) ~lst:(Some 270.359985) general_csv;
        test_col_getter
          ~p:(string_of_opt string_of_float)
          ~f:CsvReader.get_closing_prices ~fname:"closing prices col getter"
          ~fst:(Some 291.730011) ~lst:(Some 272.170013) general_csv;
        test_col_getter
          ~p:(string_of_opt string_of_float)
          ~f:CsvReader.get_volumes ~fname:"volumes col getter"
          ~fst:(Some 62078900.) ~lst:(Some 274840500.) general_csv;
      ]

  let head_tests =
    List.flatten
      [
        test_head_tail ~p:CsvReader.string_of_row ~n:(-1) ~f:"head" general_csv;
        test_head_tail ~p:CsvReader.string_of_row ~n:0 ~f:"head" general_csv;
        test_head_tail ~p:CsvReader.string_of_row ~n:3 ~f:"head" general_csv;
        test_head_tail ~p:CsvReader.string_of_row ~n:9 ~f:"head" general_csv;
        test_head_tail ~p:CsvReader.string_of_row ~n:100 ~f:"head" general_csv;
      ]

  let tail_tests =
    List.flatten
      [
        test_head_tail ~p:CsvReader.string_of_row ~n:(-1) ~f:"tail" general_csv;
        test_head_tail ~p:CsvReader.string_of_row ~n:0 ~f:"tail" general_csv;
        test_head_tail ~p:CsvReader.string_of_row ~n:3 ~f:"tail" general_csv;
        test_head_tail ~p:CsvReader.string_of_row ~n:9 ~f:"tail" general_csv;
        test_head_tail ~p:CsvReader.string_of_row ~n:100 ~f:"tail" general_csv;
      ]

  let all_tests =
    List.flatten
      [
        row_getter_tests;
        col_getter_tests;
        get_row_tests;
        size_tests;
        head_tests;
        tail_tests;
      ]
end

module MovingAverageTester = struct
  let test_avg ~w_size ~fname out csv =
    fname ^ " (window size: " ^ Printf.sprintf "%d" w_size ^ ")" >:: fun _ ->
    assert_equal
      ~printer:(string_of_list string_of_float_opt)
      out
      begin
        let avg =
          match fname with
          | "SMA" -> MovingAverage.simple_moving_avg csv w_size
          | "EMA" -> MovingAverage.exp_moving_avg csv w_size
          | "WMA" -> MovingAverage.weighted_moving_avg csv w_size
          | "TMA" -> MovingAverage.triangular_moving_avg csv w_size
          | "VAMA" -> MovingAverage.vol_adj_moving_avg csv w_size
          | _ -> failwith "Invalid input."
        in
        List.map (format_float_opt ~prec:3) avg
      end

  let sma_tests =
    [
      test_avg ~w_size:(-1) ~fname:"SMA" [] general_csv;
      test_avg ~w_size:0 ~fname:"SMA" [] general_csv;
      test_avg ~w_size:1 ~fname:"SMA"
        [
          Some 291.730;
          Some 291.560;
          Some 291.720;
          Some 289.440;
          Some 287.820;
          Some 287.820;
          None;
          Some 272.170;
        ]
        general_csv;
      test_avg ~w_size:3 ~fname:"SMA"
        [
          Some 291.67;
          Some 290.907;
          Some 289.66;
          Some 288.36;
          Some 287.82;
          Some 279.995;
        ]
        general_csv;
      test_avg ~w_size:5 ~fname:"SMA"
        [ Some 290.454; Some 289.672; Some 289.2; Some 284.313 ]
        general_csv;
      test_avg ~w_size:25 ~fname:"SMA" [ Some 287.466 ] general_csv;
    ]

  let ema_tests =
    [
      test_avg ~w_size:(-1) ~fname:"EMA" [] general_csv;
      test_avg ~w_size:0 ~fname:"EMA" [] general_csv;
      test_avg ~w_size:1 ~fname:"EMA"
        [
          Some 291.730;
          Some 291.560;
          Some 291.720;
          Some 289.440;
          Some 287.820;
          Some 287.820;
          None;
          Some 272.17;
        ]
        general_csv;
      test_avg ~w_size:4 ~fname:"EMA"
        [ Some 290.444; Some 289.394; Some 288.764; None; Some 282.127 ]
        general_csv;
      test_avg ~w_size:12 ~fname:"EMA" [ Some 284.067 ] general_csv;
    ]

  let wma_tests =
    [
      test_avg ~w_size:(-1) ~fname:"WMA" [] general_csv;
      test_avg ~w_size:0 ~fname:"WMA" [] general_csv;
      test_avg ~w_size:1 ~fname:"WMA"
        [
          Some 291.730;
          Some 291.560;
          Some 291.720;
          Some 289.440;
          Some 287.820;
          Some 287.820;
          None;
          Some 272.170;
        ]
        general_csv;
      test_avg ~w_size:3 ~fname:"WMA"
        [
          Some 291.672;
          Some 291.26;
          Some 290.31;
          Some 288.63;
          Some 287.82;
          Some 282.603;
        ]
        general_csv;
      test_avg ~w_size:20 ~fname:"WMA" [ Some 289.968 ] general_csv;
    ]

  let tma_tests =
    [
      test_avg ~w_size:(-1) ~fname:"TMA" [] general_csv;
      test_avg ~w_size:0 ~fname:"TMA" [] general_csv;
      test_avg ~w_size:1 ~fname:"TMA"
        [
          Some 291.730;
          Some 291.560;
          Some 291.720;
          Some 289.440;
          Some 287.820;
          Some 287.820;
          None;
          Some 272.170;
        ]
        general_csv;
      test_avg ~w_size:4 ~fname:"TMA"
        [ Some 291.288; Some 290.283; Some 289.01; Some 288.09; Some 283.908 ]
        general_csv;
      test_avg ~w_size:5 ~fname:"TMA"
        [ Some 290.746; Some 289.642; Some 288.613; Some 285.392 ]
        general_csv;
      test_avg ~w_size:25 ~fname:"TMA" [ Some 287.466 ] general_csv;
      test_avg ~w_size:26 ~fname:"TMA" [ Some 287.466 ] general_csv;
    ]

  let vama_tests =
    [
      test_avg ~w_size:(-1) ~fname:"VAMA" [] general_csv;
      test_avg ~w_size:0 ~fname:"VAMA" [] general_csv;
      test_avg ~w_size:1 ~fname:"VAMA"
        [
          Some 291.730;
          Some 291.560;
          None;
          Some 289.440;
          None;
          Some 287.820;
          None;
          Some 272.170;
        ]
        general_csv;
      test_avg ~w_size:3 ~fname:"VAMA"
        [
          Some 291.657;
          Some 290.071;
          Some 289.44;
          Some 288.727;
          Some 287.82;
          Some 275.957;
        ]
        general_csv;
      test_avg ~w_size:9 ~fname:"VAMA" [ Some 281.477 ] general_csv;
    ]

  let all_tests =
    List.flatten [ sma_tests; ema_tests; wma_tests; tma_tests; vama_tests ]
end

let all_tests =
  List.flatten
    [
      DateConverterTester.all_tests;
      CsvReaderTester.all_tests;
      MovingAverageTester.all_tests;
    ]

let suite = "main test suite" >::: all_tests
let _ = run_test_tt_main suite
