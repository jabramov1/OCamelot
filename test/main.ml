open Ocamelot
open Ocamelot.Utils
open OUnit2

(** TEST PLAN:

    -- TESTING OVERVIEW --
    - OUnit was used to automatically test the functions in the following
      modules: DateConverter, CsvReader, and MovingAverage. For the majority of
      these tests, a black-box testing approach was used.
    - Manual testing was used to cover the following modules: DateConverter,
      CsvReader, MovingAverage, Grapher, Strategy, and BackTester. Some evidence
      of these tests can be seen in 'demo.ml'.

    -- OUNIT TESTING --

    DateConverter
    - Black box testing was used for different date formats.
    - Created tests for every acceptable date format for our system
    - Tested if unacceptable date formats raised the proper error
    - Since nearly all formats of dates were tested, our tests for this module
      support the notion that our system to be correct.

    CsvReader
    - For our source of data, we used a messy csv file, containing empty values,
      a header that was out of order with unorthodox labels, and values with
      whitespace. This ensured that our CSV data analysis functions worked to
      any generalized piece of data. This also ensured that the functions in
      other modules which used this CSV data representation as input would also
      be generalizable.
    - The only exception to this was with our tests for [make_row] and
      [make_csv]. These functions construct a CSV row/data representation of
      type [CsvReader.row] and [CsvReader.t], respectively, from a given list.
      We compared the output to those in the clean SPY.csv, but ensured that any
      invalid inputs would raise their proper errors.
    - A black-box testing approach was used, with edge cases tested as well
      (most commonly, negative input values and values larger than length of
      lists).

    MovingAverage
    - For our source of data, we constructed a CSV date representation from the
      same messy csv file used in the CsvReader tests.
    - We made sure all edge cases were tested (e.g. empty lists, small window
      sizes, very large window sizes) were all tested. This ensured our system
      would function properly for nearly any given list of prices.
    - We also made sure to include examples that would result in values of
      [None] within the result to ensure that the system would still function
      properly afterwards.
    - All moving average calculations were verified by hand, with proof of these
      on Google Docs.

    -- MANUAL TESTING --

    DateConverter
    - In our demo, 3 different CSV file formats were utilized. Different date
      formats were present. The parsing of these dates still worked properly
      with our CSV reader.

    CsvReader
    - In our demo, 3 different CSV file formats were read into a data
      representation, including a very messy CSV. By calculating the size,
      indexing rows, and retrieving the head and tail of the data (while
      printing these out), we were further able to verify the correctness of our
      CSV reader.

    MovingAverage
    - In our demo, we were able to use the grapher on 3 different CSV files to
      graph a variety of moving averages. These moving average lines followed
      the candlestick chart as we were expecting.
    - We verified that increasing the moving average period would cause the
      graph for it to start later and be shorter in duration.

    Grapher
    - The user input dialog allows users to add or remove types of moving
      averages with varying period lengths. We tested the dialog to make sure
      the resulting graph worked properly for all types of averages and a wide
      variety of period lengths worked (including negative, 0, and large
      numbers).
    - We tested this on 2 different CSV files with different formats, as well as
      a messy CSV file with many empty values.
    - We were able to verify that the correct values were being graphed by
      analyzing the y-axis values of many candlesticks.
    - By having our grapher work on a variety of CSV formats, including a very
      messy one, our manual tests for this module support the notion of our
      system being correct.

    Strategy/BackTester: *)

let general_csv =
  CsvReader.read_csv ~date:" the date " ~open_price:"Open" ~high_price:"hi"
    ~low_price:"Low" ~close_price:" Close" ~volume:"vol1234 "
    "data/test/general.csv"

let spy_csv =
  CsvReader.read_csv ~date:"Date" ~open_price:"Open" ~high_price:"High"
    ~low_price:"Low" ~close_price:"Close" ~volume:"Volume" "./data/SPY.csv"

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
      test_convert ~date_type:"MM/DD/YYYY" "1923-09-26" "9/26/1923 16:00";
      test_convert ~date_type:"MM/DD/YY" "2023-09-20" "9/20/23 16:00";
      test_convert ~date_type:"MM-DD-YYYY" "2003-09-10" "09-10-2003";
      test_convert ~date_type:"DD-MM-YYYY" "1972-04-02" "2-4-1972 ";
      test_convert ~date_type:"DD/MM/YYYY" "2024-09-12" "12/09/2024";
      test_convert ~date_type:"YYYY-DD-MM" "2023-01-03" "2023-03-1 4:00";
      test_convert ~date_type:"YYYY/DD/MM" "1923-12-12" "1923/12/12";
      test_convert ~date_type:"MMM DD, YYYY" "2004-12-04" "DEC 4, 2004";
      test_convert ~date_type:"MMM DD, YYYY" "2004-01-04" "jaN 04, 2004";
      test_convert_raises ~date_type:"YYYY-MM-DD"
        ~err_str:"Invalid date: 20180-10-01T00:00:00Z" "20180-10-01";
      test_convert_raises ~date_type:"MMM DD, YYYY"
        ~err_str:"Invalid date input." "dece 4 ,  2023";
    ]
end

module CsvReaderTester = struct
  let date = DateConverter.string_to_date ~date_type:"YYYY-MM-DD"
  let date_to_string = DateConverter.date_to_string

  let test_size out csv =
    "size test" >:: fun _ ->
    assert_equal ~printer:string_of_int out (CsvReader.size csv)

  let test_make_row ?(date_type = "YYYY-MM-DD") out row =
    "make row test" >:: fun _ ->
    assert_equal ~printer:CsvReader.string_of_row out
      (CsvReader.make_row ~date_type row)

  let test_make_csv ?(date_type = "YYYY-MM-DD") out data =
    "make csv test" >:: fun _ ->
    assert_equal ~printer:CsvReader.string_of_data out
      (CsvReader.make_csv ~date_type data)

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

  let make_row_tests =
    [
      test_make_row
        (CsvReader.get_row spy_csv 0)
        [
          "2018-10-01";
          "292.109985";
          "292.929993";
          "290.980011";
          "291.730011";
          "62078900.";
        ];
      test_make_row
        (CsvReader.get_row spy_csv 3)
        [
          "2018-10-04";
          "291.179993";
          "291.239990";
          "287.660004";
          "289.440002";
          "111545900.";
        ];
    ]

  let make_csv_tests =
    [
      test_make_csv (CsvReader.head spy_csv 2)
        [
          [
            "2018-10-01";
            "292.109985";
            "292.929993";
            "290.980011";
            "291.730011";
            "62078900";
          ];
          [
            "2018-10-02";
            "291.559998";
            "292.359985";
            "291.140015";
            "291.559998";
            "47258200";
          ];
        ];
      test_make_csv (CsvReader.tail spy_csv 2)
        [
          [
            "2023-09-28";
            "425.480011";
            "430.250000";
            "424.869995";
            "428.519989";
            "92258300";
          ];
          [
            "2023-09-29";
            "431.670013";
            "431.850006";
            "425.910004";
            "427.480011";
            "115078500";
          ];
        ];
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
        make_row_tests;
        make_csv_tests;
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
