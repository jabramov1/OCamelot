open Ocamelot.CsvReader
open OUnit2

let general_csv =
  CsvReader.read_csv ~date:"   the date   " ~open_price:"Open" ~high_price:"hi"
    ~low_price:"Low" ~close_price:" Close" ~adj_price:"adj" ~volume:"vol1234 "
    "data/test/general.csv"

module CsvReaderTester = struct
  let string_of_string s = s

  let string_of_opt f s =
    match s with
    | None -> "None"
    | Some x -> "Some " ^ f x

  let string_of_list f lst =
    "[" ^ List.fold_left (fun acc elem -> acc ^ f elem) "" lst ^ "]"

  (* let test_size out csv = "size test" >:: fun _ -> assert_equal
     ~printer:string_of_int out (CsvReader.size csv)

     let test_get_row ~n out csv = let size = CsvReader.size csv in if n < 0 ||
     n >= size then "get row test, invalid index" >:: fun _ -> assert_raises
     Not_found (fun _ -> CsvReader.get_row csv n) else "get row test" >:: fun _
     -> assert_equal out (CsvReader.get_row csv n) *)

  let test_row_getter ~p ~f ~fname ~idx out csv =
    let len = CsvReader.size csv in
    let row = List.nth (CsvReader.head csv len) idx in
    fname ^ " test" >:: fun _ -> assert_equal ~printer:p out (f row)

  (* let test_col_getter ~p ~f ~col out csv = col >:: fun _ -> assert_equal
     ~printer:p out (f csv) *)

  let test_head ~p ~n out csv =
    "head test" >:: fun _ -> assert_equal ~printer:p out (CsvReader.head csv n)

  let test_tail ~p ~n out csv =
    "tail test" >:: fun _ -> assert_equal ~printer:p out (CsvReader.tail csv n)

  (* let test_print_data out csv = "print data test" >:: fun _ -> assert_equal
     out (CsvReader.print_data csv)

     let test_print_row out csv = "print row test" >:: fun _ -> assert_equal out
     (CsvReader.print_row csv) *)

  let get_date_tests =
    [
      test_row_getter
        ~p:(string_of_opt string_of_string)
        ~f:CsvReader.get_date ~fname:"date getter" ~idx:0 (Some "2018-10-01")
        general_csv;
      test_row_getter
        ~p:(string_of_opt string_of_string)
        ~f:CsvReader.get_date ~fname:"date getter" ~idx:4 (Some "2018-10-05")
        general_csv;
      test_row_getter
        ~p:(string_of_opt string_of_string)
        ~f:CsvReader.get_date ~fname:"date getter" ~idx:5 None general_csv;
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
        ~p:(string_of_opt string_of_int)
        ~f:CsvReader.get_volume ~fname:"volume getter" ~idx:0 (Some 62078900)
        general_csv;
      test_row_getter
        ~p:(string_of_opt string_of_int)
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

  let col_getter_tests = List.flatten []

  let head_tests =
    [
      test_head
        ~p:(string_of_list CsvReader.string_of_row)
        ~n:(-1) [] general_csv;
      test_head ~p:(string_of_list CsvReader.string_of_row) ~n:0 [] general_csv;
      test_head
        ~p:(string_of_list CsvReader.string_of_row)
        ~n:9 general_csv general_csv;
      test_head
        ~p:(string_of_list CsvReader.string_of_row)
        ~n:100 general_csv general_csv;
    ]

  let tail_tests =
    [
      test_tail
        ~p:(string_of_list CsvReader.string_of_row)
        ~n:(-1) [] general_csv;
      test_tail ~p:(string_of_list CsvReader.string_of_row) ~n:0 [] general_csv;
      test_tail
        ~p:(string_of_list CsvReader.string_of_row)
        ~n:9 general_csv general_csv;
      test_tail
        ~p:(string_of_list CsvReader.string_of_row)
        ~n:100 general_csv general_csv;
    ]

  let print_data_tests = []
  let print_row_tests = []

  let all_tests =
    List.flatten
      [
        row_getter_tests;
        col_getter_tests;
        head_tests;
        tail_tests;
        print_data_tests;
        print_row_tests;
      ]
end

let csv_tests = List.flatten [ CsvReaderTester.all_tests ]
let suite = "main test suite" >::: List.flatten [ csv_tests ]
let _ = run_test_tt_main suite

(* let empty_csv = CsvReader.read_csv "../data/test/empty.csv" *)

(* let no_header_csv = CsvReader.read_csv "../data/test/no_header.csv"
   ~date:"Date" ~open_price:"Open" ~high_price:"High" ~low_price:"Low"
   ~close_price:"Close" ~adj_price:"Adj Close" ~volume:"Volume" *)

(* let no_data_csv = CsvReader.read_csv "../data/test/no_data.csv" ~date:"Date"
   ~open_price:"Open" ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
   ~adj_price:"Adj Close" ~volume:"Volume" *)
(* let messy_csv = CsvReader.read_csv "../data/test/messy.csv" ~date:"Date"
   ~open_price:"Open" ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
   ~adj_price:"Adj Close" ~volume:"Volume" *)

(** Can probably make this concise with higher order functions *)
(* let test_getter outs f _ = let inputs = [ messy_csv; messy_csv; messy_csv ]
   in

   let rec test_inputs = function | [], [] -> () | input :: inputs, expected ::
   expected_outputs -> let result_ = f input in assert_equal expected result; *)
