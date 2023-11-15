open Ocamelot.CsvReader
open OUnit2

(** Ideal CSV example *)
let clean_csv =
  CsvReader.read_csv ~date:"Date" ~open_price:"Open" ~high_price:"High"
    ~low_price:"Low" ~close_price:"Close" ~adj_price:"Adj Close"
    ~volume:"Volume" "data/test/clean.csv"

module CsvReaderTester = struct
  let string_of_string s = s

  let string_of_opt f s =
    match s with
    | None -> "None"
    | Some x -> "Some " ^ f x

  let test_size out csv =
    "size test" >:: fun _ ->
    assert_equal ~printer:string_of_int out (CsvReader.size csv)

  let test_get_row ~n out csv =
    let size = CsvReader.size csv in
    if n < 0 || n >= size then
      "get row test, invalid index" >:: fun _ ->
      assert_raises Not_found (fun _ -> CsvReader.get_row csv n)
    else "get row test" >:: fun _ -> assert_equal out (CsvReader.get_row csv n)

  let test_row_getter ~p ~f ~fname ~idx out csv =
    let len = CsvReader.size csv in
    let row = List.nth (CsvReader.head csv len) idx in
    fname ^ " test" >:: fun _ -> assert_equal ~printer:p out (f row)

  let test_col_getter ~p ~f ~col out csv =
    col >:: fun _ -> assert_equal ~printer:p out (f csv)

  let test_head ~p ~n out csv =
    "head test" >:: fun _ -> assert_equal ~printer:p out (CsvReader.head csv n)

  let test_tail ~p ~n out csv =
    "tail test" >:: fun _ -> assert_equal ~printer:p out (CsvReader.tail csv n)

  let test_print_data out csv =
    "print data test" >:: fun _ -> assert_equal out (CsvReader.print_data csv)

  let test_print_row out csv =
    "print row test" >:: fun _ -> assert_equal out (CsvReader.print_row csv)

  let get_date_tests =
    [
      test_row_getter
        ~p:(string_of_opt string_of_string)
        ~f:CsvReader.get_date ~fname:"date getter" ~idx:0 (Some "2018-10-01")
        clean_csv;
      test_row_getter
        ~p:(string_of_opt string_of_string)
        ~f:CsvReader.get_date ~fname:"date getter" ~idx:4 (Some "2018-10-05")
        clean_csv;
    ]

  let get_open_price_tests =
    [
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_open_price ~fname:"open price getter" ~idx:0
        (Some 292.109985) clean_csv;
      test_row_getter
        ~p:(string_of_opt string_of_float)
        ~f:CsvReader.get_open_price ~fname:"open price getter" ~idx:3
        (Some 291.179993) clean_csv;
    ]

  let row_getter_tests = List.flatten [ get_date_tests; get_open_price_tests ]
  let col_getter_tests = List.flatten []
  let head_tests = []
  let tail_tests = []
  let print_data_tests = []
  let print_row_tests = []

  let all_tests =
    List.flatten [ row_getter_tests; col_getter_tests; head_tests ]
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
