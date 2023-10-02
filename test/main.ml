open Ocamelot.Csv_reader
open OUnit2

let data_path = "data/test.csv"
let csv = CsvReader.read_csv data_path

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let get_elem_tests = []

(** TODO: add more test cases, add ~msg parameter if possible**)
let get_col_tests =
  [
    ( "first column, with header" >:: fun _ ->
      assert_equal
        [
          "Date";
          "2018-10-01";
          "2018-10-02";
          "2018-10-03";
          "2018-10-04";
          "2018-10-05";
        ]
        (CsvReader.get_col ~header:true csv 0) );
    ( "first column, no header" >:: fun _ ->
      assert_equal
        [ "2018-10-01"; "2018-10-02"; "2018-10-03"; "2018-10-04"; "2018-10-05" ]
        (CsvReader.get_col ~header:false csv 0) );
  ]

let get_row_tests = []
let csv_tests = List.flatten [ get_elem_tests; get_col_tests; get_row_tests ]
let suite = "test suite for OCamelot" >::: List.flatten [ csv_tests ]
let () = run_test_tt_main suite
