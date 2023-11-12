open Ocamelot.CsvReader
open OUnit2

(** TODO: create more test csvs with different header orders, lack of header,
    etc. *)
let empty_csv = CsvReader.read_csv "../data/test/empty.csv"

let no_header_csv =
  CsvReader.read_csv "../data/test/no_header.csv" ~date:"Date"
    ~open_price:"Open" ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
    ~adj_price:"Adj Close" ~volume:"Volume"

let no_data_csv =
  CsvReader.read_csv "../data/test/no_data.csv" ~date:"Date" ~open_price:"Open"
    ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
    ~adj_price:"Adj Close" ~volume:"Volume"

let clean_csv =
  CsvReader.read_csv "../data/test/clean.csv" ~date:"Date" ~open_price:"Open"
    ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
    ~adj_price:"Adj Close" ~volume:"Volume"

let messy_csv =
  CsvReader.read_csv "../data/test/messy.csv" ~date:"Date" ~open_price:"Open"
    ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
    ~adj_price:"Adj Close" ~volume:"Volume"

(** Can probably make this concise with higher order functions *)
(* let test_getter outs f _ = let inputs = [ messy_csv; messy_csv; messy_csv ]
   in

   let rec test_inputs = function | [], [] -> () | input :: inputs, expected ::
   expected_outputs -> let result_ = f input in assert_equal expected result; *)

module CsvReaderTester = struct
  let test_getter f data out =
    "getter test" >:: fun _ -> assert_equal out (f data)

  let getter_tests = [ test_getter CsvReader.get_date clean_csv "" ]
end

let get_row_tests = []

(* let get_date_tests = [ "test" >:: getter_test [ "3" ] CsvReader.get_dates
   test_csv_1 ] *)

let get_open_price_tests = []
let get_high_price_tests = []
let get_low_price_tests = []
let get_closing_price_tests = []
let get_adj_price_tests = []
let get_volume_price_tests = []
let get_dates_tests = []
let get_open_prices_tests = []
let get_high_prices_tests = []
let get_low_prices_tests = []
let get_closing_prices_tests = []
let get_adj_prices_tests = []
let get_volume_prices_tests = []
let head_tests = []
let tail_tests = []
let print_data_tests = []
let print_row_tests = []
let print_string_list_tests = []
let csv_tests = List.flatten []
let suite = "main test suite" >::: List.flatten []
