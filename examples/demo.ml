open Ocamelot.Grapher
open Ocamelot.CsvReader

let _ =
  let csv_data =
    CsvReader.read_csv "./data/SPY.csv" ~date:"Date" ~open_price:"Open"
      ~high_price:"High" ~low_price:"Low" ~close_price:"Close"
      ~adj_price:"Adj Close" ~volume:"Volume"
  in
  Grapher.graph csv_data
