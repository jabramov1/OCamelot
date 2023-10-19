open Csv

(* TODO: get column by title name *)
(* TODO: ignore header option *)

module type CsvReaderType = sig
  val read_csv : string -> string list list
  (** Read a csv file with file name [filename] and converts it to a 2D-array of
      strings *)

  val get_elem : string list list -> int -> int -> string
  (** Given a csv [csv], return the element in [csv] located in row [i] and
      column [j]. *)

  val get_col : ?header:bool -> string list list -> int -> string list
  (** Given a csv [csv] that's been read, return the column at specified index
      [n]. *)

  val get_row : string list list -> int -> string list
  (** Given a csv [csv] that's been read, return the row at specified index [n]. *)
end

(** This module contains functions for reading CSV files and working with their
    data. *)
module CsvReader : CsvReaderType = struct
  (* type t = { timestamp : float; price : float; } *)

  let read_csv (filename : string) : string list list =
    load ~separator:',' filename

  (** Given a list [lst], return the element in that [lst] located at index [n] *)
  let rec get (lst : 'a list) (n : int) : 'a =
    match (lst, n) with
    | [], _ -> failwith "Index out of bounds"
    | h :: _, 0 -> h
    | _ :: t, n -> get t (n - 1)

  let get_elem (csv : string list list) (i : int) (j : int) : string =
    let row = get csv i in
    get row j

  (** Given a csv [csv] that's been read, return the csv without the header*)
  let no_header (csv : string list list) : string list list =
    match csv with
    | [] -> []
    | _ :: t -> t

  let rec get_col ?(header = false) (csv : string list list) (n : int) :
      string list =
    let csv = if header then csv else no_header csv in
    match csv with
    | [] -> []
    | row :: rest -> get row n :: get_col rest n

  let get_row (csv : string list list) (n : int) : string list = get csv n
end
