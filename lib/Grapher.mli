open CsvReader

module type GrapherType = sig
  val graph :
    ?m_avg:((CsvReader.t -> int -> float option list) * int) list ->
    CsvReader.t ->
    unit
end

module Grapher : GrapherType
