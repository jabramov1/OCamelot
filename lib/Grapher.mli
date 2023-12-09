type moving_avg_type =
  | Simple
  | Exponential
  | Weighted
  | Triangular
  | VolumeAdjusted

val graph : ?m_averages:(moving_avg_type * int) list -> CsvReader.t -> unit
(** [graph ?m_averages data] plots the given data along with specified moving
    averages.
    @param m_averages
      is an optional parameter specifying a list of moving average types and
      their respective periods. Defaults to 50, 100, and 200 day moving
      averages.
    @param data is the CSV data to be plotted. *)
