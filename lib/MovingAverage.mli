open CsvReader

module type MovingAverageType = sig
  val simple_moving_avg : CsvReader.t -> int -> float option list
  val exp_moving_avg : CsvReader.t -> int -> float option list
  val weighted_moving_avg : CsvReader.t -> int -> float option list
  val triangular_moving_avg : CsvReader.t -> int -> float option list
  val vol_adj_moving_avg : CsvReader.t -> int -> float option list
end

module MovingAverage : MovingAverageType
