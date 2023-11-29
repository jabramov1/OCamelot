open CsvReader

module type MovingAverageType = sig
  val simple_moving_avg : CsvReader.t -> int -> float option list
  val exp_moving_avg : CsvReader.t -> int -> float option list
  val weighted_moving_avg : unit
  val triangular_moving_avg : unit
  val variable_moving_avg : unit
  val mean : unit
end

module MovingAverage : MovingAverageType
