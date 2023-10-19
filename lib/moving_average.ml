module type MovingAverage = sig
  val simple_moving_avg : unit
  val exp_moving_avg : unit
  val weighted_moving_avg : unit
  val triangular_moving_avg : unit
  val variable_moving_avg : unit
end
