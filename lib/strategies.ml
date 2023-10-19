module type Strategies = sig
  val simple_moving_avg_crossover : unit
  val dual_moving_avg_crossover : unit
  val mean_reversion : unit
  val momentum : unit
  val inverse_volatility : unit
  val trend_following : unit
end
