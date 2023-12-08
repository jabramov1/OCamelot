module type StrategyType = sig
  type t

  val crossover : unit
  val dual_crossover : unit
  val mean_reversion : unit
end
