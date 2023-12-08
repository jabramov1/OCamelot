(* module type StrategyType = sig type t

   val crossover : unit val dual_crossover : unit val mean_reversion : unit end

   module Strategy : StrategyType = struct type t = | Buy | Hold | Sell

   let crossover = () let dual_crossover = () let mean_reversion = () end *)
