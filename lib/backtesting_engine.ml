(* This file creates a concrete engine by applying the functor. *)
open Ma_crossover_strategy

(* expose at the top level *)
include Backtesting_engine_functor.Backtesting_engine_functor (Ma_crossover_strategy)
