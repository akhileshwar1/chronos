(* This file creates a concrete engine by applying the functor. *)
open Vwap_strategy 

(* expose at the top level *)
include Backtesting_engine_functor.Backtesting_engine_functor (Vwap_strategy)
