(* ma.ml *)

open Chronos
(* open Chronos_core.Strategy *)
open Lwt
open Lwt.Syntax
open Chronos.Ma_crossover_strategy.Ma_crossover_strategy

let () =
  Lwt_main.run (
    let filename = "historical_data.csv" in
    let config : (Chronos.Ma_crossover_strategy.Ma_crossover_strategy.local_config, 'local_state) Chronos_core.Strategy.config = {
      Chronos_core.Strategy.data_layer_uri = "ws://127.0.0.1:8000/candles/stream";
      oms_layer_uri = "http://localhost:9000/order/place";
      oms_ws_uri = "ws://localhost:8081/";
      symbol = "RELIANCE";
      local_config = ( () : local_config );
    } in
    Backtesting_engine.run filename config
)
