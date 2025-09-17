(* ma.ml *)

open Chronos
(* open Chronos_core.Strategy *)
open Lwt
open Lwt.Syntax
open Chronos.Ma_crossover_strategy.Ma_crossover_strategy

let ensure_dir dir =
  if Sys.file_exists dir then () else Unix.mkdir dir 0o755

let make_kv fast slow qty =
  [ ("fast", string_of_int fast); ("slow", string_of_int slow); ("qty", string_of_int qty) ]

let run_one filename config fast slow qty =
  (* mutate config.local_config to include fast/slow if needed *)
  let kvs = make_kv fast slow qty in
  match Chronos.Ma_crossover_strategy.Ma_crossover_strategy.local_state_of_kv kvs with
  | Error msg ->
      Printf.eprintf "Invalid params fast=%d slow=%d qty=%d : %s\n%!" fast slow qty msg;
      Lwt.return_unit  (* or handle as you like *)
  | Ok local_state ->
      Backtesting_engine.run filename config local_state >>= fun () ->
        (* Backtesting_engine.run wrote positions.csv; move it to results/ *)
        let results_dir = "results" in
        ensure_dir results_dir;
        let src = "positions.csv" in
        let dst = Printf.sprintf "%s/positions_fast%d_slow%d.csv" results_dir fast slow in
        (* rename (move) file; Sys.rename raises on error *)
        (try Sys.rename src dst; Lwt.return_unit
         with e ->
           Printf.eprintf "Failed to move positions file: %s\n%!" (Printexc.to_string e);
           Lwt.return_unit)
  (* move positions.csv to results/positions_fastXXX_slowYYY.csv as you already had *)
  

let run_grid filename config =
  let fast_vals = [5;10;15;20;25;30] in
  let slow_vals = [50;75;100;150;200] in
  let rec loop_f = function
    | [] -> Lwt.return_unit
    | f::fs ->
      let rec loop_s = function
        | [] -> Lwt.return_unit
        | s::ss ->
          if f >= s then loop_s ss
          else
            run_one filename config f s 10 >>= fun () ->
            loop_s ss
      in
      loop_s slow_vals >>= fun () ->
      loop_f fs
  in
  loop_f fast_vals

let () =
  Lwt_main.run (
    let filename = "historical_data_reliance.csv" in
    let config : (Chronos.Ma_crossover_strategy.Ma_crossover_strategy.local_config) Chronos_core.Strategy.config = {
      Chronos_core.Strategy.data_layer_uri = "ws://127.0.0.1:8000/candles/stream";
      oms_layer_uri = "http://localhost:9000/order/place";
      oms_ws_uri = "ws://localhost:8081/";
      symbol = "RELIANCE";
      local_config = ( () : local_config );
    } in
    run_grid filename config)
