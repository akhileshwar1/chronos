(* vwap.ml *)

open Chronos
open Lwt
open Lwt.Syntax

(* adjust module path if your vwap strategy lives under a different module name *)
open Chronos.Vwap_strategy.Vwap_strategy

let ensure_dir dir =
  if Sys.file_exists dir then () else Unix.mkdir dir 0o755

let make_kv ~vol_mult ~qty ~tp_pct ~sl_pct ~max_hold_bars =
  [
    ("vol_mult", string_of_float vol_mult);
    ("qty", string_of_int qty);
    ("tp_pct", string_of_float tp_pct);
    ("sl_pct", string_of_float sl_pct);
    ("max_hold_bars", string_of_int max_hold_bars);
  ]

let run_one filename (config : (local_config) Chronos_core.Strategy.config) ~vol_mult ~qty ~tp_pct ~sl_pct ~max_hold_bars =
  let kvs = make_kv ~vol_mult ~qty ~tp_pct ~sl_pct ~max_hold_bars in
  match Chronos.Vwap_strategy.Vwap_strategy.local_state_of_kv kvs with
  | Error msg ->
      Printf.eprintf "Invalid params vol_mult=%.2f qty=%d tp=%.2f sl=%.2f hold=%d : %s\n%!"
        vol_mult qty tp_pct sl_pct max_hold_bars msg;
      Lwt.return_unit
  | Ok local_state ->
      Backtesting_engine_vwap.run filename config local_state >>= fun () ->
      let results_dir = "results" in
      ensure_dir results_dir;
      let src = "positions.csv" in
      let dst =
        Printf.sprintf "%s/vwap_vol%.2f_qty%d_tp%.2f_sl%.2f_hold%d.csv"
          results_dir vol_mult qty tp_pct sl_pct max_hold_bars
      in
      (try Sys.rename src dst; Lwt.return_unit
       with e ->
         Printf.eprintf "Failed to move positions file: %s\n%!" (Printexc.to_string e);
         Lwt.return_unit)

let run_grid filename config =
  (* small example grid — change to suit your tuning budget *)
  let vol_mult_vals = [1.2; 1.5] in
  let qty_vals = [1; 10] in
  let tp_vals = [1.0; 1.2] in
  let sl_vals = [0.5] in
  let hold_vals = [12; 24] in

  let rec loop_vol = function
    | [] -> Lwt.return_unit
    | vm :: vms ->
      let rec loop_qty = function
        | [] -> Lwt.return_unit
        | q :: qs ->
          let rec loop_tp = function
            | [] -> Lwt.return_unit
            | tp :: tps ->
              let rec loop_sl = function
                | [] -> Lwt.return_unit
                | sl :: sls ->
                  let rec loop_hold = function
                    | [] -> Lwt.return_unit
                    | h :: hs ->
                      run_one filename config ~vol_mult:vm ~qty:q ~tp_pct:tp ~sl_pct:sl ~max_hold_bars:h >>= fun () ->
                      loop_hold hs
                  in
                  loop_hold hold_vals >>= fun () ->
                  loop_sl sls
              in
              loop_sl sl_vals >>= fun () ->
              loop_tp tps
          in
          loop_tp tp_vals >>= fun () ->
          loop_qty qs
      in
      loop_qty qty_vals >>= fun () ->
      loop_vol vms
  in
  loop_vol vol_mult_vals

let () =
  Lwt_main.run (
    let filename = "btc_15m.csv" in
    let config : (Chronos.Vwap_strategy.Vwap_strategy.local_config) Chronos_core.Strategy.config = {
      Chronos_core.Strategy.data_layer_uri = "ws://127.0.0.1:8000/candles/stream";
      oms_layer_uri = "http://localhost:9000/order/place";
      oms_ws_uri = "ws://localhost:8081/";
      symbol = "BTCUSDT"; (* change symbol as needed *)
      local_config = ( () : local_config );
    } in
    run_grid filename config)
