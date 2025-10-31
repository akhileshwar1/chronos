(* engine.ml functor *)

open Strategy_sig
open Chronos_core

module Backtesting_engine_functor (S : S) = struct
  let read_csv (filename : string) : S.event list =
    let table = Csv.load filename in
    table
    |> List.tl
    |> List.filter_map S.read_row

  let process_order (oc : out_channel) (_ : ('local_config, 'local_state) Chronos_core.Strategy.t ref) (order : Chronos_core.Order.t) (event : S.event) : unit =
    let trade = Trade.of_order ~fee_rate_one_side:0.001 order in
    let timestamp = S.get_timestamp_from_event event in
    match timestamp with
    | Some date ->
      let timestamp_str = Ptime.to_rfc3339 date in
      let line = Trade.to_csv_line trade timestamp_str in
      output_string oc line
    | None -> ()

  let process_event (oc : out_channel) (current_strategy_ref : ('local_config, 'local_state) Chronos_core.Strategy.t ref) (event : S.event) : unit =
    let old_state = (!current_strategy_ref).state in
    let new_state_after_event = S.on_event old_state event in
    current_strategy_ref := Chronos_core.Strategy.update_state !current_strategy_ref new_state_after_event;

    let orders, new_state_after_extraction = S.extract_orders (!current_strategy_ref).state in
    current_strategy_ref := Chronos_core.Strategy.update_state !current_strategy_ref new_state_after_extraction;
    List.iter (fun order -> process_order oc current_strategy_ref order event) orders

  let run (filename : string) (config : ('local_config ) Chronos_core.Strategy.config) (local_state : 'local_state) : unit Lwt.t =
    let events = read_csv filename in
    let strategy = S.create config local_state in
    let current_strategy = ref strategy in

    let oc = open_out "positions.csv" in
    Printf.fprintf oc "timestamp,symbol,side,trade_type,qty,price,gross_notional,fee,net_notional,strategy\n";
    List.iter (fun event -> process_event oc current_strategy event) events;
    close_out oc;
    Printf.printf "Positions written to positions.csv\n";
    Lwt.return_unit
end
