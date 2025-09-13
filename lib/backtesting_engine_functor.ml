(* engine.ml functor *)

open Strategy_sig

module Backtesting_engine_functor (S : S) = struct
  let read_csv (filename : string) : S.event list =
    let table = Csv.load filename in
    table
    |> List.tl
    |> List.filter_map S.read_row

  let process_order (oc : out_channel) (current_strategy_ref : (S.local_config, S.local_state) Chronos_core.Strategy.t ref) (order : Chronos_core.Order.t) (event : S.event) : unit =
    let state = (!current_strategy_ref).state in
    let updated_positions = Chronos_core.Position.update_or_insert_position state.positions order in
    let updated_state = { state with positions = updated_positions } in
    current_strategy_ref := Chronos_core.Strategy.update_state !current_strategy_ref updated_state;
    let position = List.find (fun (pos : Chronos_core.Position.pos) -> String.equal pos.symbol order.tradingsymbol) updated_positions in
    let timestamp = S.get_timestamp_from_event event in
    match timestamp with
     | Some date ->
      let timestamp_str = Ptime.to_rfc3339 date in
      let line =
        Printf.sprintf "%s,%s,%.2f,%d,%.2f\n"
        timestamp_str
        position.symbol
        position.net_price
        position.net_qty
        position.pnl
      in
    output_string oc line
    | None -> ()

  let process_event (oc : out_channel) (current_strategy_ref : (S.local_config, S.local_state) Chronos_core.Strategy.t ref) (event : S.event) : unit =
    let old_state = (!current_strategy_ref).state in
    let new_state_after_event = S.on_event old_state event in
    current_strategy_ref := Chronos_core.Strategy.update_state !current_strategy_ref new_state_after_event;

    let orders, new_state_after_extraction = S.extract_orders (!current_strategy_ref).state in
    current_strategy_ref := Chronos_core.Strategy.update_state !current_strategy_ref new_state_after_extraction;
    List.iter (fun order -> process_order oc current_strategy_ref order event) orders

  let run (filename : string) (config : ('local_config, 'local_state) Chronos_core.Strategy.config) : unit Lwt.t =
    let events = read_csv filename in
    let strategy = S.create config in
    let current_strategy = ref strategy in

    let oc = open_out "positions.csv" in
    Printf.fprintf oc "timestamp,symbol,net_price,profit_loss\n";
    List.iter (fun event -> process_event oc current_strategy event) events;
    close_out oc;
    Printf.printf "Positions written to positions.csv\n";
    Lwt.return_unit
end
