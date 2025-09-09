(* ma.ml engine *)

open Chronos.Ma_crossover_strategy

let read_csv (filename : string) : Chronos.Ma_crossover_strategy.candle list =
  let table = Csv.load filename in
  (* skip header row with List.tl *)
  table
  |> List.tl
  |> List.filter_map (function
    | [date; open_; high; low; close; _] ->
        begin match Ptime.of_rfc3339 date with
        | Ok (ptime, _, _) ->
            Some { timestamp = ptime;
                   open_price = float_of_string open_;
                   high_price = float_of_string high;
                   low_price = float_of_string low;
                   close_price = float_of_string close
                 }
        | Error _ ->
            None (* Return None for invalid timestamps, filtering them out *)
        end
    | _ -> None) (* Return None for malformed CSV rows *)

let process_candle
  candle 
  (current_strategy_ref : (unit, Chronos.Ma_crossover_strategy.local_state) Chronos.Strategy.t ref)
  : Chronos.Position.pos list list =
    let old_state = (!current_strategy_ref).state in
    let event = Chronos.Ma_crossover_strategy.Market_data_event candle in
    let new_state_after_event = Chronos.Ma_crossover_strategy.on_event old_state event in
    current_strategy_ref := Chronos.Strategy.update_state !current_strategy_ref new_state_after_event;

    let orders, new_state_after_extraction = Chronos.Ma_crossover_strategy.extract_orders (!current_strategy_ref).Chronos.Strategy.state in
    current_strategy_ref := Chronos.Strategy.update_state !current_strategy_ref new_state_after_extraction;
    let positions =
      List.map (fun order -> Chronos.Position.update_or_insert_position new_state_after_extraction.positions order)
               orders
    in
    positions

let () =
  Lwt_main.run (
    let open Lwt.Syntax in
    let candles = read_csv "historical_data.csv" in
    let strategy_promise =
      let config = {
        Chronos.Strategy.data_layer_uri = "ws://127.0.0.1:8000/candles/stream";
        oms_layer_uri = "http://localhost:9000/order/place";
        oms_ws_uri = "ws://localhost:8081/";
        symbol = "RELIANCE";
        local_config = ();
      } in
      let strategy = Chronos.Ma_crossover_strategy.create config in
      Lwt.return strategy
    in
    let* strategy = strategy_promise in
    let current_strategy = ref strategy in

    let all_positions =
      List.fold_left (fun acc c ->
        Printf.printf "Processing candle: %s" (Ptime.to_rfc3339 c.timestamp);
        let new_positions = process_candle c current_strategy in
        (acc @ (List.flatten new_positions))
      ) [] candles
    in

    let oc = open_out "positions.csv" in 
    Printf.fprintf oc "timestamp,symbol,net_price,profit_loss\n"; 
    List.iter (fun (p : Chronos.Position.pos) ->
      let timestamp_str = Ptime.to_rfc3339 p.opened_at in
      let line =
        Printf.sprintf "%s,%s,%.2f,%.2f\n"
        timestamp_str
        p.symbol
        p.net_price
        p.pnl
      in
      output_string oc line
    ) all_positions;
    
    close_out oc;
    Printf.printf "Positions written to positions.csv\n"; 
    Lwt.return_unit)
