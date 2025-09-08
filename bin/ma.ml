(* ma.ml engine *)

open Chronos.Ma_crossover_strategy
open Lwt

let read_csv (filename : string) : Chronos.Ma_crossover_strategy.candle list =
  let table = Csv.load filename in
  (* skip header row with List.tl *)
  table
  |> List.tl
  |> List.filter_map (function
    | [date; open_; high; low; close] ->
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
  : Chronos.Position.pos =
    let old_state = (!current_strategy_ref).state in
    let%lwt new_state_after_event = Chronos.Ma_crossover_strategy.on_event old_state event in
    current_strategy_ref := Chronos.Strategy.update_state !current_strategy_ref new_state_after_event;

    let orders, new_state_after_extraction = Chronos.Ma_crossover_strategy.extract_orders (!current_strategy_ref).Chronos.Strategy.state in
    current_strategy_ref := Chronos.Strategy.update_state !current_strategy_ref new_state_after_extraction;

let () =
  let candles = read_csv "historical_data.csv" in
  List.iter (fun c ->
    Printf.printf "open=%.2f close=%.2f high=%.2f low=%.2f\n"
      c.open_price c.close_price c.high_price c.low_price
  ) candles
