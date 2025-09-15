(* ma_crossover_strategy.ml *)
open Chronos_core
open Strategy_sig

module Ma_crossover_strategy : S = struct
  type last_side = High | Low | Neutral

  (* Local state specific to AlternateStrategy *)
  type local_state = {
    sma_5 : float list;
  sma_20: float list;
  last_side : last_side;
  }

  type local_config = unit 

  (* Define the candle type for this strategy *)
  type candle = {
    timestamp : Ptime.t;
    close_price : float;
  }

  (* Event type specific to this strategy *)
  type event =
  | Market_data_event of candle

  let read_row (row : string list) : event option =
    match row with
    | [date; _; _; _; close; _] ->
      begin
        match Ptime.of_rfc3339 date with
      | Ok (ptime, _, _) ->
          Some (Market_data_event {
            timestamp = ptime;
            close_price = float_of_string close
        })
      | Error _ -> None
      end
    | _ -> None

  let get_timestamp_from_event (event : event) : Ptime.t option =
    match event with
     | Market_data_event candle -> Some candle.timestamp

  (* Initialize the strategy state *)
  let initial_local_state = {
    sma_5 = [];
  sma_20 = [];
  last_side = Neutral;
          }

  let calculate_sma list : float =
    match list with
    | [] -> 0.0
    | _ -> 
        let total = List.fold_left ( +. ) 0.0 list in
        total /. float_of_int (List.length list)

  let accumulate list len item =
    if List.length list < len then
      list @ [item]
    else (* pop the first and insert at the end *)
      match list with
      | [] -> [item]
      | hd :: t -> 
          Printf.printf "accumulate: popping %.5f, pushing %.5f\n%!" hd item;
          t @ [item]


  let string_of_float_list lst =
    "[" ^ (String.concat "; " (List.map (fun x -> Printf.sprintf "%.2f" x) lst)) ^ "]"

      (* Process the event and transform the state *)
  let on_event (state : 'local_state Strategy.state) (event : event) : 'local_state Strategy.state =
    match event with
  | Market_data_event candle ->
      Printf.printf "RAW CANDLE: close=%.5f \n%!" candle.close_price;
      let sma_5_list = accumulate state.local_state.sma_5 5 candle.close_price in
      let sma_20_list = accumulate state.local_state.sma_20 20 candle.close_price in
      let sma5 = calculate_sma sma_5_list in
      let sma20 = calculate_sma sma_20_list in
      let last_side = state.local_state.last_side in
      let make_completed_order side= 
        Order.make_completed_order
      ~tradingsymbol:"RELIANCE"
      ~quantity:10
      ~lots:0 
      ~price: candle.close_price 
      ~side: side 
      ~strategy_name:"bb" in
      Printf.printf "sma5_list=%s sma20_list=%s\n%!"
       (string_of_float_list sma_5_list)
       (string_of_float_list sma_20_list);
      if List.length sma_5_list < 5 || List.length sma_20_list < 20 then
        {state with local_state = {sma_5 = sma_5_list; sma_20 = sma_20_list; last_side = last_side}}
      else if last_side <> High && sma5 > sma20 then
      (Printf.printf " Crossing UP! %.2f, %.2f \n%! " sma5 sma20;
      {state with pending_orders = make_completed_order Order.Buy  @ state.pending_orders;
                  local_state = {sma_5 = sma_5_list; sma_20 = sma_20_list; last_side = High}})
      else if last_side <> Low && sma5 < sma20 then
        (Printf.printf " Crossing DOWN! %.2f, %.2f \n%! " sma5 sma20;
      {state with pending_orders = make_completed_order Order.Sell @ state.pending_orders;
                  local_state = {sma_5 = sma_5_list; sma_20 = sma_20_list; last_side = Low}})
    else
      (Printf.printf "in else \n%!";
      {state with local_state = {sma_5 = sma_5_list; sma_20 = sma_20_list; last_side = last_side}})

  let extract_orders (state : 'local_state Strategy.state) : Order.t list * 'local_state Strategy.state =
    let orders_to_extract = state.pending_orders in
    let new_state = { state with pending_orders = [] } in (* Create a new state with pending_orders cleared *)
    (orders_to_extract, new_state) (* Return the orders and the new state *)

(* The final strategy packaged together *)
  let create (config : ('local_config, 'local_state) Strategy.config) : ('local_config, 'local_state) Strategy.t =  Strategy.create
    config
    initial_local_state
    extract_orders
end
