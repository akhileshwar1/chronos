(* ma_crossover_strategy.ml *)

open Strategy

type last_side = High | Low | Neutral

(* Local state specific to AlternateStrategy *)
type local_state = {
  sma_5 : float list;
  sma_20: float list;
  last_side : last_side;
}

(* Config specific to AlternateStrategy *)
type local_config = unit

(* Define the candle type for this strategy *)
type candle = {
  timestamp : Ptime.t;
  open_price : float;
  high_price : float;
  low_price : float;
  close_price : float;
}

(* Event type specific to this strategy *)
type event =
  | Market_data_event of candle
  | Timer_tick_event of float

(* Initialize the strategy state *)
let initial_local_state = {
  sma_5 = [];
  sma_20 = [];
  last_side = Neutral;
}

(* Convert JSON to candle type *)
(*"timestamp": "2025-05-28T12:30:00+04:00"*)
let json_to_candle (json : Yojson.Safe.t) : candle =
  let open Yojson.Safe.Util in
  let timestamp_str = json |> member "date" |> to_string in
  match Ptime.of_rfc3339 timestamp_str with
  | Ok (ptime, _, _) ->
      {
        timestamp = ptime;
        open_price = json |> member "open" |> to_float;
        high_price = json |> member "high" |> to_float;
        low_price = json |> member "low" |> to_float;
        close_price = json |> member "close" |> to_float;
}
  | Error _ ->
      failwith ("Invalid timestamp format: " ^ timestamp_str)

(* Convert JSON to event *)
let json_to_event (json : Yojson.Safe.t) : event =
  let candle = json_to_candle json in
  Market_data_event candle

let calculate_sma list : float =
  let total = List.fold_left ( +. ) 0.0 list in
  total /. float_of_int (List.length list)

let accumulate list len item =
  if List.length list < len then
    list @ [item]
  else (* pop the first and insert at the end *)
    match list with
      | [] -> [item]
      | _ :: t -> t @ [item]

(* Process the event and transform the state *)
let on_event (state : 'local_state Strategy.state) (event : event) : 'local_state Strategy.state =
  match event with
  | Market_data_event candle ->
    let sma_5_list = accumulate state.local_state.sma_5 5 candle.close_price in
    let sma_20_list = accumulate state.local_state.sma_20 20 candle.close_price in
    let sma5 = calculate_sma sma_5_list in
    let sma20 = calculate_sma sma_20_list in
    let last_side = state.local_state.last_side in
    let make_completed_order side= 
      Order.make_completed_order
      ~tradingsymbol:"RELIANCE"
      ~quantity:1
      ~lots:0 
      ~price: candle.close_price 
      ~side: side 
      ~strategy_name:"bb" in
    Printf.printf "List length %.2d\n%!" (List.length sma_20_list);
    if List.length sma_5_list < 5 || List.length sma_20_list < 20 then
      {state with local_state = {sma_5 = sma_5_list; sma_20 = sma_20_list; last_side = last_side}}
    else if last_side != High && sma5 > sma20 then
      (Printf.printf " Crossing UP! \n%! ";
      {state with pending_orders = make_completed_order Order.Buy  @ state.pending_orders;
                  local_state = {sma_5 = sma_5_list; sma_20 = sma_20_list; last_side = High}})
    else if last_side != Low && sma5 < sma20 then
      (Printf.printf " Crossing DOWN! \n%! ";
      {state with pending_orders = make_completed_order Order.Sell @ state.pending_orders;
                  local_state = {sma_5 = sma_5_list; sma_20 = sma_20_list; last_side = Low}})
    else
      state
  | Timer_tick_event _ -> 
    (* You can implement timer-based logic if needed in the future *)
    state

let extract_orders (state : 'local_state Strategy.state) : Order.t list * 'local_state Strategy.state =
  let orders_to_extract = state.pending_orders in
  let new_state = { state with pending_orders = [] } in (* Create a new state with pending_orders cleared *)
  (orders_to_extract, new_state) (* Return the orders and the new state *)

(* The final strategy packaged together *)
let create (config : ('local_config, 'local_state) Strategy.config) : ('local_config, 'local_state) Strategy.t =  Strategy.create
    config
    initial_local_state
    extract_orders
