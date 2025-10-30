(* vwap_strategy.ml *)
[@@@ocaml.warning "-32-37-69"]
open Chronos_core
open Strategy_sig

(** VWAP + EMA Pullback-Resumption Strategy with TP/SL and max-hold
    - input candles: 15m CSV rows with RFC3339 timestamp,open,high,low,close,volume,...
    - bias computed on 1H using VWAP_1h and EMA20_1h/EMA50_1h
    - entry (long):
        prev 15m bar touches EMA20_15m (prev.low <= ema20_15m <= prev.high)
        AND current close > prev.high (breakout)
        AND current volume >= vol_mult * vol20_avg
    - exits:
        TP percent (intrabar check using high for longs, low for shorts)
        SL percent (intrabar check)
        Timeout at max_hold_bars -> exit at close
*)

(* ---------- types ---------- *)
module Vwap_strategy : S = struct
  type candle = {
    timestamp : Ptime.t;
    open_p : float;
    high : float;
    low : float;
    close : float;
    volume : float;
  }

  type event = Market_data_event of candle

  type trade_side = Long | Short

  type open_trade = {
    entry_time : Ptime.t;
    entry_price : float;
    side : trade_side;
    qty : float;
    bars_held : int;  (* increments each 15m candle *)
  }

  type last_side = LongSide | ShortSide | Neutral

  type local_state = {
    (* 15m indicators *)
    ema20_15m : float option;
    ema50_15m : float option;
    vol20_window : float list;        (* rolling volumes *)

    (* last 15m candle (previous candle used for pullback checks) *)
    last_15m_candle : candle option;

    (* 1H accumulators: aggregate 15m bars into hourly VWAP & update 1h EMAs *)
    current_hour_start : Ptime.t option;
    hour_price_vol_acc : float;
    hour_vol_acc : float;
    bars_in_hour : int;

    (* 1H indicators *)
    ema20_1h : float option;
    ema50_1h : float option;
    vwap_1h : float option;

    (* open trades tracked by strategy (for TP/SL/timeout) *)
    open_trades : open_trade list;

    (* strategy parameters *)
    vol_mult : float;   (* e.g., 1.5 *)
    qty : float;
    tp_pct : float;     (* e.g., 1.0 for 1% *)
    sl_pct : float;     (* e.g., 0.5 for 0.5% *)
    max_hold_bars : int;(* number of 15m bars to hold before timeout *)

    last_side : last_side;
    symbol : string;

  (* balances for spot simulation *)
    quote_balance : float;   (* USDT-like *)
    base_balance  : float;   (* BTC-like *)
    trade_notional: float;   (* USD notional per trade, e.g. 20.0 *)
  }

  let default_local_state = {
    ema20_15m = None;
    ema50_15m = None;
    vol20_window = [];
    last_15m_candle = None;
    current_hour_start = None;
    hour_price_vol_acc = 0.0;
    hour_vol_acc = 0.0;
    bars_in_hour = 0;
    ema20_1h = None;
    ema50_1h = None;
    vwap_1h = None;
    open_trades = [];
    vol_mult = 1.0;
    qty = 0.1;
    tp_pct = 1.0;
    sl_pct = 0.5;
    max_hold_bars = 12;  (* default 12 x 15m = 3 hours *)
    last_side = Neutral;
    symbol = "BTCUSDT";
    (* initialize balances: $100 USDT and ~0.0011 BTC (~$100 at ~90k) *)
    quote_balance = 100.0;
    base_balance = 0.0011;
    trade_notional = 20.0;
  }

  type local_config = unit

  (* ---------- helpers ---------- *)

  let alpha_of_period n = 2.0 /. (float_of_int n +. 1.0)

  let update_ema prev_ema value period =
    match prev_ema with
    | None -> value
    | Some p ->
      let a = alpha_of_period period in
      (value *. a) +. (p *. (1.0 -. a))

  let take_last n lst =
    let len = List.length lst in
    if len <= n then lst
    else
      let drop = len - n in
      let rec drop_n d l =
        if d <= 0 then l else match l with | [] -> [] | _ :: t -> drop_n (d-1) t
      in
      drop_n drop lst

  let read_float_safe s =
    try float_of_string s with _ -> 0.0

  (* make a completed order for backtesting *)
  let make_completed_order_for_side ~symbol ~qty ~price side =
    Order.make_completed_order ~tradingsymbol:symbol ~quantity:qty ~lots:0 ~price ~side ~strategy_name:"vwap_pullback"

  (* trade side conversion helpers *)
  let opposite_side = function
    | Long -> Order.Sell
    | Short -> Order.Buy

 (* fees: 0.1% per side *)
  let fee_rate_one_side = 0.001 (* 0.1% *)

  (* ---------- Strategy_sig implementation ---------- *)

  let lookup key kvs =
    try Some (List.assoc key kvs) with Not_found -> None

  let int_of_string_result s =
    try Ok (int_of_string s) with Failure _ -> Error ("invalid int: " ^ s)

  let float_of_string_result s =
    try Ok (float_of_string s) with Failure _ -> Error ("invalid float: " ^ s)

  let validate_params ~vol_mult ~qty ~tp_pct ~sl_pct ~max_hold_bars =
    if vol_mult <= 0.0 then Error "vol_mult must be > 0"
    else if tp_pct <= 0.0 then Error "tp_pct must be > 0"
    else if qty <= 0.0 then Error "qty must be > 0"
    else if sl_pct <= 0.0 then Error "sl_pct must be > 0"
    else if max_hold_bars < 1 then Error "max_hold_bars must be >= 1"
    else Ok ()

  let local_state_of_kv kvs =
    let vol_mult = match lookup "vol_mult" kvs with Some v -> (try float_of_string v with _ -> default_local_state.vol_mult) | None -> default_local_state.vol_mult in
    let qty = match lookup "qty" kvs with Some v -> (try float_of_string v with _ -> default_local_state.qty) | None -> default_local_state.qty in
    let tp_pct = match lookup "tp_pct" kvs with Some v -> (try float_of_string v with _ -> default_local_state.tp_pct) | None -> default_local_state.tp_pct in
    let sl_pct = match lookup "sl_pct" kvs with Some v -> (try float_of_string v with _ -> default_local_state.sl_pct) | None -> default_local_state.sl_pct in
    let max_hold_bars = match lookup "max_hold_bars" kvs with Some v -> (try int_of_string v with _ -> default_local_state.max_hold_bars) | None -> default_local_state.max_hold_bars in

    let init_quote = match lookup "init_quote" kvs with Some v -> (try float_of_string v with _ -> default_local_state.quote_balance) | None -> default_local_state.quote_balance in
    let init_base  = match lookup "init_base" kvs with Some v -> (try float_of_string v with _ -> default_local_state.base_balance) | None -> default_local_state.base_balance in
    let tnot = match lookup "trade_notional" kvs with Some v -> (try float_of_string v with _ -> default_local_state.trade_notional) | None -> default_local_state.trade_notional in

    match validate_params ~vol_mult ~qty ~tp_pct ~sl_pct ~max_hold_bars with
    | Error e -> Error e
    | Ok () ->
      Ok { default_local_state with vol_mult; qty; tp_pct; sl_pct; max_hold_bars; quote_balance = init_quote; base_balance = init_base; trade_notional = tnot }

  (* CSV row -> event *)
  let read_row (row : string list) : event option =
    match row with
    | date :: _open :: _high :: _low :: close_s :: vol_s :: _ ->
      begin
        match Ptime.of_rfc3339 date with
        | Ok (ptime, _, _) ->
          (* Need high/low/close/volume too; extract them from positions *)
          let open_p = read_float_safe (List.nth row 1) in
          let high = read_float_safe (List.nth row 2) in
          let low = read_float_safe (List.nth row 3) in
          let close = read_float_safe close_s in
          let volume = read_float_safe vol_s in
          Some (Market_data_event { timestamp = ptime; open_p; high; low; close; volume })
        | Error _ -> None
        end
    | _ -> None

  let get_timestamp_from_event = function
    | Market_data_event c -> Some c.timestamp

  (* Helper: compute bars difference between two Ptime.t values (rounded down to 15m bars)
   Using Ptime.to_float_s for portability across ptime versions.
*)
  let bars_between ~from_t ~to_t =
    (* Convert to seconds since epoch as float and compute difference.
     If to_t is earlier than from_t, return 0. *)
    let from_s = Ptime.to_float_s from_t in
    let to_s = Ptime.to_float_s to_t in
    let diff = to_s -. from_s in
    if diff <= 0.0 then 0
    else
      let secs = int_of_float diff in
      secs / (15 * 60)

  (* ---------- on_event: core logic ---------- *)

  let on_event (state : local_state Strategy.state) (event : event) : local_state Strategy.state =

    match event with
    | Market_data_event candle ->
      (* Printf.printf "Candle is %f\n%!" candle.volume; *)

      (* update 15m EMAs *)
      let close = candle.close in
      let ema20_15m_v = Some (update_ema state.local_state.ema20_15m close 20) in
      let ema50_15m_v = Some (update_ema state.local_state.ema50_15m close 50) in

      (* update vol20 *)
      let vol20_window = take_last 20 (state.local_state.vol20_window @ [candle.volume]) in
      let vol20_avg = if List.length vol20_window = 0 then 0.0 else (List.fold_left (+.) 0.0 vol20_window) /. float_of_int (List.length vol20_window) in

      (* compute hour start timestamp (floor to hour) *)
      let hour_start =
        match Ptime.to_date_time candle.timestamp with
        | ((y,m,d), ((hh,_,_), _)) ->
          let ts_str = Printf.sprintf "%04d-%02d-%02dT%02d:00:00Z" y m d hh in
          (match Ptime.of_rfc3339 ts_str with Ok (t,_,_) -> Some t | Error _ -> None)
      in

      (* accumulate or rotate hour aggregates *)
      let (current_hour_start_v, hour_price_vol_acc_v, hour_vol_acc_v, bars_in_hour_v, ema20_1h_v, ema50_1h_v, vwap_1h_v) =
        match state.local_state.current_hour_start with
        | None ->
          (hour_start, (candle.close *. candle.volume), candle.volume, 1, state.local_state.ema20_1h, state.local_state.ema50_1h, state.local_state.vwap_1h)
        | Some hs ->
          if Some hs = hour_start then
            (state.local_state.current_hour_start, state.local_state.hour_price_vol_acc +. (candle.close *. candle.volume), state.local_state.hour_vol_acc +. candle.volume, state.local_state.bars_in_hour + 1, state.local_state.ema20_1h, state.local_state.ema50_1h, state.local_state.vwap_1h)
          else
            (* finalize previous hour -> compute prev_vwap and update 1h EMAs *)
            let prev_vwap = if state.local_state.hour_vol_acc > 0.0 then Some (state.local_state.hour_price_vol_acc /. state.local_state.hour_vol_acc) else None in
            let ema20_1h_new = match state.local_state.ema20_1h, prev_vwap with
              | None, Some v -> Some v
              | Some prev, Some v -> Some (update_ema (Some prev) v 20)
              | _ -> state.local_state.ema20_1h
            in
            let ema50_1h_new = match state.local_state.ema50_1h, prev_vwap with
              | None, Some v -> Some v
              | Some prev, Some v -> Some (update_ema (Some prev) v 50)
              | _ -> state.local_state.ema50_1h
            in
            (hour_start, (candle.close *. candle.volume), candle.volume, 1, ema20_1h_new, ema50_1h_new, prev_vwap)
      in

      (* build intermediate local_state with updated indicators *)
      let updated_ls = {
        state.local_state with
        ema20_15m = ema20_15m_v;
        ema50_15m = ema50_15m_v;
        vol20_window = vol20_window;
        last_15m_candle = (match state.local_state.last_15m_candle with _ -> Some candle);
        current_hour_start = current_hour_start_v;
        hour_price_vol_acc = hour_price_vol_acc_v;
        hour_vol_acc = hour_vol_acc_v;
        bars_in_hour = bars_in_hour_v;
        ema20_1h = ema20_1h_v;
        ema50_1h = ema50_1h_v;
        vwap_1h = vwap_1h_v;
      } in

      (* first: check existing open trades for exit conditions (intrabar checks) *)
      let rec check_exits open_trs acc_orders remaining_trs =
        match remaining_trs with
        | [] -> (List.rev acc_orders, List.rev open_trs)
        | tr :: rest ->
          let tp_price = match tr.side with | Long -> tr.entry_price *. (1.0 +. updated_ls.tp_pct /. 100.0) | Short -> tr.entry_price *. (1.0 -. updated_ls.tp_pct /. 100.0) in
          let sl_price = match tr.side with | Long -> tr.entry_price *. (1.0 -. updated_ls.sl_pct /. 100.0) | Short -> tr.entry_price *. (1.0 +. updated_ls.sl_pct /. 100.0) in
          (* intrabar TP/SL check using current candle high/low *)
          let tp_hit =
            match tr.side with
            | Long -> candle.high >= tp_price
            | Short -> candle.low <= tp_price
          in
          let sl_hit =
            match tr.side with
            | Long -> candle.low <= sl_price
            | Short -> candle.high >= sl_price
          in
          (* check max hold *)
          let bars_held_now = tr.bars_held + 1 in
          let timeout = bars_held_now >= updated_ls.max_hold_bars in
          if tp_hit then
            let close_price = tp_price in
            let gross = tr.qty *. close_price in
            let fee = gross *. fee_rate_one_side in
            let net_notional = gross -. fee in
            let qty = net_notional /. candle.close in
            let exit_order = make_completed_order_for_side ~symbol:(state.local_state.symbol) ~qty ~price:close_price (opposite_side tr.side) in
            (* do not add trade back to open list *)
            check_exits open_trs (exit_order @ acc_orders) rest
          else if sl_hit then
            let close_price = sl_price in
            let gross = tr.qty *. close_price in
            let fee = gross *. fee_rate_one_side in
            let net_notional = gross -. fee in
            let qty = net_notional /. candle.close in
            let exit_order = make_completed_order_for_side ~symbol:(state.local_state.symbol) ~qty ~price:close_price (opposite_side tr.side) in
            check_exits open_trs (exit_order @ acc_orders) rest
          else if timeout then
            let close_price = candle.close in
            let gross = tr.qty *. close_price in
            let fee = gross *. fee_rate_one_side in
            let net_notional = gross -. fee in
            let qty = net_notional /. candle.close in
            let exit_order = make_completed_order_for_side ~symbol:(state.local_state.symbol) ~qty ~price:close_price (opposite_side tr.side) in
            check_exits open_trs (exit_order @ acc_orders) rest
          else
            (* update bars_held and keep trade open *)
            let updated_trade = { tr with bars_held = bars_held_now } in
            check_exits (updated_trade :: open_trs) acc_orders rest
      in

      let (exit_orders, remaining_open_trades) = check_exits [] [] updated_ls.open_trades in

      let _ =
        Printf.eprintf "t=%s close=%.2f ema20_15m=%s ema20_1h=%s ema50_1h=%s vwap_1h=%s vol=%.3f vol20=%.3f\n%!"
          (Ptime.to_rfc3339 candle.timestamp)
          candle.close
          (match updated_ls.ema20_15m with Some v -> Printf.sprintf "%.2f" v | None -> "None")
          (match updated_ls.ema20_1h with Some v -> Printf.sprintf "%.2f" v | None -> "None")
          (match updated_ls.ema50_1h with Some v -> Printf.sprintf "%.2f" v | None -> "None")
          (match updated_ls.vwap_1h with Some v -> Printf.sprintf "%.2f" v | None -> "None")
          candle.volume vol20_avg in

     (* second: detect new entry signal (only if bias present and prev_candle exists) *)
      let entry_orders, _=
        match updated_ls.ema20_15m, updated_ls.ema50_15m, updated_ls.ema20_1h, updated_ls.ema50_1h, updated_ls.vwap_1h, updated_ls.last_15m_candle with
        | Some ema20_15m_v , Some _ (* ema50_15m_v *), Some ema20_1h_v, Some ema50_1h_v, Some vwap_1h_v, Some prev_candle ->
          (* compute bias from 1h *)
          (* let bias_opt = *)
          (*   if (ema20_15m_v > ema50_15m_v) then Some `Long *)
          (*   else if (ema20_15m_v < ema50_15m_v) then Some `Short *)
          (*   else None *)
          (* in *)
          let bias_opt =
            if (candle.close > vwap_1h_v) && (ema20_1h_v > ema50_1h_v) then Some `Long
            else if (candle.close < vwap_1h_v) && (ema20_1h_v < ema50_1h_v) then Some `Short
            else None in
          (* pullback condition *)
          let pullback = (prev_candle.low <= ema20_15m_v) && (ema20_15m_v <= prev_candle.high) in
          (* let pullback = true in *)
          let vol_ok = if vol20_avg <= 0.0 then false else (candle.volume >= updated_ls.vol_mult *. vol20_avg) in

          (* let vol_ok = *)
          (*   if vol20_avg <= 0.0 then true *)
          (*   else candle.volume >= (max 1.0 updated_ls.vol_mult) *. vol20_avg in *)
          (* let vol_ok = true in *)
          begin match bias_opt with
            | Some `Long when pullback (* && (candle.close > prev_candle.high) *) && vol_ok ->
              (* compute required base qty from trade_notional and ensure it's convertible to int qty *)
                let gross = updated_ls.trade_notional in
                let fee = gross *. fee_rate_one_side in
                let net_notional = gross -. fee in
                let qty = net_notional /. candle.close in
                if qty <= 0.0 then
                  (Printf.eprintf "SKIP_ENTRY Long: qty %.8f rounds to zero \n%!" qty; ([], remaining_open_trades))
                else if updated_ls.quote_balance < (qty *. candle.close) *. (1.0 +. fee_rate_one_side) then
                  (Printf.eprintf "SKIP_ENTRY Long: insufficient quote balance (need %.6f got %.6f)\n%!"
                     ((qty*. candle.close) *. (1.0 +. fee_rate_one_side)) updated_ls.quote_balance;
                   ([], remaining_open_trades))
                else
                  let orders = make_completed_order_for_side ~symbol:state.local_state.symbol ~qty ~price:candle.close Order.Buy in
                  let new_trade = { entry_time = candle.timestamp; entry_price = candle.close; side = Long; qty = qty; bars_held = 0 } in
                  (orders, new_trade :: remaining_open_trades)

            | Some `Short when pullback (* && (candle.close < prev_candle.low) *) && vol_ok ->
              (* ENTRY Short: compute qty from trade_notional minus entry fee; sell that base qty *)
              let gross = updated_ls.trade_notional in
              let fee = gross *. fee_rate_one_side in
              let net_notional = gross -. fee in
              let qty = net_notional /. candle.close in
              Printf.eprintf "ENTRY Short: gross=%.4f fee=%.6f net_notional=%.4f price=%.2f qty=%.8f\n%!" gross fee net_notional candle.close qty;
              let orders = make_completed_order_for_side ~symbol:state.local_state.symbol ~qty ~price:candle.close Order.Sell in
              let new_trade = { entry_time = candle.timestamp; entry_price = candle.close; side = Short; qty = qty; bars_held = 0 } in
              (orders, new_trade :: remaining_open_trades)
            | _ ->
              ([], remaining_open_trades)
            end
        | _ -> ([], remaining_open_trades)
      in

      (* assemble pending_orders (exit orders first, then entry orders) *)
      let pending_orders = exit_orders @ entry_orders @ state.pending_orders in

      (* Apply fills (simulate trade execution & fees) by folding over all orders produced 
         NOTE: No fee calculations here because we assumed the feeling when making completed orders. *)
      let apply_fill_to_balances ls (order : Order.t) =
        (* If the order was created by make_completed_order, filled_quantity/fill price available *)
        let q = order.filled_quantity in
        let p = order.filled_price in
        match order.side with
        | Order.Buy ->
          let cost = q *. p in
          { ls with quote_balance = ls.quote_balance -. (cost); base_balance = ls.base_balance +. q }
        | Order.Sell ->
          let proceeds = q *. p in
          { ls with base_balance = ls.base_balance -. q; quote_balance = ls.quote_balance +. (proceeds) }
      in

      let all_fills = exit_orders @ entry_orders in
      let ls_after_fills = List.fold_left apply_fill_to_balances updated_ls all_fills in

      (* final updated local_state *)
      let final_local_state = { ls_after_fills with open_trades = remaining_open_trades } in

      { state with pending_orders = pending_orders; local_state = final_local_state }

  (* extract orders and clear pending list (same pattern as ma_crossover) *)
  let extract_orders (state : local_state Strategy.state) : Order.t list * local_state Strategy.state =
    let orders_to_extract = state.pending_orders in
    let new_state = { state with pending_orders = [] } in
    (orders_to_extract, new_state)

  let create config local_state =
  Strategy.create config local_state extract_orders
end
