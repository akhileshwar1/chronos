(* lib/position.ml *)

let ptime_to_yojson (t : Ptime.t) = `String (Ptime.to_rfc3339 t)
let ptime_of_yojson = function
  | `String s -> (match Ptime.of_rfc3339 s with
      | Ok (ptime, _, _) -> Ok ptime
      | Error _ -> Error "ptime_of_yojson: Invalid RFC3339 string")
  | _ -> Error "ptime_of_yojson: Expected string"

let option_to_yojson f = function
  | None -> `Null
  | Some x -> f x

let option_of_yojson f = function
  | `Null -> Ok None
  | json -> f json |> Result.map (fun x -> Some x)

let ptime_opt_to_yojson = option_to_yojson ptime_to_yojson
let ptime_opt_of_yojson = option_of_yojson ptime_of_yojson

type status =
  | Open
  | Closed[@@deriving yojson]

type side =
  | Buy
  | Sell[@@deriving yojson]

type pos = {
  opened_at : Ptime.t
    [@to_yojson ptime_to_yojson]
    [@of_yojson ptime_of_yojson];
  closed_at : Ptime.t option
    [@to_yojson ptime_opt_to_yojson]
    [@of_yojson ptime_opt_of_yojson];
  last_sell_time: Ptime.t option
    [@to_yojson ptime_opt_to_yojson]
    [@of_yojson ptime_opt_of_yojson];
  last_buy_time : Ptime.t option
    [@to_yojson ptime_opt_to_yojson]
    [@of_yojson ptime_opt_of_yojson];
  symbol : string;
  buy_qty : float;
  sell_qty: float;
  net_buy_price : float;
  net_sell_price : float;
  net_qty : float;
  net_price : float;
  side : side;
  value : float;
  status : status;
  pnl: float; (* already accumulated pnl from a previous closing *)
  delta : float;
  total_delta : float;
  vega : float;
  theta: float;
  gamma : float;
  rho : float;
}[@@deriving yojson]

let open_position_from_order (order : Order.t) : pos =
  let symbol = order.tradingsymbol in
  let qty = order.filled_quantity in
  let price = order.filled_price in (* since this represents the avg price that the quantity was filled at*)
  let side = order.side in
  Printf.printf "Adding new position for symbol %s with price %f and qty %f \n%!" symbol price qty;
  {
    opened_at = Ptime_clock.now ();
    closed_at = None;
    last_sell_time = (match side with | Buy -> None | Sell -> order.executed_at);
    last_buy_time = (match side with | Buy -> order.executed_at | Sell -> None);
    symbol;
    buy_qty = (match side with | Buy -> qty | Sell -> 0.0);
    sell_qty = (match side with | Buy -> 0.0 | Sell -> -.qty);
    net_buy_price = (match side with | Buy -> price | Sell -> 0.0);
    net_sell_price = (match side with | Buy -> 0.0 | Sell -> price);
    side = (match side with | Buy -> Buy | Sell -> Sell);
    net_qty = (match side with | Buy -> qty | Sell -> -.qty);
    net_price = price;
    value = (match side with | Buy -> qty *. price | Sell -> -.qty *. price);
    status = Open;
    pnl = qty *. price; (* backtesting change *)
    delta = 0.0;
    total_delta = 0.0;
    vega = 0.0;
    theta = 0.0;
    gamma = 0.0;
    rho = 0.0;
  }

let update_position_from_buy_order (pos : pos) (order : Order.t) : pos =
  let symbol = order.tradingsymbol in
  let qty = order.filled_quantity in
  let price = order.filled_price in (* since this represents the avg price that the quantity was filled at*)
  let now = Ptime_clock.now () in
  let total_qty = pos.net_qty +. qty in
  let total_buy_cost = (pos.buy_qty *. pos.net_buy_price) +. (qty *. price) in
  let total_sell_cost = (pos.sell_qty *. pos.net_sell_price) in
  let new_buy_price = total_buy_cost /. (pos.buy_qty +. qty) in
  let net_price, value, pnl, status, closed_at =
    if total_qty = 0.0 then
      (0.0, 0.0, -.(total_buy_cost +. total_sell_cost), Closed, Some now)
    else
      let net_price = (total_sell_cost +. total_buy_cost) /. total_qty in
      (* keep on resetting the status to open because it may be followed by a Closed *)
      (net_price, total_qty *. net_price, pos.pnl, Open, pos.closed_at) (* t2 + 15 for the close *)
  in
  let side = if total_qty > 0.0 then Buy else Sell in
  Printf.printf
    "Updating position for symbol %s at price %f:\n\
             - net_price: %.2f -> %.2f\n\
             - net_qty: %.2f\n\
             - side: %s\n\
             - value: %.2f -> %.2f\n\
             - pnl: %.2f\n%!"
    symbol
    price
    pos.net_price
    net_price
    total_qty
    "BUY"
    pos.value
    value
    pnl;
  { pos with
    buy_qty = pos.buy_qty +. qty;
    net_qty = total_qty;
    net_buy_price = new_buy_price;
    net_price = net_price;
    value = value; 
    side = side;
    opened_at = now; (* lets us handle the loading case, where close should be on t2 + 15 *)
    last_buy_time = order.executed_at;
    pnl = pnl;
    status = status;
    closed_at = closed_at;
  }

let update_position_from_sell_order (pos : pos) (order : Order.t) : pos =
  let symbol = order.tradingsymbol in
  let qty = order.filled_quantity in
  let price = order.filled_price in (* since this represents the avg price that the quantity was filled at*)
  let now = Ptime_clock.now () in
  let total_qty = pos.net_qty -. qty in
  let total_sell_cost = (pos.sell_qty *. pos.net_sell_price) +. (-.qty *. price) in
  let total_buy_cost = (pos.buy_qty *. pos.net_buy_price) in
  let new_sell_price =  total_sell_cost /. (pos.sell_qty -. qty) in
  let net_price, value, pnl, status, closed_at=
    if total_qty = 0.0 then
      (0.0, 0.0, -.(total_buy_cost +. total_sell_cost), Closed, Some now)
    else
      let net_price = (total_sell_cost +. total_buy_cost) /. total_qty in
      (net_price, total_qty *. net_price, pos.pnl, Open, pos.closed_at)
  in
  let side = if total_qty > 0.0 then Buy else Sell in
  Printf.printf
    "Updating position for symbol %s at price %f:\n\
             - net_price: %.2f -> %.2f\n\
             - net_qty: %.2f\n\
             - side: %s\n\
             - value: %.2f -> %.2f\n\
             - pnl: %.2f\n%!"
    symbol
    price
    pos.net_price
    net_price
    total_qty
    "SELL"
    pos.value
    value
    pnl;
  { pos with
    sell_qty = pos.sell_qty -. qty;
    net_qty = total_qty;
    net_sell_price = new_sell_price;
    net_price = net_price;
    value = value;
    side = side;
    opened_at = now;
    last_sell_time = order.executed_at;
    pnl = pnl;
    status = status;
    closed_at = closed_at;
  }

(* this is position over all "completed" orders for a particular symbol, not meant for partial orders. *)
let update_or_insert_position (positions : pos list) (order : Order.t) : pos list =
  let symbol = order.tradingsymbol in
  let side = order.side in
  let rec update_positions acc = function
    | [] ->
      let new_position = open_position_from_order order in
      List.rev (new_position :: acc)

    | pos :: rest when pos.symbol = symbol ->
      let updated_pos =
        match side with
        | Buy ->
          update_position_from_buy_order pos order

        | Sell ->
          update_position_from_sell_order pos order
      in
      List.rev_append acc (updated_pos :: rest)

    | pos :: rest ->
      (* if pos.symbol <> symbol then *)
      (*   Printf.printf "Mismatch: pos.symbol='%s' vs order.symbol='%s'\n%!" pos.symbol symbol; *)
      update_positions (pos :: acc) rest
  in

  update_positions [] positions

