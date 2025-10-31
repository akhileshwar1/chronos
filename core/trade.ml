(* lib/trade.ml *)

type side = Buy | Sell

type trade_type =
  | Entry
  | Exit

type trade = {
  ts : Ptime.t option;     (* timestamp from event if available *)
  symbol : string;
  side : side;
  qty : float;             (* actual base qty (post-fee or pre-fee depending on policy) *)
  price : float;           (* fill price *)
  gross_notional : float;  (* qty * price before side-fee *)
  fee : float;             (* fee in quote currency applied to this trade side *)
  net_notional : float;    (* gross_notional - fee *)
  strategy : string;       (* strategy name or tag *)
  ttype : trade_type;
}

let csv_header () =
  "timestamp,symbol,side,trade_type,qty,price,gross_notional,fee,net_notional,strategy\n"

let side_to_str = function
  | Buy -> "BUY"
  | Sell -> "SELL"

let trade_type_to_str = function
  | Entry -> "ENTRY"
  | Exit -> "EXIT"

(* strategy_name will be used to mark ENTRY/EXIT tags if the string contains ":entry" or ":exit".
   Default to Entry if not specified. *)
let infer_trade_type_from_strategy_name (s : string) : trade_type =
  if String.exists (fun c -> c = ':') s then
    (* naive split by ':' and inspect last token *)
    let parts = String.split_on_char ':' s in
    match List.rev parts with
    | "entry" :: _ -> Entry
    | "exit" :: _ -> Exit
    | _ -> Entry
  else Entry

(* Create a trade from a completed Order.t.
   fee_rate_one_side: e.g. 0.001 for 0.1% per side.
   Policy: This function will compute fee on gross notional and fill the trade.fee/net_notional fields.
   It does NOT mutate the incoming Order.t. Caller may have already adjusted order.filled_quantity
   to reflect fees-in-base or not. This function will base fees on order.filled_quantity * price.
*)
let of_order ?(fee_rate_one_side=0.001) (order : Order.t) : trade =
  let qty = order.filled_quantity in
  let price = order.filled_price in
  let gross = qty *. price in
  let fee = gross *. fee_rate_one_side in
  let net = gross -. fee in
  let ttype =
    match order.strategy_name with
    | s when s <> "" -> infer_trade_type_from_strategy_name s
    | _ -> Entry
  in
  {
    ts = order.executed_at;
    symbol = order.tradingsymbol;
    side = (match order.side with | Order.Buy -> Buy | Order.Sell -> Sell);
    qty = net /. price;
    price = price;
    gross_notional = gross;
    fee = fee;
    net_notional = net;
    strategy = order.strategy_name;
    ttype = ttype;
  }

let to_csv_line (t : trade) : string =
  let ts = match t.ts with Some p -> Ptime.to_rfc3339 p | None -> "" in
  Printf.sprintf "%s,%s,%s,%s,%.12f,%.8f,%.8f,%.8f,%.8f,%s\n"
    ts
    t.symbol
    (side_to_str t.side)
    (trade_type_to_str t.ttype)
    t.qty
    t.price
    t.gross_notional
    t.fee
    t.net_notional
    t.strategy
