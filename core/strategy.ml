(* generic code to be used by strategies *)

type ('local_config ) config = {
  data_layer_uri : string;
  oms_layer_uri : string;
  symbol : string;
  local_config : 'local_config;
  oms_ws_uri : string;
}

type 'local_state state = {
  completed_orders : Order.t list;
  pending_orders : Order.t list;
  rejected_orders : Order.t list;
  created_orders : Order.t list;
  positions : Position.pos list;
  local_state : 'local_state;
}

type ('local_config, 'local_state) t = {
  config : ('local_config ) config;
  state : 'local_state state;
  extract_orders : 'local_state state -> Order.t list * 'local_state state;
}

let create
  (config : ('local_config ) config)
  (initial_local_state : 'local_state)
  (extract_orders : 'local_state state -> Order.t list * 'local_state state)
  : ('local_config, 'local_state) t =
  let initial_state = {
    completed_orders = [];
    pending_orders = [];
    rejected_orders = [];
    created_orders = [];
    positions = [];
    local_state = initial_local_state;
  } in
  {
    config;
    state = initial_state;
    extract_orders;
  }

let update_state
  (strategy : ('local_config, 'local_state) t)
  (new_state : 'local_state state)
  : ('local_config, 'local_state) t =
  { strategy with state = new_state }
