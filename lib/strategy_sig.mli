(* strategy_sig.mli *)

module type S = sig
  type local_state
  type local_config = unit
  type event

  (* build an opaque local_state from runtime key/value parameters *)
  val local_state_of_kv : (string * string) list -> (local_state, string) result
  val read_row : string list -> event option
  val get_timestamp_from_event : event -> Ptime.t option
  val create : ('local_config) Chronos_core.Strategy.config -> local_state -> ('local_config, local_state) Chronos_core.Strategy.t
  val on_event : local_state Chronos_core.Strategy.state -> event -> local_state Chronos_core.Strategy.state
  val extract_orders : local_state Chronos_core.Strategy.state -> Chronos_core.Order.t list * local_state Chronos_core.Strategy.state
end
