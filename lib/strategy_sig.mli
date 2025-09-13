(* strategy_sig.mli *)

module type S = sig
  type local_config = unit
  type local_state
  type event

  val read_row : string list -> event option
  val get_timestamp_from_event : event -> Ptime.t option
  val initial_local_state : local_state
  val create : (local_config, local_state) Chronos_core.Strategy.config -> (local_config, local_state) Chronos_core.Strategy.t
  val on_event : local_state Chronos_core.Strategy.state -> event -> local_state Chronos_core.Strategy.state
  val extract_orders : local_state Chronos_core.Strategy.state -> Chronos_core.Order.t list * local_state Chronos_core.Strategy.state
end
