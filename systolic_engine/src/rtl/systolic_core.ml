open! Core
open! Hardcaml
open! Signal
open! Always

(* ============================================================= *)
(* Parameters                                                    *)
(* ============================================================= *)

module type Config = sig
  val data_width     : int
  val metadata_width : int
  val identifier     : int
  val mode           : [ `Forward | `Backward ]
end

module Make (C : Config) = struct

(* ============================================================= *)
(* Inputs                                                        *)
(* ============================================================= *)

  module I = struct
    type 'a t =
      { clock       : 'a
      ; reset_n     : 'a
      ; clear       : 'a
      ; ready_in    : 'a
      ; valid_in    : 'a
      ; data_in     : 'a [@bits C.data_width]
      ; metadata_in : 'a [@bits C.metadata_width]
      }
    [@@deriving hardcaml]
  end

(* ============================================================= *)
(* Outputs                                                       *)
(* ============================================================= *)

  module O = struct
    type 'a t =
      { ready_out     : 'a
      ; valid_out     : 'a
      ; data_out      : 'a [@bits C.data_width]
      ; metadata_out  : 'a [@bits C.metadata_width]
      ; control_valid : 'a
      ; control_data  : 'a [@bits C.data_width]
      }
    [@@deriving hardcaml]
  end

(* ============================================================= *)
(* Implementation                                                *)
(* ============================================================= *)

  let create (i : Signal.t I.t) : Signal.t O.t =
    if C.data_width <= 0 then
      raise_s [%message "data_width must be > 0" (C.data_width : int)];
    if C.metadata_width <= 0 then
      raise_s [%message "metadata_width must be > 0" (C.metadata_width : int)];
    if C.identifier < 0 || C.identifier >= C.data_width then
      raise_s
        [%message
          "identifier out of range"
            (C.identifier : int)
            (C.data_width : int)];

    let reset = ~:(i.I.reset_n) in
    let spec  = Reg_spec.create ~clock:i.I.clock ~reset ~clear:i.I.clear () in

    let id_const = of_int_trunc ~width:C.metadata_width C.identifier in
    let id_bit   = bit i.I.data_in ~pos:C.identifier in

    (* --------------------------------------------------------------------- *)
    (* State Logic (Register Feedback)                                       *)
    (* --------------------------------------------------------------------- *)

    let pivot_state_width = 1 + C.data_width in

    let pivot_state =
      reg_fb spec ~enable:vdd ~width:pivot_state_width ~reset_to:(Bits.zero pivot_state_width) ~f:(fun state_fb ->
        let pivot_valid = bit state_fb ~pos:C.data_width in
        let pivot_data  = uresize state_fb ~width:C.data_width in

        (* Forward Logic *)
        let forward_wants_capture = (~:pivot_valid) &: id_bit in
        let forward_ready_out     = mux2 forward_wants_capture vdd i.I.ready_in in
        let forward_fire          = i.I.valid_in &: forward_ready_out in
        let forward_capture       = forward_fire &: forward_wants_capture in

        (* Backward Logic *)
        let backward_fire         = i.I.valid_in &: i.I.ready_in in
        let backward_is_self      = i.I.metadata_in ==: id_const in
        let backward_can_capture  = backward_fire &: (~:pivot_valid) &: backward_is_self &: id_bit in
        let backward_is_left      = i.I.metadata_in >: id_const in

        let backward_stationary_bit =
          List.init C.data_width ~f:(fun k ->
            (i.I.metadata_in ==: of_int_trunc ~width:C.metadata_width k) &: bit pivot_data ~pos:k)
          |> List.fold ~init:gnd ~f:(|:)
        in

        let backward_clear =
          backward_fire &: pivot_valid &: backward_is_left &: backward_stationary_bit
        in

        (* Next State *)
        let pivot_valid_next =
          match C.mode with
          | `Forward  -> pivot_valid |: forward_capture
          | `Backward -> pivot_valid |: backward_can_capture
        in

        let pivot_data_next =
          match C.mode with
          | `Forward ->
              mux2 forward_capture i.I.data_in pivot_data
          | `Backward ->
              mux2 backward_can_capture i.I.data_in
                (mux2 backward_clear (pivot_data ^: i.I.data_in) pivot_data)
        in

        pivot_valid_next @: pivot_data_next
      )
    in

    (* --------------------------------------------------------------------- *)
    (* Output Generation                                                     *)
    (* --------------------------------------------------------------------- *)

    let pivot_valid = bit pivot_state ~pos:C.data_width in
    let pivot_data  = uresize pivot_state ~width:C.data_width in

    let forward_wants_capture = (~:pivot_valid) &: id_bit in
    let forward_ready_out     = mux2 forward_wants_capture vdd i.I.ready_in in
    let forward_fire          = i.I.valid_in &: forward_ready_out in
    let forward_forward       = forward_fire &: (~:forward_wants_capture) in

    let forward_eliminate     = pivot_valid &: id_bit in
    let forward_data          = mux2 forward_eliminate (i.I.data_in ^: pivot_data) i.I.data_in in

    let backward_ready_out    = i.I.ready_in in

    match C.mode with
    | `Forward ->
        { O.
          ready_out     = forward_ready_out
        ; valid_out     = forward_forward
        ; data_out      = forward_data
        ; metadata_out  = i.I.metadata_in
        ; control_valid = pivot_valid
        ; control_data  = pivot_data
        }

    | `Backward ->
        { O.
          ready_out     = backward_ready_out
        ; valid_out     = i.I.valid_in
        ; data_out      = i.I.data_in
        ; metadata_out  = i.I.metadata_in
        ; control_valid = pivot_valid
        ; control_data  = pivot_data
        }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"systolic_core" (fun _scope -> create)
end
