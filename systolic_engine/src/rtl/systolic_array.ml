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
end

module Make (C : Config) = struct

(* ============================================================= *)
(* Inputs                                                        *)
(* ============================================================= *)

  module I = struct
    type 'a t =
      { clock       : 'a
      ; reset_n     : 'a
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
    let spec = Reg_spec.create ~clock:i.I.clock ~reset:(~:(i.I.reset_n)) () in

    (* --------------------------------------------------------------------- *)
    (* Constants                                                             *)
    (* --------------------------------------------------------------------- *)

    let identifier_width      = Int.max 1 (Int.ceil_log2 C.data_width) in
    let index_width           = Int.max 1 (Int.ceil_log2 (C.data_width + 1)) in

    let idle_state            = of_int_trunc ~width:3 0 in
    let reset_state           = of_int_trunc ~width:3 1 in
    let load_state            = of_int_trunc ~width:3 2 in
    let clear_state           = of_int_trunc ~width:3 3 in
    let drain_state           = of_int_trunc ~width:3 4 in

    (* --------------------------------------------------------------------- *)
    (* Wires (Always DSL)                                                    *)
    (* --------------------------------------------------------------------- *)

    let pivot_feedback_var    = Variable.wire ~default:(zero C.data_width) () in
    let forward_ready_var     = Variable.wire ~default:gnd () in
    let forward_ready         = forward_ready_var.value in

    (* --------------------------------------------------------------------- *)
    (* Control Logic (Register Feedback)                                     *)
    (* --------------------------------------------------------------------- *)

    let state_width = 3 in
    let total_width = state_width + (3 * index_width) in

    let control_loop =
      reg_fb spec ~enable:vdd ~width:total_width ~reset_to:(Bits.zero total_width) ~f:(fun fb_in ->
        let drain_counter = uresize (drop_bottom fb_in ~width:(state_width + 2 * index_width)) ~width:index_width in
        let clear_counter = uresize (drop_bottom fb_in ~width:(state_width + index_width)) ~width:index_width in
        let load_counter  = uresize (drop_bottom fb_in ~width:state_width) ~width:index_width in
        let state         = uresize fb_in ~width:state_width in

        (* Gating *)
        let ready_out     = (state ==: idle_state) &: forward_ready in
        let fire_in       = i.I.valid_in &: ready_out in
        let last_in       = bit i.I.metadata_in ~pos:(C.metadata_width - 1) in
        let end_of_matrix = fire_in &: last_in in
        let pivot_exists  = mux drain_counter (bits_lsb pivot_feedback_var.value) in

        (* Transition Conditions *)
        let load_done     = load_counter  ==: of_int_trunc ~width:index_width (C.data_width - 1) in
        let clear_done    = clear_counter ==: zero index_width in
        let drain_done    = drain_counter ==: of_int_trunc ~width:index_width C.data_width in
        let drain_fire    = (state ==: drain_state) &: (~:pivot_exists |: i.I.ready_in) in

        (* Next State Logic *)
        let next_state =
          mux state
            [ mux2 end_of_matrix reset_state idle_state
            ; load_state
            ; mux2 load_done clear_state load_state
            ; mux2 clear_done drain_state clear_state
            ; mux2 drain_done idle_state drain_state
            ; idle_state
            ; idle_state
            ; idle_state
            ]
        in

        (* Next Counter Values *)
        let next_load_counter =
          mux2 (state ==: reset_state) (zero index_width)
            (mux2 (state ==: load_state) (load_counter +:. 1) load_counter)
        in

        let next_clear_counter =
          mux2 (state ==: idle_state) (of_int_trunc ~width:index_width (C.data_width - 1))
            (mux2 (state ==: clear_state) (clear_counter -:. 1) clear_counter)
        in

        let next_drain_counter =
          mux2 (state ==: reset_state) (zero index_width)
            (mux2 drain_fire (drain_counter +:. 1) drain_counter)
        in

        next_drain_counter @: next_clear_counter @: next_load_counter @: next_state
      )
    in

    let drain_identifier = uresize (drop_bottom control_loop ~width:(3 + 2 * index_width)) ~width:index_width in
    let clear_identifier = uresize (drop_bottom control_loop ~width:(3 + index_width)) ~width:index_width in
    let load_identifier  = uresize (drop_bottom control_loop ~width:3) ~width:index_width in
    let state            = uresize control_loop ~width:3 in

    let ready_out     = (state ==: idle_state) &: forward_ready in
    let clear_done    = clear_identifier ==: zero index_width in

    let drain_done    = (drain_identifier ==: of_int_trunc ~width:index_width C.data_width) in
    let perform_wipe  = (state ==: drain_state) &: drain_done in

    (* --------------------------------------------------------------------- *)
    (* Systolic Chains (Always DSL)                                          *)
    (* --------------------------------------------------------------------- *)

    let make_chain ~mode ~reset_n ~valid_in ~data_in ~metadata_in =
      let ready_vars = Array.init (C.data_width + 1) ~f:(fun _ -> Variable.wire ~default:gnd ()) in
      let termination_assign = ready_vars.(C.data_width) <-- vdd in

      let rec loop k v_in d_in m_in v_acc d_acc assigns =
        if k = C.data_width then
          (v_in, d_in, m_in, List.rev v_acc, List.rev d_acc, assigns)
        else
          let module Core = Systolic_core.Make(struct
            let data_width     = C.data_width
            let metadata_width = identifier_width
            let identifier     = k
            let mode           = mode
          end) in
          let o = Core.create
            { Core.I.
              clock       = i.I.clock
            ; reset_n     = reset_n
            ; clear       = perform_wipe
            ; ready_in    = ready_vars.(k + 1).value
            ; valid_in    = v_in
            ; data_in     = d_in
            ; metadata_in = m_in
            }
          in
          let new_assign = ready_vars.(k) <-- o.ready_out in
          loop (k + 1) o.valid_out o.data_out o.metadata_out
               (o.control_valid :: v_acc) (o.control_data :: d_acc) (new_assign :: assigns)
      in

      let (v, d, m, cv, cd, chain_assigns) = loop 0 valid_in data_in metadata_in [] [] [termination_assign] in
      compile chain_assigns;
      (ready_vars.(0).value, v, d, m, Array.of_list cv, Array.of_list cd)
    in

    (* Forward Chain *)
    let forward_reset_signal = (state ==: clear_state) &: clear_done in
    let forward_reset_n      = i.I.reset_n &: ~:forward_reset_signal in

    let forward_chain_ready_sig, _, _, _, forward_pivot_valid, forward_pivot_data =
      make_chain
        ~mode:`Forward
        ~reset_n:forward_reset_n
        ~valid_in:(i.I.valid_in &: ready_out)
        ~data_in:i.I.data_in
        ~metadata_in:(zero identifier_width)
    in

    (* Backward Chain *)
    let backward_valid_var    = Variable.wire ~default:gnd () in
    let backward_data_var     = Variable.wire ~default:(zero C.data_width) () in
    let backward_metadata_var = Variable.wire ~default:(zero identifier_width) () in

    let _, _, _, _, backward_pivot_valid, backward_pivot_data =
      make_chain
        ~mode:`Backward
        ~reset_n:(i.I.reset_n &: ~:(state ==: reset_state))
        ~valid_in:backward_valid_var.value
        ~data_in:backward_data_var.value
        ~metadata_in:backward_metadata_var.value
    in

    (* Wiring Assignments *)
    let mux_onehot arr idx = mux idx (Array.to_list arr) in

    let load_valid   = (state ==: load_state)  &: mux_onehot forward_pivot_valid load_identifier in
    let load_data    = mux_onehot forward_pivot_data load_identifier in
    let clear_valid  = (state ==: clear_state) in
    let clear_data   = mux_onehot backward_pivot_data clear_identifier in

    compile
      [ forward_ready_var     <-- forward_chain_ready_sig
      ; backward_valid_var    <-- (load_valid |: clear_valid)
      ; backward_data_var     <-- (mux2 load_valid load_data clear_data)
      ; backward_metadata_var <-- (mux2 load_valid (uresize load_identifier ~width:identifier_width) (uresize clear_identifier ~width:identifier_width))
      ; pivot_feedback_var    <-- (of_array backward_pivot_valid)
      ];

    (* --------------------------------------------------------------------- *)
    (* Output Generation                                                     *)
    (* --------------------------------------------------------------------- *)

    let drain_valid  = (state ==: drain_state) &: (drain_identifier <: of_int_trunc ~width:index_width C.data_width) in
    let pivot_exists = mux_onehot backward_pivot_valid drain_identifier in
    let pivot_data   = mux_onehot backward_pivot_data drain_identifier in

    let suffix_valid =
      let scan = Array.create ~len:(C.data_width + 1) gnd in
      for k = C.data_width - 1 downto 0 do
        scan.(k) <- scan.(k + 1) |: backward_pivot_valid.(k)
      done;
      mux drain_identifier (Array.to_list (Array.sub scan ~pos:1 ~len:C.data_width))
    in

    let out_fire     = drain_valid &: pivot_exists in
    let out_data     = pivot_data in
    let out_last     = ~:suffix_valid in
    let out_counter  = uresize drain_identifier ~width:C.metadata_width in

    let out_metadata = mux2 out_last (out_counter |: (sll (one C.metadata_width) ~by:(C.metadata_width - 1))) out_counter in

    { O.
      ready_out
    ; valid_out     = out_fire
    ; data_out      = mux2 out_fire out_data (zero C.data_width)
    ; metadata_out  = mux2 out_fire out_metadata (zero C.metadata_width)
    ; control_valid = out_fire
    ; control_data  = mux2 out_fire out_data (zero C.data_width)
    }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"systolic_array" (fun _scope -> create)
end
