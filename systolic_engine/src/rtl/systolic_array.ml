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

      (* Affine Description (Contract §3) *)
      ; b0_out        : 'a [@bits C.data_width]
      ; null_valid    : 'a
      ; null_index    : 'a [@bits (Int.ceil_log2 C.data_width)]
      ; null_data     : 'a [@bits C.data_width]
      ; k_out         : 'a [@bits (Int.ceil_log2 C.data_width)]
      ; done_pulse    : 'a
      }
    [@@deriving hardcaml]
  end

(* ============================================================= *)
(* Implementation                                                *)
(* ============================================================= *)

  let create (i : Signal.t I.t) : Signal.t O.t =
    let spec = Reg_spec.create ~clock:i.I.clock ~reset:(~:(i.I.reset_n)) () in

    (* --------------------------------------------------------------------- *)
    (* Constants & Dimensions                                                *)
    (* --------------------------------------------------------------------- *)

    let identifier_width      = Int.max 1 (Int.ceil_log2 C.data_width) in
    let index_width           = Int.max 1 (Int.ceil_log2 (C.data_width + 1)) in
    let generator_index_width = Int.ceil_log2 C.data_width in
    let number_of_variables   = C.data_width - 1 in

    let idle_state            = of_int_trunc ~width:3 0 in
    let reset_state           = of_int_trunc ~width:3 1 in
    let load_state            = of_int_trunc ~width:3 2 in
    let clear_state           = of_int_trunc ~width:3 3 in
    let drain_state           = of_int_trunc ~width:3 4 in

    (* --------------------------------------------------------------------- *)
    (* Control Logic (Register Feedback)                                     *)
    (* --------------------------------------------------------------------- *)

    let pivot_feedback_var    = Variable.wire ~default:(zero C.data_width) () in
    let forward_ready_var     = Variable.wire ~default:gnd () in

    let state_width           = 3 in
    let total_control_width   = state_width + (3 * index_width) in

    let control_loop =
      reg_fb spec ~enable:vdd ~width:total_control_width ~reset_to:(Bits.zero total_control_width) ~f:(fun feedback_in ->
        let drain_counter = uresize (drop_bottom feedback_in ~width:(state_width + 2 * index_width)) ~width:index_width in
        let clear_counter = uresize (drop_bottom feedback_in ~width:(state_width + index_width))     ~width:index_width in
        let load_counter  = uresize (drop_bottom feedback_in ~width:state_width)                     ~width:index_width in
        let state         = uresize feedback_in ~width:state_width in

        (* Gating Signals *)
        let ready_out         = (state ==: idle_state) &: forward_ready_var.value in
        let fire_in           = i.I.valid_in &: ready_out in
        let last_in           = bit i.I.metadata_in ~pos:(C.metadata_width - 1) in
        let end_of_matrix     = fire_in &: last_in in
        let pivot_exists      = mux drain_counter (bits_lsb pivot_feedback_var.value) in

        (* Transition Logic *)
        let load_done         = load_counter  ==: of_int_trunc ~width:index_width (C.data_width - 1) in
        let clear_done        = clear_counter ==: zero index_width in
        let drain_done        = drain_counter ==: of_int_trunc ~width:index_width C.data_width in
        let drain_fire        = (state ==: drain_state) &: (~:pivot_exists |: i.I.ready_in) in

        let next_state =
          mux state
            [ mux2 end_of_matrix reset_state idle_state
            ; load_state
            ; mux2 load_done clear_state load_state
            ; mux2 clear_done drain_state clear_state
            ; mux2 drain_done idle_state drain_state
            ; idle_state; idle_state; idle_state
            ]
        in

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
    let clear_identifier = uresize (drop_bottom control_loop ~width:(3 + index_width))     ~width:index_width in
    let load_identifier  = uresize (drop_bottom control_loop ~width:3)                     ~width:index_width in
    let state            = uresize control_loop ~width:3 in

    let perform_wipe     = (state ==: drain_state) &: (drain_identifier ==: of_int_trunc ~width:index_width C.data_width) in

    (* --------------------------------------------------------------------- *)
    (* Systolic Processing Chains                                            *)
    (* --------------------------------------------------------------------- *)

    let make_chain ~mode ~reset_n ~valid_in ~data_in ~metadata_in =
      let ready_vars = Array.init (C.data_width + 1) ~f:(fun _ -> Variable.wire ~default:gnd ()) in
      let rec loop k v_in d_in m_in v_acc d_acc assignments =
        if k = C.data_width then 
          (v_in, d_in, m_in, List.rev v_acc, List.rev d_acc, assignments)
        else
          let module Core = Systolic_core.Make(struct
            let data_width     = C.data_width
            let metadata_width = identifier_width
            let identifier     = k
            let mode           = mode
          end) in
          let o = Core.create 
            { Core.I. 
              clock       = i.I.clock; 
              reset_n; 
              clear       = perform_wipe; 
              ready_in    = ready_vars.(k+1).value; 
              valid_in    = v_in; 
              data_in     = d_in; 
              metadata_in = m_in 
            } 
          in
          loop (k+1) o.valid_out o.data_out o.metadata_out (o.control_valid::v_acc) (o.control_data::d_acc) ((ready_vars.(k) <-- o.ready_out)::assignments)
      in
      let (v, d, m, cv, cd, assignments) = loop 0 valid_in data_in metadata_in [] [] [ready_vars.(C.data_width) <-- vdd] in
      compile assignments; 
      (ready_vars.(0).value, v, d, m, Array.of_list cv, Array.of_list cd)
    in

    (* Pipeline Instantiation *)
    let forward_reset_n = i.I.reset_n &: ~:((state ==: clear_state) &: (clear_identifier ==: zero index_width)) in
    let forward_ready, _, _, _, forward_pivot_valid, forward_pivot_data =
      make_chain ~mode:`Forward ~reset_n:forward_reset_n
        ~valid_in:(i.I.valid_in &: (state ==: idle_state) &: forward_ready_var.value) 
        ~data_in:i.I.data_in ~metadata_in:(zero identifier_width) 
    in

    let backward_valid_var     = Variable.wire ~default:gnd () in
    let backward_data_var      = Variable.wire ~default:(zero C.data_width) () in
    let backward_metadata_var  = Variable.wire ~default:(zero identifier_width) () in

    let _, _, _, _, backward_pivot_valid, backward_pivot_data =
      make_chain ~mode:`Backward ~reset_n:(i.I.reset_n &: ~:(state ==: reset_state))
        ~valid_in:backward_valid_var.value ~data_in:backward_data_var.value ~metadata_in:backward_metadata_var.value 
    in

    let mux_onehot array_in index_in = mux index_in (Array.to_list array_in) in
    let load_fire = (state ==: load_state) &: mux_onehot forward_pivot_valid load_identifier in

    compile 
      [ forward_ready_var     <-- forward_ready
      ; backward_valid_var    <-- (load_fire |: (state ==: clear_state))
      ; backward_data_var     <-- (mux2 load_fire (mux_onehot forward_pivot_data load_identifier) (mux_onehot backward_pivot_data clear_identifier))
      ; backward_metadata_var <-- (uresize (mux2 load_fire load_identifier clear_identifier) ~width:identifier_width)
      ; pivot_feedback_var    <-- (of_array backward_pivot_valid) 
      ];

    (* --------------------------------------------------------------------- *)
    (* Affine Variable Mapping (Contract §3)                                 *)
    (* --------------------------------------------------------------------- *)

    (* Pivot Column Detection *)
    let find_pivot_column row_bits =
      let rec search k =
        if k < 0 then of_int_trunc ~width:generator_index_width 0
        else mux2 (bit row_bits ~pos:k) (of_int_trunc ~width:generator_index_width k) (search (k - 1))
      in search (number_of_variables - 1)
    in

    let pivot_variable_map = Array.init C.data_width ~f:(fun r -> find_pivot_column backward_pivot_data.(r)) in

    let is_pivot_variable = Array.init C.data_width ~f:(fun col ->
      List.reduce_exn ~f:(|:) (Array.to_list (Array.mapi backward_pivot_valid ~f:(fun r v -> 
        v &: (pivot_variable_map.(r) ==: of_int_trunc ~width:generator_index_width col)
      )))
    ) in

    (* Particular Solution Extraction (b0) *)
    let b0_signals = Array.init C.data_width ~f:(fun col ->
      List.reduce_exn ~f:(|:) (Array.to_list (Array.mapi backward_pivot_valid ~f:(fun r v ->
        v &: (pivot_variable_map.(r) ==: of_int_trunc ~width:generator_index_width col) &: bit backward_pivot_data.(r) ~pos:number_of_variables
      )))
    ) in

    (* Nullspace Generator Extraction (f_j) *)
    let current_column_is_free = ~:(mux_onehot is_pivot_variable (uresize drain_identifier ~width:generator_index_width)) in
    let null_valid_pulse        = (state ==: drain_state) &: current_column_is_free &: (drain_identifier <: of_int_trunc ~width:index_width number_of_variables) in
    
    let null_data_signals = Array.init C.data_width ~f:(fun col ->
      let is_identity_bit = (uresize drain_identifier ~width:generator_index_width) ==: of_int_trunc ~width:generator_index_width col in
      let is_pivot_coefficient = List.reduce_exn ~f:(|:) (Array.to_list (Array.mapi backward_pivot_valid ~f:(fun r v ->
        let row_data       = backward_pivot_data.(r) in
        let coefficient    = mux (uresize drain_identifier ~width:generator_index_width) (bits_lsb row_data) in
        v &: (pivot_variable_map.(r) ==: of_int_trunc ~width:generator_index_width col) &: coefficient
      ))) in
      is_identity_bit |: is_pivot_coefficient
    ) in

    let null_index_register = Variable.reg spec ~width:generator_index_width ~reset_to:(Bits.zero generator_index_width) in
    compile [ when_ null_valid_pulse [ null_index_register <-- null_index_register.value +:. 1 ] ];

    (* --------------------------------------------------------------------- *)
    (* Output Mapping                                                        *)
    (* --------------------------------------------------------------------- *)

    let drain_fire    = (state ==: drain_state) &: (mux_onehot backward_pivot_valid drain_identifier) in
    let drain_meta    = uresize drain_identifier ~width:C.metadata_width in
    
    let suffix_active = 
      let scan_array = Array.create ~len:(C.data_width + 1) gnd in 
      for k = number_of_variables downto 0 do scan_array.(k) <- scan_array.(k+1) |: backward_pivot_valid.(k) done; 
      mux drain_identifier (Array.to_list (Array.sub scan_array ~pos:1 ~len:C.data_width)) 
    in

    { O. 
      ready_out     = (state ==: idle_state) &: forward_ready_var.value
    ; valid_out     = drain_fire
    ; data_out      = mux2 drain_fire (mux_onehot backward_pivot_data drain_identifier) (zero C.data_width)
    ; metadata_out  = mux2 (~:suffix_active) (drain_meta |: (sll (one C.metadata_width) ~by:(C.metadata_width - 1))) drain_meta
    ; control_valid = drain_fire
    ; control_data  = mux2 drain_fire (mux_onehot backward_pivot_data drain_identifier) (zero C.data_width)

    (* Affine Description *)
    ; b0_out     = of_array b0_signals
    ; null_valid = null_valid_pulse
    ; null_index = null_index_register.value
    ; null_data  = of_array null_data_signals
    ; k_out      = popcount (~:(of_array (Array.sub is_pivot_variable ~pos:0 ~len:number_of_variables)))
    ; done_pulse = (state ==: drain_state) &: (drain_identifier ==: of_int_trunc ~width:index_width C.data_width) 
    }

  let hierarchical scope = 
    let module Scoped = Hierarchy.In_scope (I) (O) in 
    Scoped.hierarchical ~scope ~name:"systolic_array" (fun _scope -> create)
end
