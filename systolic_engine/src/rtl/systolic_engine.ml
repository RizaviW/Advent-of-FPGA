open! Core
open! Hardcaml
open! Signal
open! Always

(* ============================================================= *)
(* Parameters                                                    *)
(* ============================================================= *)

let csr_data_width    = 32
let csr_address_width = 8

(* ============================================================= *)
(* AXI-Stream                                                    *)
(* ============================================================= *)

module AXIS_source = struct
  type 'a t =
    { tdata  : 'a [@bits 64]
    ; tkeep  : 'a [@bits 8]
    ; tlast  : 'a
    ; tvalid : 'a
    }
  [@@deriving hardcaml]
end

module AXIS_sink = struct
  type 'a t =
    { tready : 'a }
  [@@deriving hardcaml]
end

(* ============================================================= *)
(* Inputs                                                        *)
(* ============================================================= *)

module I = struct
  type 'a t =
    { clock               : 'a
    ; reset_n             : 'a

    ; csr_ram_read_data   : 'a [@bits csr_data_width]

    ; csr_ff_valid        : 'a
    ; csr_ff_write_enable : 'a
    ; csr_ff_address      : 'a [@bits csr_address_width]
    ; csr_ff_write_data   : 'a [@bits csr_data_width]

    ; axis_h2c_src        : 'a AXIS_source.t
    ; axis_c2h_sink       : 'a AXIS_sink.t
    }
  [@@deriving hardcaml]
end

(* ============================================================= *)
(* Outputs                                                       *)
(* ============================================================= *)

module O = struct
  type 'a t =
    { csr_ram_valid        : 'a
    ; csr_ram_write_enable : 'a
    ; csr_ram_address      : 'a [@bits csr_address_width]
    ; csr_ram_write_data   : 'a [@bits csr_data_width]

    ; csr_ff_read_data     : 'a [@bits csr_data_width]

    ; axis_c2h_src         : 'a AXIS_source.t
    ; axis_h2c_sink        : 'a AXIS_sink.t
    }
  [@@deriving hardcaml]
end

(* ============================================================= *)
(* Core Configuration                                            *)
(* ============================================================= *)

module Core_config = struct
  let data_width     = 64
  let metadata_width = 8
end

module Core = Systolic_array.Make (Core_config)

(* ============================================================= *)
(* Implementation                                                *)
(* ============================================================= *)

let create (i : Signal.t I.t) : Signal.t O.t =
  let spec = Reg_spec.create ~clock:i.I.clock ~reset:(~:(i.I.reset_n)) () in

  (* --------------------------------------------------------------------- *)
  (* Input Distributor (Round Robin)                                       *)
  (* --------------------------------------------------------------------- *)

  let input_index = Variable.reg spec ~width:2 ~reset_to:(Bits.zero 2) in
  let input_ready = Variable.wire ~default:gnd () in

  let input_fire       = i.I.axis_h2c_src.tvalid &: input_ready.value in
  let input_last       = i.I.axis_h2c_src.tlast in
  let input_switch     = input_fire &: input_last in

  let next_input_index =
    mux input_index.value
      [ of_int_trunc ~width:2 1
      ; of_int_trunc ~width:2 2
      ; of_int_trunc ~width:2 0
      ; of_int_trunc ~width:2 0
      ]
  in

  (* --------------------------------------------------------------------- *)
  (* Output Arbiter (Round Robin)                                          *)
  (* --------------------------------------------------------------------- *)

  let output_index = Variable.reg spec ~width:2 ~reset_to:(Bits.zero 2) in
  let output_valid = Variable.wire ~default:gnd () in
  let output_last  = Variable.wire ~default:gnd () in
  
  let output_ready      = i.I.axis_c2h_sink.tready in
  let output_fire       = output_valid.value &: output_ready in
  let output_switch     = output_fire &: output_last.value in

  let next_output_index =
    mux output_index.value
      [ of_int_trunc ~width:2 1
      ; of_int_trunc ~width:2 2
      ; of_int_trunc ~width:2 0
      ; of_int_trunc ~width:2 0
      ]
  in

  compile
    [ when_ input_switch  [ input_index  <-- next_input_index ]
    ; when_ output_switch [ output_index <-- next_output_index ]
    ];

  (* --------------------------------------------------------------------- *)
  (* Core Instantiation                                                    *)
  (* --------------------------------------------------------------------- *)

  let core_ready_ins = Array.init 3 ~f:(fun _ -> Variable.wire ~default:gnd ()) in

  let cores =
    Array.init 3 ~f:(fun k ->
      let selected_in = input_index.value ==: of_int_trunc ~width:2 k in
      let valid_in    = i.I.axis_h2c_src.tvalid &: selected_in in
      
      let meta_in     = mux2 input_last (sll (one 8) ~by:7) (zero 8) in

      Core.create
        { Core.I.
          clock       = i.I.clock
        ; reset_n     = i.I.reset_n
        ; ready_in    = core_ready_ins.(k).value
        ; valid_in    = valid_in
        ; data_in     = i.I.axis_h2c_src.tdata
        ; metadata_in = meta_in
        }
    )
  in

  (* --------------------------------------------------------------------- *)
  (* Muxing & Wiring                                                       *)
  (* --------------------------------------------------------------------- *)

  let selected_ready_out =
    mux input_index.value (Array.to_list (Array.map cores ~f:(fun c -> c.Core.O.ready_out)))
  in
  compile [ input_ready <-- selected_ready_out ];

  let selected_valid_out =
    mux output_index.value (Array.to_list (Array.map cores ~f:(fun c -> c.Core.O.valid_out)))
  in
  let selected_data_out =
    mux output_index.value (Array.to_list (Array.map cores ~f:(fun c -> c.Core.O.data_out)))
  in
  let selected_meta_out =
    mux output_index.value (Array.to_list (Array.map cores ~f:(fun c -> c.Core.O.metadata_out)))
  in
  let selected_last_out =
    bit selected_meta_out ~pos:7
  in

  compile
    [ output_valid <-- selected_valid_out
    ; output_last  <-- selected_last_out
    ];

  let core_ready_assignments =
    Array.to_list (Array.mapi cores ~f:(fun k _ ->
      let selected_out = output_index.value ==: of_int_trunc ~width:2 k in
      core_ready_ins.(k) <-- (output_ready &: selected_out)
    ))
  in
  compile core_ready_assignments;

  { O.
    axis_c2h_src =
      { AXIS_source.
        tdata  = selected_data_out
      ; tkeep  = ones 8
      ; tlast  = selected_last_out
      ; tvalid = output_valid.value
      }
  ; axis_h2c_sink =
    { AXIS_sink.
      tready = input_ready.value
    }

  ; csr_ram_valid        = gnd
  ; csr_ram_write_enable = gnd
  ; csr_ram_address      = zero csr_address_width
  ; csr_ram_write_data   = zero csr_data_width

  ; csr_ff_read_data     = zero csr_data_width
  }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"systolic_engine" (fun _scope -> create)
