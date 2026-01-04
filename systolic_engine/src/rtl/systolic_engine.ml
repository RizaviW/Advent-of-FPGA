open! Core
open! Hardcaml
open! Signal
open! Always

(* ============================================================= *)
(* Parameters & Constants                                        *)
(* ============================================================= *)

let csr_data_width    = 32
let csr_address_width = 8

(* Symbolic Register Address Map *)
let address_statistic_axis_h2c_beats     = 0x00
let address_statistic_axis_c2h_beats     = 0x01
let address_statistic_matrices_received  = 0x02
let address_statistic_matrices_processed = 0x03
let address_statistic_backpressure_stall = 0x04
let address_statistic_hamming_total_sum  = 0x05
let address_global_statistics_clear      = 0x80

(* ============================================================= *)
(* AXI-Stream Interfaces                                         *)
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
(* Inputs / Outputs                                              *)
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
(* Core & Solver Configuration                                   *)
(* ============================================================= *)

module Config = struct
  let data_width     = 64
  let metadata_width = 8
  let generations    = 64
end

module Array_engine  = Systolic_array.Make (Config)
module Solver_engine = Hamming_solver.Make (Config)

(* ============================================================= *)
(* Implementation                                                *)
(* ============================================================= *)

let create (i : Signal.t I.t) : Signal.t O.t =
  (* --------------------------------------------------------------------- *)
  (* Interface Invariants & Ghost Connectivity                            *)
  (* --------------------------------------------------------------------- *)

  (* Ensure all input bits are part of the signal graph to satisfy 
     Hardcaml port name invariants during circuit finalization. *)
  let ghost_connectivity = 
    (reduce ~f:(&: ) (bits_lsb i.I.csr_ff_write_data)) &: 
    (reduce ~f:(&: ) (bits_lsb i.I.csr_ram_read_data)) 
  in

  let spec = Reg_spec.create ~clock:i.I.clock ~reset:(~:(i.I.reset_n)) () in
  let generator_index_width = Int.ceil_log2 Config.generations in

  (* --------------------------------------------------------------------- *)
  (* CSR Statistics Scoreboard & Pulse Logic                               *)
  (* --------------------------------------------------------------------- *)

  let csr_write_strobe    = i.I.csr_ff_valid &: i.I.csr_ff_write_enable in
  let clear_requested     = csr_write_strobe &: (i.I.csr_ff_address ==: of_int_trunc ~width:csr_address_width address_global_statistics_clear) in
  let clear_previous      = Variable.reg spec ~width:1 ~reset_to:Bits.gnd in
  let stats_clear_pulse   = Variable.wire ~default:gnd () in
  
  let stats_spec = Reg_spec.override spec ~clear:stats_clear_pulse.value in

  let statistic_axis_h2c_beats      = Variable.reg stats_spec ~width:csr_data_width ~reset_to:(Bits.zero csr_data_width) in
  let statistic_axis_c2h_beats      = Variable.reg stats_spec ~width:csr_data_width ~reset_to:(Bits.zero csr_data_width) in
  let statistic_matrices_received   = Variable.reg stats_spec ~width:csr_data_width ~reset_to:(Bits.zero csr_data_width) in
  let statistic_matrices_processed  = Variable.reg stats_spec ~width:csr_data_width ~reset_to:(Bits.zero csr_data_width) in
  let statistic_backpressure_stall  = Variable.reg stats_spec ~width:csr_data_width ~reset_to:(Bits.zero csr_data_width) in
  let statistic_hamming_total_sum   = Variable.reg stats_spec ~width:csr_data_width ~reset_to:(Bits.zero csr_data_width) in

  compile 
    [ clear_previous <-- clear_requested
    ; when_ (clear_requested &: ~:(clear_previous.value))
        [ stats_clear_pulse <-- vdd ]
    ];

  (* --------------------------------------------------------------------- *)
  (* Input Distributor Stage                                               *)
  (* --------------------------------------------------------------------- *)

  let input_distributor_index     = Variable.reg spec ~width:2 ~reset_to:(Bits.zero 2) in
  let input_distributor_ready     = Variable.wire ~default:gnd () in
  let input_distributor_ready_reg = Variable.reg spec ~width:1 ~reset_to:Bits.gnd in
  let input_handshake_fire        = i.I.axis_h2c_src.tvalid &: input_distributor_ready_reg.value in
  let input_handshake_last        = i.I.axis_h2c_src.tlast in

  let next_round_robin_index round_robin_index = 
    mux round_robin_index [of_int_trunc ~width:2 1; of_int_trunc ~width:2 2; of_int_trunc ~width:2 0; of_int_trunc ~width:2 0] 
  in

  compile
    [ input_distributor_ready_reg <-- input_distributor_ready.value
    ; when_ (input_handshake_fire &: input_handshake_last)
        [ input_distributor_index <-- next_round_robin_index input_distributor_index.value
        ; statistic_matrices_received <-- statistic_matrices_received.value +:. 1 ]
    ; when_ input_handshake_fire
        [ statistic_axis_h2c_beats <-- statistic_axis_h2c_beats.value +:. 1 ]
    ];

  (* --------------------------------------------------------------------- *)
  (* Processing Unit Macro-Cells                                           *)
  (* --------------------------------------------------------------------- *)

  let processing_units =
    Array.init 3 ~f:(fun unit_index ->
      let is_selected_for_input = input_distributor_index.value ==: of_int_trunc ~width:2 unit_index in
      let array_ready_wire      = Variable.wire ~default:vdd () in
      
      let array_instance = Array_engine.create
        { Array_engine.I.
          clock       = i.I.clock
        ; reset_n     = i.I.reset_n
        ; ready_in    = array_ready_wire.value
        ; valid_in    = i.I.axis_h2c_src.tvalid &: is_selected_for_input
        ; data_in     = i.I.axis_h2c_src.tdata
        ; metadata_in = mux2 input_handshake_last (sll (one 8) ~by:7) (zero 8)
        }
      in

      (* Sealed Elastic Skid Buffer Registers *)
      let main_register_valid = Variable.reg spec ~width:1 ~reset_to:Bits.gnd in
      let main_register_data  = Variable.reg spec ~width:Config.data_width ~reset_to:(Bits.zero Config.data_width) in
      let main_register_meta  = Variable.reg spec ~width:Config.metadata_width ~reset_to:(Bits.zero Config.metadata_width) in
      let skid_register_valid = Variable.reg spec ~width:1 ~reset_to:Bits.gnd in
      let skid_register_data  = Variable.reg spec ~width:Config.data_width ~reset_to:(Bits.zero Config.data_width) in
      let skid_register_meta  = Variable.reg spec ~width:Config.metadata_width ~reset_to:(Bits.zero Config.metadata_width) in
      let downstream_ready    = Variable.wire ~default:gnd () in

      (* Solver Pipeline Snapshots & Transactional Handshakes *)
      let solver_trigger_start      = Variable.reg spec ~width:1 ~reset_to:Bits.gnd in
      let solver_result_latch       = Variable.reg spec ~width:1 ~reset_to:Bits.gnd in
      let solver_weight_snapshot    = Variable.reg spec ~width:csr_data_width ~reset_to:(Bits.zero csr_data_width) in
      let solver_particular_reg     = Variable.reg spec ~width:Config.data_width ~reset_to:(Bits.zero Config.data_width) in
      let solver_generator_bank     = Array.init Config.generations ~f:(fun _ -> Variable.reg spec ~width:Config.data_width ~reset_to:(Bits.zero Config.data_width)) in
      let solver_nullity_reg        = Variable.reg spec ~width:generator_index_width ~reset_to:(Bits.zero generator_index_width) in
      let solver_consumption_pulse  = Variable.wire ~default:gnd () in
      let unit_backpressure_event   = Variable.wire ~default:gnd () in

      let solver_instance = Solver_engine.create
        { Solver_engine.I.
          clock         = i.I.clock
        ; reset_n       = i.I.reset_n
        ; start         = solver_trigger_start.value
        ; particular    = solver_particular_reg.value
        ; generators    = Array.to_list (Array.map solver_generator_bank ~f:(fun r -> r.value))
        ; nullity_count = solver_nullity_reg.value
        }
      in

      (* Skid Buffer Control Logic *)
      compile
        [ array_ready_wire <-- ~:(main_register_valid.value &: skid_register_valid.value)
        ; if_ downstream_ready.value
            [ if_ skid_register_valid.value
                [ main_register_data  <-- skid_register_data.value
                ; main_register_meta  <-- skid_register_meta.value
                ; main_register_valid <-- vdd
                ; skid_register_valid <-- gnd ]
                [ main_register_valid <-- array_instance.valid_out
                ; main_register_data  <-- array_instance.data_out
                ; main_register_meta  <-- array_instance.metadata_out ]
            ]
            [ if_ (array_instance.valid_out &: ~:(skid_register_valid.value))
                [ if_ ~:(main_register_valid.value)
                    [ main_register_data  <-- array_instance.data_out
                    ; main_register_meta  <-- array_instance.metadata_out
                    ; main_register_valid <-- vdd ]
                    [ skid_register_data  <-- array_instance.data_out
                    ; skid_register_meta  <-- array_instance.metadata_out
                    ; skid_register_valid <-- vdd ]
                ] []
            ]
        ; when_ (array_instance.valid_out &: skid_register_valid.value &: ~:(downstream_ready.value))
            [ unit_backpressure_event <-- vdd ]
        ];

      (* Solver Transaction Control *)
      compile (
        [ solver_trigger_start <-- gnd
        ; when_ array_instance.done_pulse
            [ solver_particular_reg <-- array_instance.b0_out
            ; solver_nullity_reg    <-- uresize array_instance.k_out ~width:generator_index_width
            ; solver_trigger_start  <-- vdd ]
        ; if_ solver_instance.valid_out
            [ solver_result_latch    <-- vdd
            ; solver_weight_snapshot <-- uresize solver_instance.minimum_weight ~width:csr_data_width ]
            [ if_ solver_consumption_pulse.value [ solver_result_latch <-- gnd ] [] ]
        ] @ Array.to_list (Array.mapi solver_generator_bank ~f:(fun bank_index reg ->
            when_ (array_instance.null_valid &: (array_instance.null_index ==: of_int_trunc ~width:generator_index_width bank_index))
              [ reg <-- array_instance.null_data ]
        ))
      );

      (array_instance, main_register_valid, main_register_data, main_register_meta, downstream_ready, solver_result_latch, solver_weight_snapshot, solver_consumption_pulse, unit_backpressure_event)
    )
  in

  (* --------------------------------------------------------------------- *)
  (* Atomic Result Accumulator Stage                                       *)
  (* --------------------------------------------------------------------- *)

  let accumulator_index   = Variable.reg spec ~width:2 ~reset_to:(Bits.zero 2) in
  let processing_results  = Array.to_list processing_units |> List.map ~f:(fun (_, _, _, _, _, latch, _, _, _) -> latch.value) in
  let processing_weights  = Array.to_list processing_units |> List.map ~f:(fun (_, _, _, _, _, _, snapshot, _, _) -> snapshot.value) in
  let processing_stalls   = Array.to_list processing_units |> List.map ~f:(fun (_, _, _, _, _, _, _, _, event) -> event.value) in
  
  let current_pending_valid  = mux accumulator_index.value processing_results in
  let current_pending_weight = mux accumulator_index.value processing_weights in
  let any_unit_stalled       = reduce ~f:(|: ) processing_stalls in

  compile
    [ if_ current_pending_valid
        [ statistic_hamming_total_sum <-- statistic_hamming_total_sum.value +: current_pending_weight
        ; accumulator_index <-- next_round_robin_index accumulator_index.value ]
        [ accumulator_index <-- next_round_robin_index accumulator_index.value ]
    ; when_ any_unit_stalled
        [ statistic_backpressure_stall <-- statistic_backpressure_stall.value +:. 1 ]
    ];

  (* --------------------------------------------------------------------- *)
  (* Output Arbiter Stage                                                  *)
  (* --------------------------------------------------------------------- *)

  let output_arbiter_index = Variable.reg spec ~width:2 ~reset_to:(Bits.zero 2) in
  let unit_valids          = Array.to_list processing_units |> List.map ~f:(fun (_, valid, _, _, _, _, _, _, _) -> valid.value) in
  let unit_datas           = Array.to_list processing_units |> List.map ~f:(fun (_, _, data, _, _, _, _, _, _) -> data.value) in
  let unit_metas           = Array.to_list processing_units |> List.map ~f:(fun (_, _, _, meta, _, _, _, _, _) -> meta.value) in
  
  let arbiter_valid_out     = mux output_arbiter_index.value unit_valids in
  let arbiter_data_out      = mux output_arbiter_index.value unit_datas in
  let arbiter_meta_out      = mux output_arbiter_index.value unit_metas in
  let output_handshake_fire = arbiter_valid_out &: i.I.axis_c2h_sink.tready in
  let output_is_last_beat   = bit arbiter_meta_out ~pos:7 in

  compile
    [ when_ output_handshake_fire
        [ statistic_axis_c2h_beats <-- statistic_axis_c2h_beats.value +:. 1 ]
    ; when_ (output_handshake_fire &: output_is_last_beat)
        [ output_arbiter_index <-- next_round_robin_index output_arbiter_index.value
        ; statistic_matrices_processed <-- statistic_matrices_processed.value +:. 1 ]
    ; input_distributor_ready <-- mux input_distributor_index.value (Array.to_list processing_units |> List.map ~f:(fun (a, _, _, _, _, _, _, _, _) -> a.ready_out))
  ];

  (* Transactional Signal Routing *)
  Array.iteri processing_units ~f:(fun cell_index (_, _, _, _, downstream, _, _, consumption, _) ->
    compile [ downstream  <-- ((output_arbiter_index.value ==: of_int_trunc ~width:2 cell_index) &: i.I.axis_c2h_sink.tready)
            ; consumption <-- ((accumulator_index.value    ==: of_int_trunc ~width:2 cell_index) &: current_pending_valid) ]);

  (* --------------------------------------------------------------------- *)
  (* Interface Mapping & Declarative Logic                                 *)
  (* --------------------------------------------------------------------- *)

  let read_multiplexer_out = List.fold [
      address_statistic_axis_h2c_beats,     statistic_axis_h2c_beats.value;
      address_statistic_axis_c2h_beats,     statistic_axis_c2h_beats.value;
      address_statistic_matrices_received,  statistic_matrices_received.value;
      address_statistic_matrices_processed, statistic_matrices_processed.value;
      address_statistic_backpressure_stall, statistic_backpressure_stall.value;
      address_statistic_hamming_total_sum,  statistic_hamming_total_sum.value
    ] ~init:(uresize ghost_connectivity ~width:csr_data_width) ~f:(fun accumulator (address, signal_value) ->
      mux2 (i.I.csr_ff_address ==: of_int_trunc ~width:csr_address_width address) signal_value accumulator)
  in

  { O.
    axis_c2h_src         = { AXIS_source. tdata=arbiter_data_out; tkeep=ones 8; tlast=output_is_last_beat; tvalid=arbiter_valid_out }
  ; axis_h2c_sink        = { AXIS_sink. tready = input_distributor_ready_reg.value }
  ; csr_ff_read_data     = read_multiplexer_out
  ; csr_ram_valid        = gnd
  ; csr_ram_write_enable = gnd
  ; csr_ram_address      = zero csr_address_width
  ; csr_ram_write_data   = zero csr_data_width
  }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"systolic_engine" (fun _scope -> create)
