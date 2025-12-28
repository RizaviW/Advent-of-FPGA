open Hardcaml
open Signal

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
    { clock   : 'a
    ; reset_n : 'a

    ; csr_ram_read_data : 'a [@bits csr_data_width]

    ; csr_ff_valid        : 'a
    ; csr_ff_write_enable : 'a
    ; csr_ff_address      : 'a [@bits csr_address_width]
    ; csr_ff_write_data   : 'a [@bits csr_data_width]

    ; axis_h2c_src  : 'a AXIS_source.t
    ; axis_c2h_sink : 'a AXIS_sink.t
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

    ; axis_c2h_src  : 'a AXIS_source.t
    ; axis_h2c_sink : 'a AXIS_sink.t
    }
  [@@deriving hardcaml]
end

(* ============================================================= *)
(* Implementation                                                *)
(* ============================================================= *)

let create (i : Signal.t I.t) : Signal.t O.t =
  let axis_c2h_src =
    AXIS_source.map i.axis_h2c_src ~f:(fun x -> x)
  in
  let axis_h2c_sink =
    AXIS_sink.map i.axis_c2h_sink ~f:(fun x -> x)
  in
  { O.
    axis_c2h_src
  ; axis_h2c_sink

  ; csr_ram_valid        = gnd
  ; csr_ram_write_enable = gnd
  ; csr_ram_address      = zero csr_address_width
  ; csr_ram_write_data   = zero csr_data_width

  ; csr_ff_read_data     = zero csr_data_width
  }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"systolic_engine" (fun _scope -> create)
;;
