open Hardcaml

(* ============================================================= *)
(* Parameters                                                    *)
(* ============================================================= *)

val csr_data_width    : int
val csr_address_width : int

(* ============================================================= *)
(* AXI-Stream                                                    *)
(* ============================================================= *)

module AXIS_source : sig
  type 'a t =
    { tdata  : 'a [@bits 64]
    ; tkeep  : 'a [@bits 8]
    ; tlast  : 'a
    ; tvalid : 'a
    }
  [@@deriving hardcaml]
end

module AXIS_sink : sig
  type 'a t =
    { tready : 'a }
  [@@deriving hardcaml]
end

(* ============================================================= *)
(* Inputs                                                        *)
(* ============================================================= *)

module I : sig
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

module O : sig
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
(* Construction                                                  *)
(* ============================================================= *)

val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
