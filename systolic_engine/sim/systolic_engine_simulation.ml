open Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_test_harness

module Engine  = Systolic_engine
module Harness = Cyclesim_harness.Make (Engine.I) (Engine.O)

let ( <--. ) = Bits.( <--. )

(* ========================================================================== *)
(* Low-level drivers                                                          *)
(* ========================================================================== *)

let apply_reset (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  inputs.reset_n := Bits.vdd;
  cycle ();
  inputs.reset_n := Bits.gnd;
  cycle ();
  inputs.reset_n := Bits.vdd;
  cycle ()
;;

let write_csr ~addr ~data (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let cycle () = Cyclesim.cycle sim in
  inputs.csr_ff_valid        := Bits.vdd;
  inputs.csr_ff_write_enable := Bits.vdd;
  inputs.csr_ff_address      <--. addr;
  inputs.csr_ff_write_data   <--. data;
  cycle ();
  inputs.csr_ff_valid        := Bits.gnd;
  inputs.csr_ff_write_enable := Bits.gnd;
  cycle ()
;;

let send_axis_word ~data ~last (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let cycle () = Cyclesim.cycle sim in
  inputs.axis_h2c_src.tdata   <--. data;
  inputs.axis_h2c_src.tkeep   <--. 0xFF;
  inputs.axis_h2c_src.tlast   := if last then Bits.vdd else Bits.gnd;
  inputs.axis_h2c_src.tvalid  := Bits.vdd;
  inputs.axis_c2h_sink.tready := Bits.vdd;
  cycle ();
  inputs.axis_h2c_src.tvalid  := Bits.gnd;
  cycle ()
;;

let recv_axis_word (sim : Harness.Sim.t) =
  let outputs = Cyclesim.outputs sim in
  Bits.to_unsigned_int !(outputs.axis_c2h_src.tdata)
;;

(* ========================================================================== *)
(* Test procedure                                                             *)
(* ========================================================================== *)

let axis_loopback_test (sim : Harness.Sim.t) =
  apply_reset sim;

  (* CSR write (currently ignored by RTL, retained for forward compatibility) *)
  write_csr ~addr:0 ~data:7 sim;

  (* AXIS stimulus *)
  send_axis_word ~data:10 ~last:true sim;

  (* AXIS response *)
  let result = recv_axis_word sim in
  print_s [%message "AXIS result" (result : int)];

  (* Allow pipeline to settle *)
  Cyclesim.cycle ~n:2 sim
;;

(* ========================================================================== *)
(* Waveform configuration                                                     *)
(* ========================================================================== *)

let waves_config = Waves_config.no_waves

(* ========================================================================== *)
(* Expect tests                                                               *)
(* ========================================================================== *)

let%expect_test "systolic_engine axis loopback" =
  Harness.run_advanced
    ~waves_config
    ~create:Engine.hierarchical
    axis_loopback_test;
  [%expect {| ("AXIS result" (result 10)) |}]
;;

let%expect_test "systolic_engine axis loopback with waveform dump" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "systolic_engine*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Engine.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:30
        ~display_width:70
        ~wave_width:1
        waves)
    axis_loopback_test;
  [%expect {|
    ("AXIS result" (result 10))
    ┌Signals─────────────────────┐┌Waves─────────────────────────────────┐
    │systolic_engine$i$axis_c2h_s││                    ┌───────────────  │
    │                            ││────────────────────┘                 │
    │                            ││────────────────────┬───────────────  │
    │systolic_engine$i$axis_h2c_s││ 0                  │10               │
    │                            ││────────────────────┴───────────────  │
    │                            ││────────────────────┬───────────────  │
    │systolic_engine$i$axis_h2c_s││ 0                  │255              │
    │                            ││────────────────────┴───────────────  │
    │systolic_engine$i$axis_h2c_s││                    ┌───────────────  │
    │                            ││────────────────────┘                 │
    │systolic_engine$i$axis_h2c_s││                    ┌───┐             │
    │                            ││────────────────────┘   └───────────  │
    │                            ││────────────────────┬───────────────  │
    │systolic_engine$o$axis_c2h_s││ 0                  │10               │
    │                            ││────────────────────┴───────────────  │
    │                            ││────────────────────┬───────────────  │
    │systolic_engine$o$axis_c2h_s││ 0                  │255              │
    │                            ││────────────────────┴───────────────  │
    │systolic_engine$o$axis_c2h_s││                    ┌───────────────  │
    │                            ││────────────────────┘                 │
    │systolic_engine$o$axis_c2h_s││                    ┌───┐             │
    │                            ││────────────────────┘   └───────────  │
    │systolic_engine$o$axis_h2c_s││                    ┌───────────────  │
    │                            ││────────────────────┘                 │
    │                            ││────────────────────────────────────  │
    │systolic_engine$o$csr_ff_rea││ 0                                    │
    │                            ││────────────────────────────────────  │
    │                            ││────────────────────────────────────  │
    │systolic_engine$o$csr_ram_ad││ 0                                    │
    │                            ││────────────────────────────────────  │
    │systolic_engine$o$csr_ram_va││                                      │
    │                            ││────────────────────────────────────  │
    │                            ││────────────────────────────────────  │
    │systolic_engine$o$csr_ram_wr││ 0                                    │
    │                            ││────────────────────────────────────  │
    │systolic_engine$o$csr_ram_wr││                                      │
    │                            ││────────────────────────────────────  │
    └────────────────────────────┘└──────────────────────────────────────┘
    |}]
;;
