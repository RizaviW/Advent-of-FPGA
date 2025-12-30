open! Core
open! Hardcaml
open! Hardcaml_test_harness

(* ========================================================================== *)
(* Configuration                                                              *)
(* ========================================================================== *)

module Engine  = Systolic_engine
module Harness = Cyclesim_harness.Make (Engine.I) (Engine.O)

let ( <--. ) = Bits.( <--. )

(* ========================================================================== *)
(* Software Reference (Golden Model)                                          *)
(* ========================================================================== *)

module Reference = struct
  let compute = Systolic_solver.compute_rref
end

(* ========================================================================== *)
(* Simulation Utilities                                                       *)
(* ========================================================================== *)

let cycle sim = Cyclesim.cycle sim

let reset (sim : Harness.Sim.t) =
  let i = Cyclesim.inputs sim in
  i.reset_n := Bits.vdd; cycle sim;
  i.reset_n := Bits.gnd; cycle sim;
  i.reset_n := Bits.vdd; cycle sim
;;

(* ========================================================================== *)
(* AXI-Stream Drivers                                                         *)
(* ========================================================================== *)

(* Blocking AXIS sender: sends one matrix, respects ready *)
let send_matrix (sim : Harness.Sim.t) (rows : int64 list) =
  let i = Cyclesim.inputs  sim in
  let o = Cyclesim.outputs sim in

  let rec wait_ready () =
    cycle sim;
    if not (Bits.to_bool !(o.axis_h2c_sink.tready)) then wait_ready ()
  in

  List.iteri rows ~f:(fun idx row ->
    i.axis_h2c_src.tvalid := Bits.vdd;
    i.axis_h2c_src.tdata  := Bits.of_int64_trunc ~width:64 row;
    i.axis_h2c_src.tkeep  <--. 0xFF;
    i.axis_h2c_src.tlast  := Bits.of_bool (idx = List.length rows - 1);
    wait_ready ()
  );

  i.axis_h2c_src.tvalid := Bits.gnd;
  i.axis_h2c_src.tlast  := Bits.gnd;
  cycle sim
;;

(* Blocking AXIS receiver: drains output stream *)
let recv_matrix (sim : Harness.Sim.t) =
  let i = Cyclesim.inputs  sim in
  let o = Cyclesim.outputs sim in

  i.axis_c2h_sink.tready := Bits.vdd;

  let acc = ref [] in
  for _ = 0 to 300 do
    if Bits.to_bool !(o.axis_c2h_src.tvalid) then
      acc := !acc @ [Bits.to_int64_trunc !(o.axis_c2h_src.tdata)];
    cycle sim
  done;
  !acc
;;

(* ========================================================================== *)
(* Duplex Streaming Driver (Stress / Throughput)                              *)
(* ========================================================================== *)

type axis_packet =
  { data : int64
  ; last : bool
  }

let run_duplex
    (sim : Harness.Sim.t)
    ~(packets : axis_packet list)
    ~(expected_words : int)
  =
  let i = Cyclesim.inputs  sim in
  let o = Cyclesim.outputs sim in

  let send_q = ref packets in
  let recv_q = ref [] in
  let timeout = ref 5000 in

  i.axis_c2h_sink.tready := Bits.vdd;

  while
    (!timeout > 0)
    && (not (List.is_empty !send_q) || List.length !recv_q < expected_words)
  do
    (* Drive input *)
    (match !send_q with
     | pkt :: _ ->
       i.axis_h2c_src.tvalid := Bits.vdd;
       i.axis_h2c_src.tdata  := Bits.of_int64_trunc ~width:64 pkt.data;
       i.axis_h2c_src.tkeep  <--. 0xFF;
       i.axis_h2c_src.tlast  := Bits.of_bool pkt.last
     | [] ->
       i.axis_h2c_src.tvalid := Bits.gnd;
       i.axis_h2c_src.tlast  := Bits.gnd;
       i.axis_h2c_src.tdata  := Bits.zero 64);

    cycle sim;
    decr timeout;

    (* Input handshake *)
    let fire_in =
      Bits.to_bool !(i.axis_h2c_src.tvalid)
      && Bits.to_bool !(o.axis_h2c_sink.tready)
    in
    if fire_in then send_q := List.tl_exn !send_q;

    (* Output capture *)
    if Bits.to_bool !(o.axis_c2h_src.tvalid) then
      recv_q := !recv_q @ [Bits.to_int64_trunc !(o.axis_c2h_src.tdata)]
  done;

  if !timeout = 0 then
    printf "WARNING: Timeout\n";

  !recv_q
;;

(* ========================================================================== *)
(* Scenario Runner                                                            *)
(* ========================================================================== *)

let run_scenario ~name ~input =
  Harness.run_advanced ~create:Engine.hierarchical (fun sim ->
    reset sim;

    let expected = Reference.compute input in

    send_matrix sim input;
    let actual = recv_matrix sim in

    let matches = List.equal Int64.equal expected actual in
    print_s [%message name (matches : bool)];

    if not matches then
      print_s [%message "FAILURE"
        (input    : int64 list)
        (expected : int64 list)
        (actual   : int64 list)]
  )
;;

(* ========================================================================== *)
(* Unit Tests                                                                 *)
(* ========================================================================== *)

let%expect_test "Diagonal Independence" =
  run_scenario ~name:"Diagonal" ~input:[1L; 2L; 4L; 8L];
  [%expect {| (Diagonal (matches true)) |}]
;;

let%expect_test "Linear Reduction" =
  run_scenario ~name:"Reduction" ~input:[3L; 1L];
  [%expect {| (Reduction (matches true)) |}]
;;

let%expect_test "Zero Rows" =
  run_scenario ~name:"Zeros" ~input:[0L; 5L; 0L; 5L];
  [%expect {| (Zeros (matches true)) |}]
;;

let%expect_test "Complex Dependency" =
  run_scenario ~name:"Complex" ~input:[7L; 3L; 1L];
  [%expect {| (Complex (matches true)) |}]
;;

let%expect_test "Random Sweep" =
  Random.init 0;
  for i = 1 to 5 do
    let len = 2 + Random.int 8 in
    let input = List.init len ~f:(fun _ -> Random.int64 256L) in
    run_scenario ~name:(sprintf "Random_%d" i) ~input
  done;
  [%expect {|
    (Random_1 (matches true))
    (Random_2 (matches true))
    (Random_3 (matches true))
    (Random_4 (matches true))
    (Random_5 (matches true)) |}]
;;

(* ========================================================================== *)
(* Throughput & Stress                                                        *)
(* ========================================================================== *)

let%expect_test "Back-to-Back Matrices" =
  Harness.run_advanced ~create:Engine.hierarchical (fun sim ->
    reset sim;

    let a = [3L; 1L] in
    let b = [4L; 8L] in
    let expected = Reference.compute a @ Reference.compute b in

    send_matrix sim a;
    send_matrix sim b;

    let actual = recv_matrix sim in
    let matches = List.equal Int64.equal expected actual in

    print_s [%message "Back-to-Back" (matches : bool)]
  );
  [%expect {| (Back-to-Back (matches true)) |}]
;;

let%expect_test "Super Stress (50 Matrices)" =
  Harness.run_advanced ~create:Engine.hierarchical (fun sim ->
    reset sim;
    Random.init 123;

    let scenarios =
      List.init 50 ~f:(fun _ ->
        let len = 20 + Random.int 30 in
        let rows = List.init len ~f:(fun _ -> Random.int64 256L) in
        rows, Reference.compute rows)
    in

    let packets =
      List.concat_map scenarios ~f:(fun (rows, _) ->
        List.mapi rows ~f:(fun i r ->
          { data = r; last = (i = List.length rows - 1) }))
    in

    let expected = List.concat_map scenarios ~f:snd in
    let actual =
      run_duplex sim ~packets ~expected_words:(List.length expected)
    in

    printf "Super Stress (sent %d, received %d, matches %b)\n"
      (List.length packets)
      (List.length actual)
      (List.equal Int64.equal expected actual)
  );
  [%expect {| Super Stress (sent 1754, received 400, matches true) |}]
;;
