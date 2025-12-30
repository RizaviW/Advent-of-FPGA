open! Core
open! Hardcaml
open! Systolic_engine

(* ============================================================= *)
(* RTL generation                                                *)
(* ============================================================= *)

let generate_systolic_engine_rtl () =
  let module C = Circuit.With_interface (I) (O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"application" (hierarchical scope) in
  let rtl =
    Rtl.create
      ~database:(Scope.circuit_database scope)
      Verilog
      [ circuit ]
    |> Rtl.full_hierarchy
    |> Rope.to_string
  in
  print_endline rtl
;;

(* ============================================================= *)
(* Command-line interface                                        *)
(* ============================================================= *)

let systolic_engine_rtl_command =
  Command.basic
    ~summary:"Generate systolic_engine Verilog"
    (Command.Param.return generate_systolic_engine_rtl)
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Produce RTL"
       [ "systolic-engine", systolic_engine_rtl_command ])
;;
