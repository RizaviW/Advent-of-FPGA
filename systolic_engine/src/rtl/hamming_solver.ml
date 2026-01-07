open! Core
open! Hardcaml
open! Signal
open! Always

(* ============================================================= *)
(* Parameters                                                    *)
(* ============================================================= *)

module type Config = sig
  val data_width  : int
  val generations : int
end

module Make (C : Config) = struct
  let weight_width      = Int.ceil_log2 (C.data_width + 1)
  let generator_width   = Int.ceil_log2 C.generations

(* ============================================================= *)
(* Inputs                                                        *)
(* ============================================================= *)

  module I = struct
    type 'a t =
      { clock           : 'a
      ; reset_n         : 'a
      ; start           : 'a
      ; particular      : 'a [@bits C.data_width]
      ; generators      : 'a list [@length C.generations] [@bits C.data_width]
      ; nullity_count   : 'a [@bits generator_width]
      }
    [@@deriving hardcaml]
  end

(* ============================================================= *)
(* Outputs                                                       *)
(* ============================================================= *)

  module O = struct
    type 'a t =
      { valid_out       : 'a
      ; minimum_weight  : 'a [@bits weight_width]
      ; minimum_vector  : 'a [@bits C.data_width]
      }
    [@@deriving hardcaml]
  end

(* ============================================================= *)
(* Implementation Helpers                                        *)
(* ============================================================= *)

  (* Logarithmic priority encoder for O(log N) combinational depth *)
  let priority_encoder_tree (mask : Signal.t) =
    let rec build bits =
      match bits with
      | [bit_signal] -> bit_signal, zero generator_width
      | _ ->
        let length      = List.length bits in
        let left, right = List.split_n bits (length / 2) in
        let valid_left,  index_left  = build left in
        let valid_right, index_right = build right in
        
        let valid_out   = valid_left |: valid_right in
        let index_out   = 
          mux2 valid_left 
            (uresize index_left ~width:generator_width) 
            (uresize index_right ~width:generator_width |: 
              (of_int_trunc ~width:generator_width (List.length left))) 
        in
        valid_out, index_out
    in
    let _, winner_index = build (bits_lsb mask) in
    winner_index

  (* Chunked popcount for shorter critical path using 4-bit nibbles *)
  let popcount_chunked (signal_in : Signal.t) =
    let nibble_chunks = split_lsb signal_in ~part_width:4 in
    let nibble_sums   = List.map nibble_chunks ~f:(fun nibble -> 
      mux nibble (List.init 16 ~f:(fun integer_val -> 
        let bits  = Bits.of_int_trunc ~width:4 integer_val in
        let count = Bits.to_int_trunc (Bits.popcount bits) in
        of_int_trunc ~width:weight_width count
      ))
    ) in
    List.reduce_exn nibble_sums ~f:(+:)

(* ============================================================= *)
(* Implementation                                                *)
(* ============================================================= *)

  let create (i : Signal.t I.t) : Signal.t O.t =
    let spec = Reg_spec.create ~clock:i.I.clock ~reset:(~:(i.I.reset_n)) () in

    (* --------------------------------------------------------------------- *)
    (* Internal State                                                        *)
    (* --------------------------------------------------------------------- *)

    let solver_active       = Variable.reg spec ~width:1 ~reset_to:Bits.gnd in
    let enumeration_count   = Variable.reg spec ~width:(generator_width + 1) ~reset_to:(Bits.zero (generator_width + 1)) in
    let gray_code_register  = Variable.reg spec ~width:(generator_width + 1) ~reset_to:(Bits.zero (generator_width + 1)) in
    
    let current_vector      = Variable.reg spec ~width:C.data_width ~reset_to:(Bits.zero C.data_width) in
    let current_weight      = Variable.reg spec ~width:weight_width ~reset_to:(Bits.zero weight_width) in
    
    let best_vector_seen    = Variable.reg spec ~width:C.data_width ~reset_to:(Bits.zero C.data_width) in
    let best_weight_seen    = Variable.reg spec ~width:weight_width ~reset_to:(Bits.of_int_trunc ~width:weight_width C.data_width) in
    let solution_done       = Variable.reg spec ~width:1 ~reset_to:Bits.gnd in

    (* --------------------------------------------------------------------- *)
    (* Gray Code Delta Logic                                                 *)
    (* --------------------------------------------------------------------- *)

    let next_count          = enumeration_count.value +:. 1 in
    let count_shifted_right = gnd @: (next_count.:[generator_width, 1]) in
    let gray_code_next      = next_count ^: count_shifted_right in
    
    let flip_mask           = gray_code_next ^: gray_code_register.value in
    
    (* Structural Masking: constrain flips to the valid nullspace dimension *)
    let nullity_mask        = (binary_to_onehot i.I.nullity_count) -:. 1 in
    let safe_flip_mask      = flip_mask &: uresize nullity_mask ~width:(generator_width + 1) in
    let flip_exists         = safe_flip_mask <>: zero (generator_width + 1) in
    
    let flip_index          = priority_encoder_tree (uresize safe_flip_mask ~width:C.generations) in
    let generator_to_flip   = mux2 flip_exists (mux flip_index i.I.generators) (zero C.data_width) in
    
    let weight_of_generator = popcount_chunked generator_to_flip in
    let weight_of_common    = popcount_chunked (current_vector.value &: generator_to_flip) in
    
    (* w(A ^ B) = w(A) + w(B) - 2*w(A & B) *)
    let next_weight         = (current_weight.value +: weight_of_generator) -: (uresize (weight_of_common @: gnd) ~width:weight_width) in
    let next_vector         = current_vector.value ^: generator_to_flip in

    (* --------------------------------------------------------------------- *)
    (* Search Control                                                        *)
    (* --------------------------------------------------------------------- *)

    let search_stop_limit   = binary_to_onehot i.I.nullity_count in
    let stop_condition_met  = (enumeration_count.value ==: uresize search_stop_limit ~width:(generator_width + 1)) in
    let zero_weight_found   = (best_weight_seen.value ==: zero weight_width) in

    compile
      [ solution_done <-- gnd;

        if_ i.I.start
          [ solver_active      <-- vdd
          ; enumeration_count  <-- zero (generator_width + 1)
          ; gray_code_register <-- zero (generator_width + 1)
          ; current_vector     <-- i.I.particular
          ; current_weight     <-- popcount_chunked i.I.particular
          ; best_vector_seen   <-- i.I.particular
          ; best_weight_seen   <-- popcount_chunked i.I.particular
          ]
        [ if_ solver_active.value
            [ if_ (zero_weight_found |: stop_condition_met)
                [ solver_active <-- gnd
                ; solution_done <-- vdd (* Pulses for exactly one cycle *)
                ]
                [ enumeration_count  <-- next_count
                ; gray_code_register <-- gray_code_next
                ; current_vector     <-- next_vector
                ; current_weight     <-- next_weight
                
                ; if_ (next_weight <: best_weight_seen.value)
                    [ best_weight_seen <-- next_weight
                    ; best_vector_seen <-- next_vector
                    ] []
                ]
            ] []
        ]
      ];

    { O.
      valid_out      = solution_done.value
    ; minimum_weight = best_weight_seen.value
    ; minimum_vector = best_vector_seen.value
    }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"hamming_solver" (fun _scope -> create)
end
