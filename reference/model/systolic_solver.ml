(** =============================================================================
    THE SYSTOLIC SOLVER :: GF(2) LINEAR ALGEBRA
    =============================================================================

    A streaming implementation of Gaussian Elimination over a Galois Field (2).
    Designed for finding Minimum Hamming Weight solutions to underdetermined systems.

    Compile: ocamlfind ocamlopt -package str -linkpkg systolic_solver.ml -o solver
    Usage:   ./solver [optional: line_number for debug mode]
*)

open Printf

(* ========================================================================== *)
(* TYPE DEFINITIONS                                                           *)
(* ========================================================================== *)

type matrix = int array array
type vector = int array
type system = { matrix : matrix; target : vector }

type mode = 
  | Batch
  | Debug of int

(* ========================================================================== *)
(* UTILITIES & LOGGING                                                        *)
(* ========================================================================== *)

module Logger = struct
  type t = {
    (* The 'a. quantifier allows this function to work with ANY format string *)
    log : 'a. ('a, out_channel, unit) format -> 'a;
    matrix : string -> matrix -> unit;
  }

  let silent = {
    (* ifprintf consumes arguments based on format but prints nothing *)
    log = (fun fmt -> Printf.ifprintf stdout fmt); 
    matrix = (fun _ _ -> ());
  }

  let verbose = {
    log = (fun fmt -> printf fmt);
    matrix = (fun title mat ->
      let rows = Array.length mat in
      if rows > 0 then begin
        let cols = Array.length mat.(0) in
        printf "\n--- %s (%dx%d) ---\n" title rows cols;
        Array.iteri (fun i row ->
          printf "  Row %2d: | " i;
          Array.iter (printf "%d ") row;
          printf "|\n"
        ) mat
      end
    );
  }
end

(* ========================================================================== *)
(* INPUT PARSING                                                              *)
(* ========================================================================== *)

module Parser = struct
  let re_vector = Str.regexp "\\[[#.]+\\]"
  let re_button = Str.regexp "([0-9, ]+)"

  let parse_target line =
    let content = String.sub line 1 (String.length line - 2) in
    Array.init (String.length content) (fun i -> if content.[i] = '#' then 1 else 0)

  let parse_indices str =
    let content = String.sub str 1 (String.length str - 2) in
    if String.trim content = "" then []
    else 
      String.split_on_char ',' content 
      |> List.map (fun s -> int_of_string (String.trim s))

  let build_matrix target_str button_strs =
    let target = parse_target target_str in
    let rows   = Array.length target in
    let cols   = List.length button_strs in
    let mat    = Array.make_matrix rows cols 0 in
    
    button_strs 
    |> List.map parse_indices 
    |> List.iteri (fun col indices ->
         indices |> List.iter (fun row ->
           if row < rows then mat.(row).(col) <- 1
         )
       );
    { matrix = mat; target }

  let parse_line line =
    try
      let _     = Str.search_forward re_vector line 0 in
      let t_str = Str.matched_string line in
      
      let rec collect_buttons pos acc =
        try
          let _ = Str.search_forward re_button line pos in
          collect_buttons (Str.match_end ()) (Str.matched_string line :: acc)
        with Not_found -> List.rev acc
      in
      
      Some (build_matrix t_str (collect_buttons 0 []))
    with Not_found -> None
end

(* ========================================================================== *)
(* CORE LOGIC: SYSTOLIC GF(2) SOLVER                                          *)
(* ========================================================================== *)

module Solver = struct
  
  let xor_rows r1 r2 = Array.map2 (lxor) r1 r2

  let augment sys =
    let rows = Array.length sys.matrix in
    Array.init rows (fun r -> 
      Array.append sys.matrix.(r) [| sys.target.(r) |]
    )

  (** Phase 1: Forward Elimination (The Filling) 
      Streams rows into a conceptual array of Processing Elements (PEs). *)
  let forward_pass ~(logger:Logger.t) augmented_matrix =
    logger.log "\n>>> PHASE 1: FORWARD ELIMINATION <<<\n";
    
    let num_cols = Array.length augmented_matrix.(0) in
    let pe_array = Array.make num_cols None in

    let rec inject_row row col =
      if col >= num_cols then ()
      else if row.(col) = 0 then 
        (* Skip zero columns to find the pivot *)
        inject_row row (col + 1)
      else 
        match pe_array.(col) with
        | None -> 
            (* Found an empty PE slot for this pivot *)
            logger.log "  PE %2d: Captured Pivot\n" col;
            pe_array.(col) <- Some row
        | Some stored_row -> 
            (* Collision: Eliminate and push downstream *)
            inject_row (xor_rows row stored_row) (col + 1)
    in

    Array.iter (fun row -> inject_row row 0) augmented_matrix;
    pe_array

  (** Phase 2: Backward Elimination (The Draining)
      Clears columns above pivots to achieve RREF. *)
  let backward_pass ~(logger:Logger.t) pe_array =
    logger.log "\n>>> PHASE 2: BACKWARD ELIMINATION <<<\n";
    
    let num_cols = Array.length pe_array in
    
    (* Iterate backwards from the last column *)
    for src = num_cols - 1 downto 0 do
      match pe_array.(src) with
      | None -> ()
      | Some travelling_row ->
          for tgt = src - 1 downto 0 do
            match pe_array.(tgt) with
            | None -> ()
            | Some stationary_row ->
                if stationary_row.(src) = 1 then begin
                  logger.log "  Clearing Col %d in Row %d\n" src tgt;
                  pe_array.(tgt) <- Some (xor_rows stationary_row travelling_row)
                end
          done
    done;
    
    (* Extract non-empty rows *)
    let rref = Array.of_list (List.filter_map Fun.id (Array.to_list pe_array)) in
    logger.matrix "RREF Matrix" rref;
    rref

  (** Phase 3: Solution Extraction
      Identifies Free Variables and performs exhaustive search for Min-Weight. *)
  let minimize ~(logger:Logger.t) rref =
    let rows = Array.length rref in
    if rows = 0 then None else
    let cols = Array.length rref.(0) in
    let num_vars = cols - 1 in (* Last column is augmented vector *)

    (* Identify Pivot vs Free variables *)
    let is_pivot = Array.make num_vars false in
    Array.iter (fun row ->
      let rec mark_pivot c =
        if c < num_vars then
          if row.(c) = 1 then is_pivot.(c) <- true else mark_pivot (c + 1)
      in mark_pivot 0
    ) rref;

    let free_indices = 
      List.filter (fun c -> not is_pivot.(c)) (List.init num_vars Fun.id) 
    in

    logger.log "\n>>> PHASE 3: OPTIMIZATION <<<\n";
    logger.log "  Free Variables: %d\n" (List.length free_indices);

    let min_weight = ref None in
    let combinations = 1 lsl (List.length free_indices) in

    (* Iterate through all combinations of free variables *)
    for i = 0 to combinations - 1 do
      try
        let sol = Array.make num_vars 0 in
        
        (* Set Free Variables *)
        List.iteri (fun bit idx ->
          if (i land (1 lsl bit)) <> 0 then sol.(idx) <- 1
        ) free_indices;

        (* Solve Dependent Variables *)
        for r = 0 to rows - 1 do
          let row = rref.(r) in
          let rec find_pivot c =
            if c >= num_vars then None
            else if row.(c) = 1 then Some c else find_pivot (c + 1)
          in
          match find_pivot 0 with
          | Some p ->
              let sum = ref 0 in
              List.iter (fun f -> 
                if f > p && row.(f) = 1 then sum := !sum lxor sol.(f)
              ) free_indices;
              sol.(p) <- row.(num_vars) lxor !sum
          | None -> 
              (* Contradiction: 0 = 1 *)
              if row.(num_vars) = 1 then raise Exit
        done;

        (* Calculate Hamming Weight *)
        let w = Array.fold_left (+) 0 sol in
        
        begin match !min_weight with
        | None -> min_weight := Some w
        | Some min_w -> if w < min_w then min_weight := Some w
        end;
        
        let s_str = String.concat "" (Array.to_list sol |> List.map string_of_int) in
        logger.log "  [Comb %d] Sol: %s | Weight: %d\n" i s_str w;

      with Exit -> () (* Impossible configuration *)
    done;
    !min_weight

  (** Main Pipeline *)
  let solve ~(logger:Logger.t) system =
    let augmented = augment system in
    logger.matrix "Initial Augmented Matrix" augmented;
      
    augmented
    |> forward_pass  ~logger
    |> backward_pass ~logger
    |> minimize      ~logger
end

(* ========================================================================== *)
(* LIBRARY INTERFACE & UTILITIES                                              *)
(* ========================================================================== *)

let read_systems filename =
  printf "Reading %s...\n" filename;
  try
    let ic = open_in filename in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      let rec loop acc =
        try
          let line = input_line ic in
          match Parser.parse_line line with
          | Some sys -> loop (sys :: acc)
          | None -> loop acc
        with End_of_file -> List.rev acc
      in loop []
    )
  with Sys_error msg -> 
    printf "Error opening file: %s\n" msg; exit 1

(** Executes the solver on a list of systems in silent mode. *)
let run_batch systems =
  let count = List.length systems in
  printf "Found %d systems. Solving...\n" count;
  let t0 = Sys.time () in
  
  let total_weight = 
    List.fold_left (fun acc sys ->
      match Solver.solve ~logger:Logger.silent sys with
      | Some w -> acc + w
      | None -> acc
    ) 0 systems 
  in
  
  let dt = Sys.time () -. t0 in
  printf "------------------------------------------------\n";
  printf "Total Minimum Hamming Weight: %d\n" total_weight;
  printf "Execution Time: %.4f seconds\n" dt;
  printf "------------------------------------------------\n";
  total_weight

(** Executes the solver on a specific system with full logging. *)
let run_debug systems line_num =
  let idx = line_num - 1 in
  if idx < 0 || idx >= List.length systems then begin
    printf "Error: Line %d out of bounds.\n" line_num;
    None
  end else begin
    printf "\n==============================================\n";
    printf "      DEBUG MODE: SYSTEM #%d\n" line_num;
    printf "==============================================\n";
    
    let sys = List.nth systems idx in
    let result = Solver.solve ~logger:Logger.verbose sys in
    begin match result with
    | Some w -> printf "\nSUCCESS: Minimum Weight = %d\n" w
    | None   -> printf "\nFAILURE: No Solution\n"
    end;
    result
  end

(* ========================================================================== *)
(* MAIN EXECUTION                                                             *)
(* ========================================================================== *)

let () =
  (* Only execute the CLI logic if not being used as a library module *)
  if !Sys.interactive = false then begin
    let mode = 
      if Array.length Sys.argv > 1 then
        try Debug (int_of_string Sys.argv.(1))
        with Failure _ -> 
          printf "Usage: %s [line_number]\n" Sys.argv.(0); exit 1
      else Batch 
    in

    let systems = read_systems "factory_input.txt" in
    
    match mode with
    | Batch             -> ignore (run_batch systems)
    | Debug line_num -> ignore (run_debug systems line_num)
  end
