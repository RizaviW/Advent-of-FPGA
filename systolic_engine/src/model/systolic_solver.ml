open! Core

(** Computes the Reduced Row Echelon Form (basis) of a list of 64-bit integers *)
let compute_rref input_rows =
  (* Find the index of the most significant bit set *)
  let get_pivot_index x =
    let rec loop i =
      if i >= 64 then None
      else if Int64.((x land (1L lsl i)) <> 0L) then Some i
      else loop (i + 1)
    in
    loop 0
  in

  (* Forward reduction phase *)
  let basis = Array.create ~len:64 0L in
  List.iter input_rows ~f:(fun row ->
    let rec reduce r =
      match get_pivot_index r with
      | None -> ()
      | Some pivot ->
        if Int64.(basis.(pivot) = 0L) then basis.(pivot) <- r
        else reduce Int64.(r lxor basis.(pivot))
    in
    reduce row
  );

  (* Backward reduction phase to clean up columns *)
  for i = 0 to 63 do
    if Int64.(basis.(i) <> 0L) then
      for j = 0 to 63 do
        if i <> j && Int64.(basis.(j) <> 0L) then
          let has_bit_set = Int64.((basis.(j) land (1L lsl i)) <> 0L) in
          if has_bit_set then basis.(j) <- Int64.(basis.(j) lxor basis.(i))
      done
  done;

  Array.to_list basis |> List.filter ~f:(fun x -> Int64.(x <> 0L))
;;
