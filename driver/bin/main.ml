open Core
open Core_unix
open Ctypes
open Foreign

(* --- C Definition --- *)
let vfio_ctx = structure "vfio_context_t"
let _        = field vfio_ctx "padding_1" int
let _        = field vfio_ctx "padding_2" int
let dev_fd   = field vfio_ctx "device_fd" int
let ()       = seal vfio_ctx

let c_init = foreign "c_init_vfio" (int @-> string @-> ptr vfio_ctx @-> returning int)
let c_bar  = foreign "c_get_bar_info" (int @-> int @-> ptr ulong @-> ptr ulong @-> returning int)

(* --- The Driver --- *)
let init ~group ~addr =
  let ctx = allocate_n vfio_ctx ~count:1 in
  if c_init group addr ctx <> 0 then failwith "VFIO Init failed";

  let fd = getf (!@ ctx) dev_fd in
  let off, sz = allocate ulong Unsigned.ULong.zero, allocate ulong Unsigned.ULong.zero in

  if c_bar fd 0 off sz <> 0 then failwith "BAR Info failed";

  (* Map directly to Array1 for maximum performance (no Genarray overhead) *)
  let mem =
    let len = Unsigned.ULong.to_int (!@ sz) / 4 in
    let pos = Int64.of_int (Unsigned.ULong.to_int (!@ off)) in
    Core_unix.map_file (File_descr.of_int fd) ~pos ~shared:true
      Bigarray.int32 Bigarray.c_layout [| len |]
    |> Bigarray.array1_of_genarray
  in
  (mem, ctx) (* Return ctx to prevent Garbage Collection from closing FDs *)

(* --- The Application --- *)
let () =
  let (mmio, _ctx) = init ~group:24 ~addr:"0000:09:00.0" in
  let base = 0x40000 / 4 in (* Pre-calculate index offset *)

  (* Clean, type-inferred reads/writes *)
  mmio.{base}     <- Int32.of_int_exn 64;
  mmio.{base + 1} <- Int32.of_int_exn 64;

  Printf.printf "HW Result: %ld\n%!" mmio.{base}
