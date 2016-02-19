open Core.Std
open Async.Std


module Pp = Oci_pp
include Log.Global

let unlink_no_fail filename =
  (** Sys.file_exists follows symlink *)
  Monitor.try_with ~here:[%here]
    (fun () -> Unix.lstat filename)
  >>= function
  | Ok _ -> Unix.unlink filename
  | Error _ -> return ()
  (* | Error (Unix.Unix_error _) -> return () *)
  (* | Error exn -> raise exn *)

let unlink_no_fail_blocking filename =
  let open Core.Std in
  (** Sys.file_exists follows symlink *)
  try
    ignore (Unix.lstat filename);
    Unix.unlink filename
  with _ -> ()

let backup_and_open_file file =
  let file_bak = Oci_Filename.add_extension file "bak" in
  Sys.file_exists_exn file
  >>= fun exi ->
  begin if exi then begin
      unlink_no_fail file_bak
      >>= fun () ->
      Unix.rename ~src:file ~dst:file_bak
    end
    else return ()
  end
  >>= fun () ->
  Writer.open_file file

let open_if_exists file f =
  Sys.file_exists_exn file
  >>= fun exi ->
  if exi then begin
    Reader.open_file file
    >>= fun reader ->
    f reader
    >>= fun () ->
    Reader.close reader
  end
  else return ()

let read_if_exists file bin_reader_t f =
  open_if_exists file
    (fun reader ->
       Reader.read_bin_prot reader bin_reader_t
       >>= function
       | `Eof -> return ()
       | `Ok r -> f r
    )
