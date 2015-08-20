include Async_extra.Log.Global

open Async.Std

let unlink_no_fail filename =
  Sys.file_exists_exn filename
  >>= fun b ->
  if b then Unix.unlink filename
  else return ()

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
