include Async_extra.Log.Global

open Async.Std

let unlink_no_fail filename =
  Sys.file_exists_exn filename
  >>= fun b ->
  if b then Unix.unlink filename
  else return ()
