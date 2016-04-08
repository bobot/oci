

open! Core.Std
open! Async.Std
open Oci_Std

let prog, args =
  match Array.to_list Sys.argv with
  | [] | [_] -> "sleep", ["0"]
  | _::prog::args -> prog, args

let () =
  never_returns begin
    Scheduler.go_main
      ~main:(fun () ->
          Process.create_exn ~prog ~args ()
          >>> fun p ->
          printf "process started\n%!";
          wait4 (Process.pid p)
          >>> fun (status, ru) ->
          printf "process stopped\n%s\n%s\n%!"
            (Unix.Exit_or_signal.to_string_hum status)
            (Sexp.to_string_hum
               (Core.Core_unix.Resource_usage.sexp_of_t ru));
          Shutdown.shutdown 0
        ) ()
  end
