let display fmt =
  Printf.ksprintf
    (fun s ->
      print_endline s;
      flush stdout)
    fmt
;;

let rec monitor_process pid =
  display "About to wait";
  match Unix.waitpid [ Unix.WUNTRACED ] pid with
  | _, Unix.WSTOPPED s ->
    display "process %d caught signal %d" pid s;
    Ptrace.cont pid 0;
    monitor_process pid
  | _, Unix.WEXITED s | _, Unix.WSIGNALED s ->
    display "process %d has terminated with code %d" pid s;
    exit 0
;;

let _ =
  if Array.length Sys.argv > 1
  then (
    let pid = int_of_string Sys.argv.(1) in
    display "Attaching process with pid %d" pid;
    at_exit (fun () ->
      display "Detaching process %d" pid;
      Ptrace.detach pid);
    Ptrace.attach pid;
    monitor_process pid)
;;
