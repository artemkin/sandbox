
open Core

let do_grep pattern ch =
  let f line =
    if String.is_substring line ~substring:pattern then begin
      print_string line;
      print_string "\n"
    end
  in
  In_channel.iter_lines ch ~f;
  Out_channel.flush stdout

let grep pattern filename =
  if (filename = "-") then
    do_grep pattern In_channel.stdin
  else
    In_channel.(with_file filename ~f:(fun file -> do_grep pattern file))

let zstdgrep pattern filename =
  let p = Unix.open_process_in ("unzstd -c " ^ filename) in
  do_grep pattern p;
  let status = Unix.close_process_in p in
  eprintf "unzstd exit status: %s" (Unix.Exit_or_signal.to_string_hum status)

let () =
  match Array.to_list Sys.argv |> List.tl_exn with
  | [ "-z"; pattern; filename ] -> zstdgrep pattern filename
  | [ pattern; filename ] -> grep pattern filename
  | _ -> printf "usage: %s [-z] pattern filename\n" Sys.argv.(0)

