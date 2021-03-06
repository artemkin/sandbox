
open Core.Std

let skip_blank lst =
  List.filter_map lst ~f:(fun str ->
      let str = String.strip str in
      if String.length str > 0 then Some str else None)

let print_error = function
  | `Wrong_path -> print_endline "Wrong path"
  | `Wrong_command -> print_endline "Wrong command"
  | `Unknown_command_name name -> printf "Unknown command name: %s\n" name
  | `Can't_remove_current_directory -> print_endline "Can't remove current directory"
  | `Dir_not_found -> print_endline "Directory not found"
  | `Dir_not_empty -> print_endline "Directory not empty"
  | `Not_dir -> print_endline "This is not directory"
  | `File_not_found -> print_endline "File not found"
  | `Not_file -> print_endline "This is not file"
  | `Already_exists -> print_endline "Already exists"
  | `Destination_not_found -> print_endline "Destination not found"
  | `Source_not_found -> print_endline "Source not found"
  | `Hard_linked -> print_endline "Hard linked"

let run_command fs command path =
  match command with
  | "CD" -> File_system.change_dir fs path
  | "MD" -> File_system.make_dir fs path
  | "MF" -> File_system.make_file fs path
  | "RD" -> File_system.remove_dir fs path
  | "DEL" -> File_system.remove_file fs path
  | "DELTREE" -> File_system.remove_dir_recursive fs path
  | _ -> Error (`Unknown_command_name command)

let run_command_2 fs command src dest =
  match command with
  | "COPY" -> File_system.copy fs ~src ~dest
  | "MHL" -> File_system.make_link fs ~link_kind:File_system.Hard_link ~src ~dest
  | "MDL" -> File_system.make_link fs ~link_kind:File_system.Dynamic_link ~src ~dest
  | _ -> Error (`Unknown_command_name command)

let () =
  let fs = File_system.empty in
  In_channel.input_lines stdin
  |> skip_blank
  |> List.map ~f:String.uppercase
  |> List.fold ~init:(Ok fs) ~f:(fun fs str ->
      match fs with
      | Error _ -> fs
      | Ok fs ->
        match skip_blank (String.split str ~on:' ') with
        | [command; path] -> run_command fs command path
        | [command; path1; path2] -> run_command_2 fs command path1 path2
        | _ -> Error `Wrong_command)
  |> function
  | Error err -> print_error err
  | Ok fs -> File_system.print fs

