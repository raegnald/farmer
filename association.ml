
type t =
  { target : string
  ; link   : string }

external symlink_file_target_link : string -> string -> int = "caml_symlink_wrapper"
external fnmatch : string -> string -> bool = "caml_fnmatch_wrapper"
external strerror : unit -> string = "caml_strerror_wrapper"

let printf = Printf.printf

let rec symlink ?(origin=Filename.current_dir_name) ~ignores ~cwd { target; link } =
  let target' =
    if Filename.is_relative target
    then Filename.concat cwd target
    else target
  and link' =
    if Filename.is_relative link
    then Filename.concat cwd link
    else link
  in

  let ignores =
    let ignore_file = Filename.concat target' "ignore" in
    if Sys.file_exists ignore_file
    then (
      printf "  Reading ignore file %s\n\n" ignore_file;
      In_channel.(with_open_bin ignore_file input_lines) @ ignores)
    else (
      printf "  Ignore file %s does NOT exist\n\n" ignore_file;
      ignores)
  in

  if not (Sys.file_exists link) then
  begin
    printf "> mkdir %s\n" link';
    Sys.mkdir link 0o777;
  end;

  let sym filename =
    let target'' = Filename.concat target filename
    and link'' = Filename.concat link filename in

    if Sys.is_directory target'' then
      begin
        let assoc = { target = target''
                    ; link = link'' }
        in
        symlink ~origin:(Filename.concat origin "..") ~ignores ~cwd assoc
      end
    else                        (* Normal file *)
      let target = (Filename.concat target' filename)
      and link_name = link''
      in
      if List.exists (fun ignore_pattern -> fnmatch ignore_pattern filename) ignores
      then
        printf "  \027[34mIgnoring %s\027[0m\n\n" target
      else
      begin

        printf "> ln -s \"%s\" \"%s\"\n" target link_name;

        let code = symlink_file_target_link target link_name in
        if code == -1 then
          printf "  \027[31m%s\027[0m\n\n" (strerror ())
        else
          printf "  \027[32mCreated symlink\027[0m\n\n"
      end
  in

  let files = Sys.readdir target in
  Array.iter sym files
