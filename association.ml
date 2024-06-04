
type t =
  { target : string
  ; link   : string }

external symlink_file_target_link : string -> string -> int = "caml_symlink_wrapper"
external strerror : unit -> string = "caml_strerror_wrapper"

let falsear = false

let printf = Printf.printf

let rec symlink ?(origin=Filename.current_dir_name) cwd { target; link } =
  let target' =
    if Filename.is_relative target
    then Filename.concat cwd target
    else target
  and link' =
    if Filename.is_relative link
    then Filename.concat cwd link
    else link
  in

  if not (Sys.file_exists link) then
  begin
    printf "> mkdir %s\n" link';
    Sys.mkdir link 0o777;
  end;

  printf "> cd %s\n\n" link';

  let sym filename =
    let target'' = Filename.concat target filename
    and link'' = Filename.concat link filename in

    if Sys.is_directory target'' then
      begin
        let assoc = { target = target''
                    ; link = link'' }
        in
        symlink ~origin:(Filename.concat origin "..") cwd assoc;
        printf "> cd ..\n\n";
      end
    else                        (* Normal file *)
      begin
        let target = (Filename.concat target' filename)
        and link_name = link'' in

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
