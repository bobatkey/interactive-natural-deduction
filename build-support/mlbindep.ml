open Printf

let split ~on str =
  let open String in
  let rec scan i =
    if i = length str || str.[i] <> on then i
    else scan (i+1)
  in
  match index str on with
    | exception Not_found ->
       str,
       None
    | i ->
       let j = scan i in
       sub str 0 i,
       Some (sub str j (length str - j))

let parse_args () =
  let cons_inplace l x = l := x :: !l in
  let include_dirs    = ref [] in
  let input_filenames = ref [] in
  Arg.parse
    [ "-I", Arg.String (cons_inplace include_dirs),
      "directories to search for dependencies"
    ]
    (cons_inplace input_filenames)
    "FIXME";
  List.rev !include_dirs,
  List.rev !input_filenames

let remove_comments line =
  match String.index line '#' with
    | exception Not_found -> line
    | i                   -> String.sub line 0 i

let parse_mlbin filename =
  let ch = open_in filename in
  let rec read_loop items_rev =
    match input_line ch with
      | exception End_of_file ->
         List.rev items_rev
      | line ->
         let line = String.trim (remove_comments line) in
         match split ~on:' ' line with
           | ("module" | "MOD"), Some modulename ->
              read_loop (`Module modulename :: items_rev)
           | ("library" | "LIB"), Some libraryname ->
              read_loop (`Library libraryname :: items_rev)
           | "", None ->
              read_loop items_rev
           | _ ->
              failwith
                (sprintf "line '%s' not understood" line)
  in
  try
    let items = read_loop [] in close_in ch; items
  with e -> close_in ch; raise e

(*
   - for each `Module m, search for
       <include_dir>/m.ml, <include_dir>/m.mll, <include_dir>/m.mly
       when found, add <include_dir>/_build/m.cmo to the list of deps
                   add <include_dir>/_build/m.cmx

   - for 
*)

let rec find_first f = function
  | [] -> None
  | x::xs ->
     (match f x with Some y -> Some y | None -> find_first f xs)

let (</>) = Filename.concat

let find_prereq search_dirs suffixes name =
  search_dirs |> find_first (fun dir ->
      suffixes |> find_first (fun suffix ->
          let candidate = dir </> name ^ "." ^ suffix in
          if Sys.file_exists candidate then Some dir else None))

let get_all_prereqs include_dirs =
  let rec loop byte native = function
    | [] ->
       (List.rev byte, List.rev native)
    | `Module modulename :: items ->
       (match find_prereq include_dirs ["ml";"mll";"mly"] modulename with
         | None ->
            failwith (sprintf "Module '%s' not found" modulename)
         | Some dir ->
            loop
              ((dir </> "_build" </> modulename ^ ".cmo") :: byte)
              ((dir </> "_build" </> modulename ^ ".cmx") :: native)
              items)
    | `Library libraryname :: items ->
       (match find_prereq include_dirs ["mllib"] libraryname with
         | None ->
            failwith (sprintf "Library '%s' not found" libraryname)
         | Some dir ->
            loop
              ((dir </> "_build" </> libraryname ^ ".cma") :: byte)
              ((dir </> "_build" </> libraryname ^ ".cmxa") :: native)
              items)
  in
  loop [] []

let () =
  let open Filename in
  let include_dirs, input_filenames = parse_args () in
  input_filenames |> List.iter begin fun filename ->
    let is_mlbin = check_suffix filename ".mlbin" in
    let items    = parse_mlbin filename in
    let name     = chop_extension (basename filename) in
    let dir      = dirname filename in
    let byte_tgt, native_tgt = 
      if is_mlbin then
        (dir </> "_build" </> "byte_bin" </> name,
         dir </> "_build" </> "native_bin" </> name)
      else
        (dir </> "_build" </> name ^ ".cma",
         dir </> "_build" </> name ^ ".cmxa")
    in
    let byte_objs, native_objs =
      get_all_prereqs include_dirs items
    in
    printf "%s: \\\n  %s\n"
      byte_tgt
      (String.concat " \\\n  " byte_objs);
    printf "%s: \\\n  %s\n"
      native_tgt
      (String.concat " \\\n  " native_objs)
  end
