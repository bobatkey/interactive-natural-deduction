open Printf

let split_list ?(skip_empty=false) ~on str =
  let open String in
  let len = length str in
  let rec loop i collected =
    match index_from str i on with
      | exception Not_found ->
         if i = len && skip_empty then
           collected
         else
           sub str i (len - i)::collected
      | j ->
         if j = i && skip_empty then
           loop (j+1) collected
         else
           loop (j+1) (sub str i (j-i)::collected)
  in
  [] |> loop 0 |> List.rev

let fold_lines f filename i =
  let ch = open_in filename in
  let rec loop a =
    match input_line ch with
      | exception End_of_file -> a
      | line -> loop (f line a)
  in
  try
    let r = loop i in close_in ch; r
  with e -> close_in ch; raise e

(**********************************************************************)
let parse_args () =
  let action = ref None in
  let set_action a () =
    match !action with
      | None -> action := Some a
      | Some _ -> raise (Arg.Bad "multiple actions specified")
  in
  let input = ref None in
  let spec =
    [ "-ocamldep-flags",
      Arg.Unit (set_action `ocamldep_flags),
      "output ocamldep flags"

    ; "-ppx-bins",
      Arg.Unit (set_action `ppx_bins),
      "output list of ppx binaries"

    ; "-ocamlc-flags",
      Arg.Unit (set_action `ocamlc_flags),
      "output ocamlc flags"

    ; "-src-dirs",
      Arg.Unit (set_action `srcdirs),
      "output source directory options"

    ; "-bin-dirs",
      Arg.Unit (set_action `bindirs),
      "output binary directory options"
    ]
  in
  Arg.parse
    spec
    (fun filename ->
       match !input with
         | None   -> input := Some filename
         | Some _ -> raise (Arg.Bad "multiple input files"))
    "FIXME: usage";
  match !action, !input with
    | Some action, Some input -> action, input
    | _, _ ->
       Arg.usage spec "FIXME: usage";
       exit 1

(**********************************************************************)
type dot_merlin =
  { source_dirs : string list
  ; binary_dirs : string list
  ; packages    : string list
  ; flags       : string list
  }

let empty_dot_merlin =
  { source_dirs = []; binary_dirs = []; packages = []; flags = [] }

let parse_dotmerlin filename =
  let read_directive line dot_merlin =
    let line = String.trim line in (* FIXME: and remove comments *)
    match split_list ~on:' ' ~skip_empty:true line with
      | [] ->
         dot_merlin
      | "B"::bin_dirs ->
         { dot_merlin with binary_dirs = dot_merlin.binary_dirs @ bin_dirs }
      | "S"::src_dirs ->
         { dot_merlin with source_dirs = dot_merlin.source_dirs @ src_dirs }
      | "PKG"::package_names ->
         { dot_merlin with packages = dot_merlin.packages @ package_names }
      | "FLG"::flags ->
         { dot_merlin with flags = dot_merlin.flags @ flags }
      | ("CMI" | "CMT" | "EXT")::_ ->
         dot_merlin
      | _ ->
         failwith
           (sprintf ".merlin directive '%s' not understood" line)
  in
  empty_dot_merlin |> fold_lines read_directive filename

(**********************************************************************)
(* FIXME: Urrgh. Symbolic links? *)
let canonicalise_path path =
  path
  |> split_list ~on:'/' ~skip_empty:true
  |> (List.fold_left
        (fun rev_path -> function
           | "." -> rev_path
           | ".." ->
              (match rev_path with
                | [] -> [".."]
                | _::rev_path -> rev_path)
           | component ->
              component :: rev_path)
        [])
  |> List.rev
  |> String.concat "/"

let canonicalise_and_extract_ppx_binaries srcdir flags =
  let rec loop r_flags ppx_bins = function
    | [] ->
       List.rev r_flags, ppx_bins
    | "-ppx"::ppx_name::flags ->
       let ppx_name = canonicalise_path (Filename.concat srcdir ppx_name) in
       loop (ppx_name::"-ppx"::r_flags) (ppx_name::ppx_bins) flags
    | flag::flags ->
       loop (flag::r_flags) ppx_bins flags
  in
  loop [] [] flags

let () =
  let action,filename = parse_args () in
  let dotmerlin = parse_dotmerlin filename in
  let dirname   = Filename.dirname filename in
  let canonicalise_name name =
    canonicalise_path (Filename.concat dirname name)
  in
  let flags, ppx_bins =
    canonicalise_and_extract_ppx_binaries dirname dotmerlin.flags
  in
  match action with
    | `ppx_bins ->
       print_endline (String.concat " " ppx_bins)
    | `ocamlc_flags ->
       printf "%s %s\n"
         (String.concat " " flags)
         (String.concat " "
            (List.map (fun s -> "-package "^s)
               dotmerlin.packages))
    | `ocamldep_flags ->
       printf "%s\n"
         (String.concat " "
            (List.map (fun s -> "-package "^s)
               dotmerlin.packages))
(*         (String.concat " "
            (List.map (fun s -> "-ppx "^s)
               ppx_bins)) *)
    | `srcdirs ->
       printf "-I %s %s\n"
         dirname
         (String.concat " "
            (List.map (fun n -> "-I "^canonicalise_name n)
               dotmerlin.source_dirs))
    | `bindirs ->
       printf "-I %s/_build %s\n"
         dirname
         (String.concat " "
            (List.map (fun n -> "-I "^canonicalise_name n)
               dotmerlin.binary_dirs))
