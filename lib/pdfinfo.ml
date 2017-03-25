open Base

type 'a sexp_option = 'a option

module Encrypted = struct
  type t =
    { print     : bool
    ; copy      : bool
    ; change    : bool
    ; add_notes : bool
    ; algorithm : string
    } [@@deriving sexp]
end

type t =
  { author         : string sexp_option
  ; creation_date  : string sexp_option
  ; creator        : string sexp_option
  ; encrypted      : Encrypted.t sexp_option
  ; file_size      : int64  sexp_option
  ; javascript     : bool   sexp_option
  ; mod_date       : string sexp_option
  ; optimized      : bool   sexp_option
  ; page_rot       : int    sexp_option
  ; pages          : int    sexp_option
  ; pdf_version    : float  sexp_option
  ; producer       : string sexp_option
  ; subject        : string sexp_option
  ; tagged         : bool   sexp_option
  ; title          : string sexp_option
  } [@@deriving sexp]

let default =
  { author         = None
  ; creation_date  = None
  ; creator        = None
  ; encrypted      = None
  ; file_size      = None
  ; javascript     = None
  ; mod_date       = None
  ; optimized      = None
  ; page_rot       = None
  ; pages          = None
  ; pdf_version    = None
  ; producer       = None
  ; subject        = None
  ; tagged         = None
  ; title          = None
  }

let split_line line =
  try
    let ix = String.index_exn line ':' in
    let key = String.sub line ~pos:0 ~len:ix in
    let value = String.sub line ~pos:(ix + 1)
        ~len:(String.length line - ix - 1) in
    ( key, Caml.String.trim value )
  with _ ->
    ( "", "" )

let yesno s =
  match s with
  | "yes" -> Some true
  | "no"  -> Some false
  | _     -> None

let parse_file_size s =
  try
    let ix = String.index_exn s ' ' in
    let s' = String.sub s ~pos:0 ~len:ix in
    Some (Int64.of_string s')
  with _ ->
    None

let parse_enc s =
  if String.is_prefix s ~prefix:"yes (" then
    try
      let ix = String.index_exn s ')' in
      let s' = String.sub s ~pos:5 ~len:(ix - 5) in
      let xs = String.split s' ~on:' ' in
      let a = ref Encrypted.
                    { print = false
                    ; copy = false
                    ; change = false
                    ; add_notes = false
                    ; algorithm = "" } in
      List.iter xs ~f:begin fun x ->
        let k, v = split_line x in
        try
          match k with
          | "print"     -> a := { !a with print = yesno v |> Option.value_exn }
          | "copy"      -> a := { !a with copy = yesno v |> Option.value_exn }
          | "change"    -> a := { !a with change = yesno v |> Option.value_exn }
          | "addNotes"  -> a := { !a with add_notes = yesno v
                                                      |> Option.value_exn }
          | "algorithm" -> a := { !a with algorithm = v }
          | _ -> ()
        with _ -> ()
      end;
      Some !a
    with _ ->
      None
  else if String.equal s "no" then
    None
  else
    None

let process k v info =
  try
    match k with
    | "Author"         -> { info with author = Some v }
    | "CreationDate"   -> { info with creation_date = Some v }
    | "Creator"        -> { info with creator = Some v }
    | "Encrypted"      -> { info with encrypted = parse_enc v }
    | "File size"      -> { info with file_size = parse_file_size v }
    | "JavaScript"     -> { info with javascript = yesno v }
    | "ModDate"        -> { info with mod_date = Some v }
    | "Optimized"      -> { info with optimized = yesno v }
    | "PDF version"    -> { info with
                            pdf_version = Some (Caml.float_of_string v) }
    | "Page rot"       -> { info with page_rot = Some (Caml.int_of_string v) }
    | "Pages"          -> { info with pages = Some (Caml.int_of_string v) }
    | "Producer"       -> { info with producer = Some v }
    | "Subject"        -> { info with subject = Some v }
    | "Tagged"         -> { info with tagged = yesno v }
    | "Title"          -> { info with title = Some v }
    | _                ->
      Stdio.print_endline k;
      Stdio.print_endline v;
      info
  with _               -> info

let parse_channel ichan =
  let info = ref default in
  Stdio.In_channel.iter_lines ichan ~f:begin fun line ->
    let k, v = split_line line in
    info := process k v !info
  end;
  !info

let of_file ?(password=`None) ~path () =
  let cmdline = Buffer.create 80 in
  Buffer.add_string cmdline "pdfinfo -isodates";
  begin match password with
    | `Owner password ->
      Buffer.add_string cmdline " -opw '";
      Buffer.add_string cmdline password;
        Buffer.add_string cmdline "'";
    | `User password ->
      Buffer.add_string cmdline " -upw '";
      Buffer.add_string cmdline password;
      Buffer.add_string cmdline "'";
    | `None -> ()
  end;
  Buffer.add_string cmdline " '";
  Buffer.add_string cmdline path;
  Buffer.add_string cmdline "'";

  let (ichan, _, echan) as chan =
    Unix.open_process_full
      (Buffer.contents cmdline)
      (Unix.environment ()) in
  let info = parse_channel ichan in
  let err =
    Stdio.In_channel.input_all echan
    |> Caml.String.trim in
  match Unix.close_process_full chan with
  | WEXITED 0   -> Ok info
  | WEXITED _   -> Error err
  | WSIGNALED _ -> Error "SIGNALED"
  | WSTOPPED _  -> Error "STOPPED"
