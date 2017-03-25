(** pdfinfo library for OCaml

    A wrapper around the pdfinfo command (for collecting PDF file info).
*)

open Base

(** encrypted flags *)
module Encrypted : sig
  type t =
    { print     : bool
    ; copy      : bool
    ; change    : bool
    ; add_notes : bool
    ; algorithm : string
    }

  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

(** pdfinfo *)
type t =
  { author        : string option
  ; creation_date : string option
  ; creator       : string option
  ; encrypted     : Encrypted.t option
  ; file_size     : int64  option
  ; javascript    : bool   option
  ; mod_date      : string option
  ; optimized     : bool   option
  ; page_rot      : int    option
  ; pages         : int    option
  ; pdf_version   : float  option
  ; producer      : string option
  ; subject       : string option
  ; tagged        : bool   option
  ; title         : string option
  }

val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t

(** read pdfinfo from file at [path] *)
val of_file :
  ?password:[< `None | `Owner of string | `User of string > `None ] ->
  path:string -> unit -> (t, string) Result.t

val parse_channel : Stdio.In_channel.t -> t
