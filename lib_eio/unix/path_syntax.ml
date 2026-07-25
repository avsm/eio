(* POSIX path syntax *)

let is_step s = not (String.starts_with ~prefix:"/" s)

(* Like [Filename.concat] but always using "/" as the separator. *)
let join a b =
  let l = String.length a in
  if l = 0 || a.[l - 1] = '/' then a ^ b
  else a ^ "/" ^ b

(* "/foo/bar//" -> "/foo/bar"
   "///" -> "/"
   "foo/bar" -> "foo/bar"
 *)
let remove_trailing_slashes s =
  let rec aux i =
    if i <= 1 || s.[i - 1] <> '/' then (
      if i = String.length s then s
      else String.sub s 0 i
    ) else aux (i - 1)
  in
  aux (String.length s)

let split p =
  match remove_trailing_slashes p with
  | "" -> None
  | "/" -> None
  | p ->
    match String.rindex_opt p '/' with
    | None -> Some ("", p)
    | Some idx ->
      let basename = String.sub p (idx + 1) (String.length p - idx - 1) in
      let dirname =
        if idx = 0 then "/"
        else remove_trailing_slashes (String.sub p 0 idx)
      in
      Some (dirname, basename)
