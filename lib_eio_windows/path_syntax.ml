(* Windows path syntax. *)

let is_drive_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
let is_sep c = c = '\\' || c = '/'

(* A recognizer matches at position [i] of [s] and returns the position
   one past the match, or [None] if it doesn't match. *)
type recognizer = string -> int -> int option

let charp f : recognizer =
  fun s i -> if i < String.length s && f s.[i] then Some (i + 1) else None

let ( *> ) (p : recognizer) (q : recognizer) : recognizer =
  fun s i -> Option.bind (p s i) (q s)

let ( <|> ) (p : recognizer) (q : recognizer) : recognizer =
  fun s i -> match p s i with Some _ as r -> r | None -> q s i

let opt p : recognizer = p <|> (fun _ i -> Some i)

let sep = charp is_sep
let bslash = charp (Char.equal '\\')
let qmark = charp (Char.equal '?')

(* The (possibly empty) run of non-separator characters at [i]. *)
let component : recognizer = fun s i ->
  let n = String.length s in
  let rec scan j = if j < n && not (is_sep s.[j]) then scan (j + 1) else j in
  Some (scan i)

(* A component whose text satisfies [f]. *)
let component_is f : recognizer = fun s i ->
  match component s i with
  | Some e when f (String.sub s i (e - i)) -> Some e
  | _ -> None

(* The component after this one. [opt sep] rather than [sep] so that a
   malformed prefix is cut at whatever exists. *)
let next = opt sep *> component

let drive = charp is_drive_letter *> charp (Char.equal ':')
let q_or_dot = component_is (fun c -> c = "?" || c = ".")
let unc_kw = component_is (fun c -> String.uppercase_ascii c = "UNC")

(* The "C:", "\\server\share", "\\.\device", "\\?\..." or "\??\..."
   volume prefix of a path. *)
let volume_prefix =
  drive                                                        (* C: *)
  <|> (sep *> sep
       *> ((q_or_dot *> ((opt sep *> unc_kw *> next *> next)   (* \\?\UNC\server\share *)
                         <|> next))                            (* \\?\C: or \\.\device *)
           <|> (component *> next)))                           (* \\server\share *)
  <|> (bslash *> qmark *> qmark *> component *> next)          (* \??\C: - the NT object-manager form (backslash only) *)

let volume_end s = Option.value (volume_prefix s 0) ~default:0
let volume s = String.sub s 0 (volume_end s)

(* Win32 does no normalization in the verbatim and NT namespaces. *)
let verbatim_prefix = bslash *> (bslash <|> qmark) *> qmark
let verbatim s = Option.is_some (verbatim_prefix s 0)

let is_step s =
  volume_end s = 0 && (s = "" || not (is_sep s.[0]))

let split p =
  let vend = volume_end p in
  let sep_at = if verbatim p then Char.equal '\\' else is_sep in
  let sep_at i = sep_at p.[i] in
  (* Trailing separators are ignored; one is kept for a bare root. *)
  let rec trim i = if i > vend + 1 && sep_at (i - 1) then trim (i - 1) else i in
  let stop = trim (String.length p) in
  if stop <= vend || (stop = vend + 1 && sep_at vend) then None
  else
    let rec rsep i = if i < vend then None else if sep_at i then Some i else rsep (i - 1) in
    match rsep (stop - 1) with
    | None -> Some (volume p, String.sub p vend (stop - vend))
    | Some idx ->
      let basename = String.sub p (idx + 1) (stop - idx - 1) in
      let dirname =
        (* keep the root separator itself *)
        String.sub p 0 (if idx = vend then vend + 1 else trim idx)
      in
      Some (dirname, basename)

let join a b =
  let l = String.length a in
  if l = 0 then b
  else if (if verbatim a then a.[l - 1] = '\\' else is_sep a.[l - 1]) then a ^ b
  else if l = 2 && a.[1] = ':' && is_drive_letter a.[0] then
    a ^ b   (* a bare drive is drive-relative: adding a separator would change its meaning *)
  else if verbatim a then a ^ "\\" ^ b
  else a ^ "/" ^ b
