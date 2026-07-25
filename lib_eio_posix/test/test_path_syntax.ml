(* Unit tests for POSIX path syntax. *)

module P = Eio_unix.Private.Path_syntax

let failures = ref 0

let check_bool name got =
  if not got then (incr failures; Printf.printf "FAIL %s\n" name)

let check_str name got expected =
  if got <> expected then begin
    incr failures;
    Printf.printf "FAIL %s: got %S, expected %S\n" name got expected
  end

let check_split name got expected =
  let pp = function None -> "None" | Some (a, b) -> Printf.sprintf "Some (%S, %S)" a b in
  if got <> expected then begin
    incr failures;
    Printf.printf "FAIL %s: got %s, expected %s\n" name (pp got) (pp expected)
  end

(* is_step *)
let () =
  check_bool "relative" (P.is_step "x/y");
  check_bool "empty" (P.is_step "");
  check_bool "absolute" (not (P.is_step "/x"));
  check_bool "double root" (not (P.is_step "//srv/share"));
  (* A drive prefix means nothing in POSIX syntax. *)
  check_bool "drive" (P.is_step "C:\\x")

(* join *)
let () =
  check_str "join" (P.join "a" "b") "a/b";
  check_str "join root" (P.join "/" "a") "/a";
  check_str "join empty" (P.join "" "b") "b";
  check_str "join trailing" (P.join "a/" "b") "a/b"

(* split *)
let () =
  check_split "split" (P.split "/a//b/") (Some ("/a", "b"));
  check_split "split relative" (P.split "bar") (Some ("", "bar"));
  check_split "split root child" (P.split "/bar") (Some ("/", "bar"));
  check_split "split root" (P.split "/") None;
  check_split "split slashes" (P.split "///") None;
  check_split "split empty" (P.split "") None

(* split/join round-trip: [join dir base] refers to the same path. *)
let () =
  let rt name p =
    match P.split p with
    | None -> incr failures; Printf.printf "FAIL rt %s: no split\n" name
    | Some (d, b) -> check_str ("rt " ^ name) (P.join d b) p
  in
  rt "abs" "/a/b";
  rt "root child" "/a";
  rt "rel" "a/b";
  rt "single" "bar"

let () =
  if !failures = 0 then print_endline "path_syntax: all tests passed"
  else (Printf.printf "path_syntax: %d failures\n" !failures; exit 1)
