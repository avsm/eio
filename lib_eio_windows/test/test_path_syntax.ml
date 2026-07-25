module W = Eio_windows.Path_syntax
module P = Eio_unix.Private.Path_syntax

let check_bool name got = Alcotest.(check bool) name true got
let check_str name got expected = Alcotest.(check string) name expected got
let check_split name got expected =
  Alcotest.(check (option (pair string string))) name expected got

let test_is_step () =
  check_bool "relative" (W.is_step "foo\\bar");
  check_bool "empty" (W.is_step "");
  check_bool "drive absolute" (not (W.is_step "C:\\foo"));
  check_bool "drive fwd" (not (W.is_step "C:/foo"));
  check_bool "drive relative" (not (W.is_step "C:foo"));
  check_bool "bare drive" (not (W.is_step "C:"));
  check_bool "rooted" (not (W.is_step "\\foo"));
  check_bool "unc" (not (W.is_step "\\\\srv\\share\\x"));
  check_bool "verbatim" (not (W.is_step "\\\\?\\C:\\x"));
  check_bool "nt" (not (W.is_step "\\??\\C:\\x"));
  check_bool "device" (not (W.is_step "\\\\.\\NUL"));
  check_bool "posix relative" (P.is_step "x/y");
  check_bool "posix absolute" (not (P.is_step "/x"));
  check_bool "posix double root" (not (P.is_step "//srv/share"));
  (* A drive prefix means nothing in POSIX syntax *)
  check_bool "posix drive" (P.is_step "C:\\x")

let test_split () =
  check_split "relative" (W.split "a\\b") (Some ("a", "b"));
  check_split "mixed seps" (W.split "a/b\\c") (Some ("a/b", "c"));
  check_split "trailing seps" (W.split "a\\b\\\\") (Some ("a", "b"));
  check_split "drive" (W.split "C:\\a\\b") (Some ("C:\\a", "b"));
  check_split "drive root child" (W.split "C:\\b") (Some ("C:\\", "b"));
  check_split "drive root" (W.split "C:\\") None;
  check_split "bare drive" (W.split "C:") None;
  check_split "drive relative" (W.split "C:x") (Some ("C:", "x"));
  check_split "rooted" (W.split "\\x") (Some ("\\", "x"));
  check_split "unc" (W.split "\\\\srv\\share\\x") (Some ("\\\\srv\\share\\", "x"));
  check_split "unc root" (W.split "\\\\srv\\share") None;
  (* In verbatim paths, '/' is an ordinary character. *)
  check_split "verbatim slash literal" (W.split "\\\\?\\C:\\a/b")
    (Some ("\\\\?\\C:\\", "a/b"));
  check_split "nt" (W.split "\\??\\C:\\a\\b") (Some ("\\??\\C:\\a", "b"));
  (* The NT prefix is backslash-only: with forward slashes this is just a
     rooted path with ordinary components. *)
  check_split "not nt with fwd seps" (W.split "/??/C:/x") (Some ("/??/C:", "x"));
  check_split "empty" (W.split "") None;
  check_split "posix" (P.split "/a//b/") (Some ("/a", "b"));
  check_split "posix relative" (P.split "bar") (Some ("", "bar"))

let test_join () =
  check_str "rel" (W.join "a" "b") "a/b";
  check_str "empty dir" (W.join "" "b") "b";
  check_str "trailing sep" (W.join "a/" "b") "a/b";
  check_str "drive root" (W.join "C:\\" "b") "C:\\b";
  check_str "bare drive stays drive-relative" (W.join "C:" "x") "C:x";
  check_str "rooted" (W.join "\\" "x") "\\x";
  check_str "unc root" (W.join "\\\\srv\\share\\" "x") "\\\\srv\\share\\x";
  check_str "verbatim" (W.join "\\\\?\\C:\\a" "b") "\\\\?\\C:\\a\\b";
  check_str "verbatim root" (W.join "\\\\?\\C:\\" "b") "\\\\?\\C:\\b";
  check_str "nt" (W.join "\\??\\C:\\a" "b") "\\??\\C:\\a\\b";
  check_str "posix" (P.join "a" "b") "a/b";
  check_str "posix root" (P.join "/" "a") "/a"

(* [join dir base] of a split must refer to the same path as the input.
   For canonical inputs the string is identical; backslash-separated
   non-verbatim components re-join with "/", which Win32 treats the same. *)
let test_round_trip () =
  let rt ?expect name p =
    let expect = Option.value expect ~default:p in
    match W.split p with
    | None -> Alcotest.failf "%s: no split" name
    | Some (d, b) -> check_str name (W.join d b) expect
  in
  rt "drive root child" "C:\\b";
  rt "drive relative" "C:x";
  rt "rooted" "\\x";
  rt "unc" "\\\\srv\\share\\x";
  rt "verbatim" "\\\\?\\C:\\a\\b";
  rt "verbatim slash literal" "\\\\?\\C:\\a/b";
  rt "nt" "\\??\\C:\\a\\b";
  rt "posix-style" "a/b";
  rt "drive abs (equivalent)" "C:\\a\\b" ~expect:"C:\\a/b";
  rt "backslash rel (equivalent)" "a\\b" ~expect:"a/b"

let tests = [
  "is-step", `Quick, test_is_step;
  "split", `Quick, test_split;
  "join", `Quick, test_join;
  "round-trip", `Quick, test_round_trip;
]
