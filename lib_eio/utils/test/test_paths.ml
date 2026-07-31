(* Unit tests for path syntax. *)

module P = Eio_utils.Posix_path
module W = Eio_utils.Nt_path

let check_bool name got = Alcotest.(check bool) name true got
let check_str name got expected = Alcotest.(check string) name expected got
let check_split name got expected =
  Alcotest.(check (option (pair string string))) name expected got

module Posix = struct
  let test_is_step () =
    check_bool "relative" (P.is_step "x/y");
    check_bool "empty" (P.is_step "");
    check_bool "absolute" (not (P.is_step "/x"));
    check_bool "double root" (not (P.is_step "//srv/share"));
    (* A drive prefix means nothing in POSIX syntax. *)
    check_bool "drive" (P.is_step "C:\\x")

  let test_join () =
    check_str "join" (P.join "a" "b") "a/b";
    check_str "join root" (P.join "/" "a") "/a";
    check_str "join empty" (P.join "" "b") "b";
    check_str "join trailing" (P.join "a/" "b") "a/b";
    (* An absolute second argument replaces the directory part. *)
    check_str "join replaces absolute" (P.join "a" "/b") "/b"

  let test_split () =
    check_split "split" (P.split "/a//b/") (Some ("/a", "b"));
    check_split "split relative" (P.split "bar") (Some ("", "bar"));
    check_split "split root child" (P.split "/bar") (Some ("/", "bar"));
    check_split "split root" (P.split "/") None;
    check_split "split slashes" (P.split "///") None;
    check_split "split empty" (P.split "") None

  (* split/join round-trip: [join dir base] refers to the same path. *)
  let test_round_trip () =
    let rt name p =
      match P.split p with
      | None -> Alcotest.failf "%s: no split" name
      | Some (d, b) -> check_str name (P.join d b) p
    in
    rt "abs" "/a/b";
    rt "root child" "/a";
    rt "rel" "a/b";
    rt "single" "bar"

  let tests = [
    "is-step", `Quick, test_is_step;
    "split", `Quick, test_split;
    "join", `Quick, test_join;
    "round-trip", `Quick, test_round_trip;
  ]
end

module Windows = struct
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
    check_bool "device" (not (W.is_step "\\\\.\\NUL"))

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
    check_split "empty" (W.split "") None

  let test_join () =
    check_str "empty" (W.join "a" "") "a\\";
    check_str "abs" (W.join "a" "C:\\b") "C:\\b";
    check_str "dot" (W.join "." "b") "b";
    check_str "rel" (W.join "a" "b") "a\\b";
    check_str "empty dir" (W.join "" "b") "b";
    check_str "trailing sep" (W.join "a/" "b") "a/b";
    check_str "drive root" (W.join "C:\\" "b") "C:\\b";
    check_str "bare drive stays drive-relative" (W.join "C:" "x") "C:x";
    check_str "rooted" (W.join "\\" "x") "\\x";
    check_str "unc root" (W.join "\\\\srv\\share\\" "x") "\\\\srv\\share\\x";
    check_str "verbatim" (W.join "\\\\?\\C:\\a" "b") "\\\\?\\C:\\a\\b";
    check_str "verbatim root" (W.join "\\\\?\\C:\\" "b") "\\\\?\\C:\\b";
    check_str "nt" (W.join "\\??\\C:\\a" "b") "\\??\\C:\\a\\b";
    (* "abs" above covers a drive-absolute second argument; every other form
       that names its own volume or root replaces the directory part too. *)
    check_str "replaces drive fwd" (W.join "a" "C:/b") "C:/b";
    check_str "replaces drive relative" (W.join "a" "C:b") "C:b";
    check_str "replaces bare drive" (W.join "a" "C:") "C:";
    check_str "replaces rooted" (W.join "a" "\\b") "\\b";
    check_str "replaces posix rooted" (W.join "a" "/b") "/b";
    check_str "replaces unc" (W.join "a" "\\\\srv\\share") "\\\\srv\\share";
    check_str "replaces verbatim" (W.join "a" "\\\\?\\C:\\b") "\\\\?\\C:\\b";
    check_str "replaces nt" (W.join "a" "\\??\\C:\\b") "\\??\\C:\\b";
    check_str "replaces device" (W.join "a" "\\\\.\\NUL") "\\\\.\\NUL"

  (* [join dir base] of a split must refer to the same path as the input.
     For canonical inputs the string is identical; forward-slash-separated
     components re-join with a backslash, which Win32 treats the same. *)
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
    rt "drive abs" "C:\\a\\b";
    rt "backslash rel" "a\\b";
    rt "posix-style (equivalent)" "a/b" ~expect:"a\\b"

  let tests = [
    "is-step", `Quick, test_is_step;
    "split", `Quick, test_split;
    "join", `Quick, test_join;
    "round-trip", `Quick, test_round_trip;
  ]
end

let () =
  Alcotest.run "path-syntax" [
    "posix", Posix.tests;
    "windows", Windows.tests;
  ]
