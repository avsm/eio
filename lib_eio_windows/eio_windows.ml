(*
 * Copyright (C) 2023 Thomas Leonard
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Low_level = Low_level
module Pty = Pty

type stdenv = Eio_unix.Stdenv.base

let run main =
  Domain_mgr.run_event_loop (fun () ->
    Eio.Switch.run @@ fun sw ->
    (* Windows cannot probe a handle's blocking mode, so give the standard streams a
       known one: they are synchronous handles, so tag them blocking (routing them to
       the systhread path). [~close_unix:false] keeps the process-global OS handles
       open when the switch finishes. *)
    let std_fd fd = Eio_unix.Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:false fd in
    let stdin = (Flow.of_fd (std_fd Unix.stdin) :> _ Eio_unix.source) in
    let stdout = (Flow.of_fd (std_fd Unix.stdout) :> _ Eio_unix.sink) in
    let stderr = (Flow.of_fd (std_fd Unix.stderr) :> _ Eio_unix.sink) in
    main @@ object (_ : stdenv)
      method stdin = stdin
      method stdout = stdout
      method stderr = stderr
      method debug = Eio.Private.Debug.v
      method clock = Time.clock
      method mono_clock = Time.mono_clock
      method net = Net.v
      method domain_mgr = Domain_mgr.v
      method cwd = ((Fs.cwd, "") :> Eio.Fs.dir_ty Eio.Path.t)
      method fs = ((Fs.fs, "") :> Eio.Fs.dir_ty Eio.Path.t)
      method process_mgr = Process.mgr
      method secure_random = Flow.secure_random
      method backend_id = "windows"
    end) ()
