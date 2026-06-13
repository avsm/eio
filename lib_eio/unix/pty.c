/* Pseudoterminal support for Eio_unix.Pty. */

#include "primitives.h" /* Defines _GNU_SOURCEfor posix_openpt. */

#include <string.h>
#include <errno.h>

#ifndef _WIN32
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

/* Returns [(pty_fd, tty_fd, name)], both descriptors close-on-exec. */
CAMLprim value eio_unix_open_pty(value v_unit) {
#ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "open_pty", Nothing);
#else
  CAMLparam1(v_unit);
  CAMLlocal2(v_name, v_ret);
  int pty_fd, tty_fd = -1;

  pty_fd = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (pty_fd < 0)
    caml_uerror("posix_openpt", Nothing);

  if (grantpt(pty_fd) < 0) goto fail;
  if (unlockpt(pty_fd) < 0) goto fail;

  {
    char namebuf[256];
    int err = ptsname_r(pty_fd, namebuf, sizeof namebuf);
    if (err != 0) { errno = err; goto fail; }
    v_name = caml_copy_string(namebuf);
  }

#ifdef TIOCGPTPEER
  tty_fd = ioctl(pty_fd, TIOCGPTPEER, O_RDWR | O_NOCTTY | O_CLOEXEC);
#else
  tty_fd = open(String_val(v_name), O_RDWR | O_NOCTTY | O_CLOEXEC);
#endif
  if (tty_fd < 0) goto fail;

  v_ret = caml_alloc_tuple(3);
  Store_field(v_ret, 0, Val_int(pty_fd));
  Store_field(v_ret, 1, Val_int(tty_fd));
  Store_field(v_ret, 2, v_name);
  CAMLreturn(v_ret);

fail:
  {
    int olde = errno;
    if (tty_fd >= 0) close(tty_fd);
    if (pty_fd >= 0) close(pty_fd);
    errno = olde;
    caml_uerror("open_pty", Nothing);
  }
  CAMLreturn(Val_unit);  /* NOTREACHED */
#endif
}

CAMLprim value eio_unix_get_winsize(value v_fd) {
#ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "get_window_size", Nothing);
#else
  CAMLparam1(v_fd);
  CAMLlocal1(v_ws);
  struct winsize w;
  if (ioctl(Int_val(v_fd), TIOCGWINSZ, &w) < 0)
    caml_uerror("get_window_size", Nothing);
  v_ws = caml_alloc_tuple(4);
  Store_field(v_ws, 0, Val_int(w.ws_row));
  Store_field(v_ws, 1, Val_int(w.ws_col));
  Store_field(v_ws, 2, Val_int(w.ws_xpixel));
  Store_field(v_ws, 3, Val_int(w.ws_ypixel));
  CAMLreturn(v_ws);
#endif
}

CAMLprim value eio_unix_set_winsize(value v_fd, value v_ws) {
#ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "set_window_size", Nothing);
#else
  CAMLparam2(v_fd, v_ws);
  struct winsize w;
  memset(&w, 0, sizeof w);
  w.ws_row    = Int_val(Field(v_ws, 0));
  w.ws_col    = Int_val(Field(v_ws, 1));
  w.ws_xpixel = Int_val(Field(v_ws, 2));
  w.ws_ypixel = Int_val(Field(v_ws, 3));
  if (ioctl(Int_val(v_fd), TIOCSWINSZ, &w) < 0)
    caml_uerror("set_window_size", Nothing);
  CAMLreturn(Val_unit);
#endif
}
