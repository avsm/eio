#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <wchar.h>
#include <stdlib.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#include <assert.h>
#include <ntstatus.h>
#include <bcrypt.h>
#include <winternl.h>
#include <ntdef.h>

typedef ULONG (__stdcall *pNtCreateFile)(
   PHANDLE FileHandle,
   ULONG DesiredAccess,
   PVOID ObjectAttributes,
   PVOID IoStatusBlock,
   PLARGE_INTEGER AllocationSize,
   ULONG FileAttributes,
   ULONG ShareAccess,
   ULONG CreateDisposition,
   ULONG CreateOptions,
   PVOID EaBuffer,
   ULONG EaLength
 );

typedef NTSTATUS (__stdcall *pNtSetInformationFile)(
   HANDLE FileHandle,
   PIO_STATUS_BLOCK IoStatusBlock,
   PVOID FileInformation,
   ULONG Length,
   ULONG FileInformationClass
 );

/* FILE_INFORMATION_CLASS value for a rename via NtSetInformationFile. */
#define Eio_FileRenameInformation 10

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/osdeps.h>

/* We need <caml/unixsupport.h> above for [Handle_val] / [struct filedescr] and
   the [Nothing]/[uerror] macros, so (unlike eio_unix_stubs.c, which uses almost
   nothing from it and can drop it entirely) we keep the include here. But it
   declares these functions as plain [extern], which on Windows leaves flexdll to
   patch direct REL32 relocations when loading the stubs; those overflow when the
   OCaml runtime DLL is mapped more than 2GB away ("cannot relocate caml_uerror
   ... target is too far"). Redeclaring them CAMLextern (i.e. __declspec(dllimport))
   after the header overrides the linkage so the references go through the import
   table instead. Tested with the mingw-w64/flexdll toolchain. */
CAMLextern void caml_win32_maperr(DWORD errcode);
CAMLextern value caml_win32_alloc_handle(HANDLE);
CAMLnoret CAMLextern void caml_unix_error (int errcode, const char * cmdname, value arg);
CAMLnoret CAMLextern void caml_uerror (const char * cmdname, value arg);
CAMLextern void caml_unix_check_path(value path, const char * cmdname);

#ifdef ARCH_SIXTYFOUR
#define Int63_val(v) Long_val(v)
#else
#define Int63_val(v) (Int64_val(v)) >> 1
#endif

/* Whether the current process has a console attached. */
static int eio_has_console(void)
{
  return GetConsoleWindow() != NULL;
}

/* ntdll exports, resolved once (a benign race just re-stores the same address). */
static pNtCreateFile eio_NtCreateFile(void)
{
  static pNtCreateFile fn = NULL;
  if (!fn) fn = (pNtCreateFile)GetProcAddress(GetModuleHandle("ntdll.dll"), "NtCreateFile");
  return fn;
}

static pNtSetInformationFile eio_NtSetInformationFile(void)
{
  static pNtSetInformationFile fn = NULL;
  if (!fn) fn = (pNtSetInformationFile)GetProcAddress(GetModuleHandle("ntdll.dll"), "NtSetInformationFile");
  return fn;
}

static void caml_stat_free_preserving_errno(void *ptr)
{
  int saved = errno;
  caml_stat_free(ptr);
  errno = saved;
}

CAMLprim value caml_eio_windows_getrandom(value v_ba, value v_off, value v_len)
{
  CAMLparam1(v_ba);
  NTSTATUS ret;
  ssize_t off = (ssize_t)Long_val(v_off);
  ssize_t len = (ssize_t)Long_val(v_len);
  /* Single CNG call: it doesn't set errno, so there's nothing to retry on. */
  void *buf = (uint8_t *)Caml_ba_data_val(v_ba) + off;
  caml_enter_blocking_section();
  ret = BCryptGenRandom(NULL, buf, (ULONG)len, BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  caml_leave_blocking_section();
  if (ret != STATUS_SUCCESS) {
    caml_win32_maperr(RtlNtStatusToDosError(ret));
    uerror("getrandom", Nothing);
  }
  CAMLreturn(Val_long(len));
}

/* A [Cstruct.t] is a record { buffer; off; len }. */
#define Cstruct_ptr(v) ((char *)Caml_ba_data_val(Field((v), 0)) + Long_val(Field((v), 1)))
#define Cstruct_len(v) ((DWORD)Long_val(Field((v), 2)))

/* Positioned scatter read. Reads each buffer in turn at successive offsets,
   stopping at the first short read (or EOF). Returns the total bytes read. */
CAMLprim value caml_eio_windows_preadv(value v_fd, value v_bufs, value v_offset)
{
  CAMLparam3(v_fd, v_bufs, v_offset);
  HANDLE h = Handle_val(v_fd);
  mlsize_t n_bufs = Wosize_val(v_bufs);
  ULONG64 offset = (ULONG64)Int63_val(v_offset);
  ULONG64 total = 0;
  BOOL ok = TRUE;
  DWORD err = 0;
  for (mlsize_t i = 0; i < n_bufs; i++) {
    value v_buf = Field(v_bufs, i);
    char *ptr = Cstruct_ptr(v_buf);
    DWORD len = Cstruct_len(v_buf);
    DWORD got = 0;
    OVERLAPPED ov;
    memset(&ov, 0, sizeof(ov));
    ov.Offset = (DWORD)offset;
    ov.OffsetHigh = (DWORD)(offset >> 32);
    caml_enter_blocking_section();
    ok = ReadFile(h, ptr, len, &got, &ov);
    if (!ok) err = GetLastError();
    caml_leave_blocking_section();
    if (!ok) {
      /* Reading at or past end-of-file reports EOF rather than 0 bytes. */
      if (err == ERROR_HANDLE_EOF) { ok = TRUE; }
      break;
    }
    total += got;
    offset += got;
    if (got < len) break; /* short read / EOF */
  }
  if (!ok) {
    caml_win32_maperr(err);
    uerror("preadv", Nothing);
  }
  CAMLreturn(Val_long(total));
}

/* Positioned gather write. Writes every buffer fully at successive offsets.
   Returns the total bytes written. */
CAMLprim value caml_eio_windows_pwritev(value v_fd, value v_bufs, value v_offset)
{
  CAMLparam3(v_fd, v_bufs, v_offset);
  HANDLE h = Handle_val(v_fd);
  mlsize_t n_bufs = Wosize_val(v_bufs);
  ULONG64 offset = (ULONG64)Int63_val(v_offset);
  ULONG64 total = 0;
  BOOL ok = TRUE;
  DWORD err = 0;
  for (mlsize_t i = 0; i < n_bufs && ok; i++) {
    value v_buf = Field(v_bufs, i);
    char *ptr = Cstruct_ptr(v_buf);
    DWORD len = Cstruct_len(v_buf);
    DWORD done = 0;
    while (done < len) {
      DWORD put = 0;
      OVERLAPPED ov;
      memset(&ov, 0, sizeof(ov));
      ov.Offset = (DWORD)offset;
      ov.OffsetHigh = (DWORD)(offset >> 32);
      caml_enter_blocking_section();
      ok = WriteFile(h, ptr + done, len - done, &put, &ov);
      if (!ok) err = GetLastError();
      caml_leave_blocking_section();
      if (!ok || put == 0) break;
      total += put;
      offset += put;
      done += put;
    }
  }
  if (!ok) {
    caml_win32_maperr(err);
    uerror("pwritev", Nothing);
  }
  CAMLreturn(Val_long(total));
}

// File-system operations

/* Convert a FILETIME (100ns ticks since 1601-01-01) to Unix epoch seconds as a
   double. 116444736000000000 is the number of 100ns ticks between the 1601 and
   1970 epochs. */
static double eio_filetime_to_unix(FILETIME ft)
{
  ULONGLONG t = ((ULONGLONG)ft.dwHighDateTime << 32) | ft.dwLowDateTime;
  return ((double)t - 116444736000000000.0) / 1e7;
}

/* Native fstat by handle. Fills an [Eio.File.Stat]-shaped tuple straight from
   GetFileInformationByHandle plus the FileStandardInfo/FileStorageInfo classes,
   giving the real volume/index device and inode ids, 100ns-resolution times, a
   link count and allocation-based block usage that OCaml's Unix stat truncates
   (it reports second-resolution times, weak ino/dev and no blocks/blksize).

   The tuple order is
     (kind, dev, ino, nlink, size, perm, atime, mtime, ctime, blksize, blocks)
   where [kind] is 0=regular, 1=directory, 2=symlink and [blocks] is in 512-byte
   units. Returns [None] for a non-disk handle (socket, pipe, console): those
   have no by-handle file information, so the caller falls back to
   [Unix.LargeFile.fstat]. Intended to run on the systhread pool
   ([in_worker_thread]). */
CAMLprim value caml_eio_windows_fstat(value v_fd)
{
  CAMLparam1(v_fd);
  CAMLlocal2(v_res, v_some);
  HANDLE h = Handle_val(v_fd);
  BY_HANDLE_FILE_INFORMATION bhfi;
  FILE_STANDARD_INFO std;
  FILE_STORAGE_INFO storage;
  BOOL ok_info, ok_std, ok_storage;
  DWORD ftype, err = 0;
  int is_dir, is_symlink, kind, perm;
  ULONGLONG ino, size, alloc;
  ULONG blksize;
  ULONG nlink;
  double atime, mtime, ctime;

  caml_enter_blocking_section();
  ftype = GetFileType(h);
  if (ftype != FILE_TYPE_DISK) {
    caml_leave_blocking_section();
    CAMLreturn(Val_int(0)); /* None: caller falls back to Unix.LargeFile.fstat. */
  }
  ok_info = GetFileInformationByHandle(h, &bhfi);
  if (!ok_info) err = GetLastError();
  ok_std = GetFileInformationByHandleEx(h, FileStandardInfo, &std, sizeof(std));
  ok_storage = GetFileInformationByHandleEx(h, FileStorageInfo, &storage, sizeof(storage));
  caml_leave_blocking_section();

  if (!ok_info) {
    caml_win32_maperr(err);
    uerror("fstat", Nothing);
  }

  is_dir = (bhfi.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
  is_symlink = (bhfi.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0;
  kind = is_symlink ? 2 : (is_dir ? 1 : 0);

  ino = ((ULONGLONG)bhfi.nFileIndexHigh << 32) | bhfi.nFileIndexLow;
  size = ((ULONGLONG)bhfi.nFileSizeHigh << 32) | bhfi.nFileSizeLow;
  nlink = bhfi.nNumberOfLinks;
  atime = eio_filetime_to_unix(bhfi.ftLastAccessTime);
  mtime = eio_filetime_to_unix(bhfi.ftLastWriteTime);
  ctime = eio_filetime_to_unix(bhfi.ftCreationTime);

  /* Permissions mirror what the CRT (and thus OCaml's Unix.stat) reports on
     Windows: read everywhere, write unless the read-only attribute is set, and
     the execute bit for directories. */
  perm = (bhfi.dwFileAttributes & FILE_ATTRIBUTE_READONLY) ? 0444 : 0666;
  if (is_dir) perm |= 0111;

  /* Allocation size / 512 gives POSIX-style st_blocks; fall back to the plain
     size if the FileStandardInfo class is somehow unavailable. */
  if (ok_std) {
    alloc = (ULONGLONG)std.AllocationSize.QuadPart;
    nlink = std.NumberOfLinks; /* prefer the by-class link count */
  } else {
    alloc = size;
  }

  /* Preferred I/O block size: the volume's physical sector size for performance,
     or a conventional 4096 when the storage-info class isn't supported. */
  if (ok_storage && storage.PhysicalBytesPerSectorForPerformance > 0)
    blksize = storage.PhysicalBytesPerSectorForPerformance;
  else
    blksize = 4096;

  v_res = caml_alloc_tuple(11);
  Store_field(v_res, 0, Val_int(kind));
  Store_field(v_res, 1, caml_copy_int64((int64_t)bhfi.dwVolumeSerialNumber));
  Store_field(v_res, 2, caml_copy_int64((int64_t)ino));
  Store_field(v_res, 3, caml_copy_int64((int64_t)nlink));
  Store_field(v_res, 4, caml_copy_int64((int64_t)size));
  Store_field(v_res, 5, Val_int(perm));
  Store_field(v_res, 6, caml_copy_double(atime));
  Store_field(v_res, 7, caml_copy_double(mtime));
  Store_field(v_res, 8, caml_copy_double(ctime));
  Store_field(v_res, 9, caml_copy_int64((int64_t)blksize));
  Store_field(v_res, 10, caml_copy_int64((int64_t)(alloc / 512)));

  v_some = caml_alloc(1, 0); /* Some v_res */
  Store_field(v_some, 0, v_res);
  CAMLreturn(v_some);
}

// No follow
void no_follow(HANDLE h) {
  BY_HANDLE_FILE_INFORMATION b;

  if (!GetFileInformationByHandle(h, &b)) {
    caml_win32_maperr(GetLastError());
    uerror("nofollow", Nothing);
  }

  if (b.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) {
    CloseHandle(h);
    caml_unix_error(ELOOP, "nofollow", Nothing);
  }
}

// We recreate an openat like function using NtCreateFile
CAMLprim value caml_eio_windows_openat(value v_dirfd, value v_nofollow, value v_pathname, value v_desired_access, value v_create_disposition, value v_create_options)
{
  CAMLparam2(v_dirfd, v_pathname);
  HANDLE h, dir;
  OBJECT_ATTRIBUTES obj_attr;
  IO_STATUS_BLOCK io_status;
  wchar_t *pathname;
  UNICODE_STRING relative;
  NTSTATUS r;

  // Not sure what the overhead of this is, but it allows us to have low-level control
  // over file creation. In particular, we can specify the HANDLE to the parent directory
  // of a relative path a la openat.
  pNtCreateFile NtCreatefile = eio_NtCreateFile();
  caml_unix_check_path(v_pathname, "openat");
  pathname = caml_stat_strdup_to_utf16(String_val(v_pathname));
  RtlInitUnicodeString(&relative, pathname);

  // If NULL the filepath has to be absolute
  if (Is_some(v_dirfd)) {
    dir = Handle_val(Field(v_dirfd, 0));
  } else {
    dir = NULL;
  }

  // Initialise object attributes, passing in the root directory FD
  InitializeObjectAttributes(
    &obj_attr,
    &relative,
    OBJ_CASE_INSENSITIVE, // TODO: Double-check what flags need to be passed at this point.
    dir,
    NULL
  );

  // Create the file
  r = NtCreatefile(
    &h,
    Int_val(v_desired_access) | FILE_READ_ATTRIBUTES,
    &obj_attr,
    &io_status,
    0, // Allocation size
    FILE_ATTRIBUTE_NORMAL, // TODO: Could check flags to see if we can do READONLY here a la OCaml
    (FILE_SHARE_READ | FILE_SHARE_WRITE),
    Int_val(v_create_disposition),
    (
       FILE_SYNCHRONOUS_IO_NONALERT
      | FILE_OPEN_FOR_BACKUP_INTENT
      | Int_val(v_create_options)
      /* FILE_OPEN_REPARSE_POINT opens the reparse point itself rather than
         following it; it must be added to the caller's create options, not
         substituted for them, or constraints like FILE_DIRECTORY_FILE are lost. */
      | (Bool_val(v_nofollow) ? FILE_OPEN_REPARSE_POINT : 0)),
    NULL, // Extended attribute buffer
    0     // Extended attribute buffer length
  );

  // Free the allocated pathname
  caml_stat_free(pathname);

  // Check [r], not [h]: NtCreateFile needn't write [h] on failure.
  if (!NT_SUCCESS(r)) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    uerror("openat", v_pathname);
  }

  // No follow check -- Windows doesn't actually have that ability
  // so we have to do it after the fact. This will raise if a symbolic
  // link is encountered and will close the handle.
  if (Bool_val(v_nofollow)) {
    no_follow(h);
  }
  
  CAMLreturn(caml_win32_alloc_handle(h));
}

value caml_eio_windows_openat_bytes(value* values, int argc) {
    return caml_eio_windows_openat(values[0], values[1], values[2], values[3], values[4], values[5]);
}

/* Seek a file handle natively. The backend's files are raw HANDLEs from
   [caml_win32_alloc_handle] (see [caml_eio_windows_openat]); OCaml's
   [Unix.LargeFile.lseek] returns garbage on them, so we call [SetFilePointerEx]
   directly. [v_cmd] is 0/1/2 for SEEK_SET/CUR/END. */
CAMLprim value caml_eio_windows_lseek(value v_fd, value v_ofs, value v_cmd)
{
  CAMLparam3(v_fd, v_ofs, v_cmd);
  static const DWORD whence[] = { FILE_BEGIN, FILE_CURRENT, FILE_END };
  LARGE_INTEGER dist, pos;
  dist.QuadPart = Int64_val(v_ofs);
  if (!SetFilePointerEx(Handle_val(v_fd), dist, &pos, whence[Int_val(v_cmd)])) {
    caml_win32_maperr(GetLastError());
    caml_uerror("lseek", Nothing);
  }
  CAMLreturn(caml_copy_int64(pos.QuadPart));
}

CAMLprim value caml_eio_windows_unlinkat(value v_dirfd, value v_pathname, value v_dir)
{
  CAMLparam2(v_dirfd, v_pathname);
  HANDLE h, dir;
  OBJECT_ATTRIBUTES obj_attr;
  IO_STATUS_BLOCK io_status;
  wchar_t *pathname;
  UNICODE_STRING relative;
  NTSTATUS r;

  // Not sure what the overhead of this is, but it allows us to have low-level control
  // over file creation. In particular, we can specify the HANDLE to the parent directory
  // of a relative path a la openat.
  pNtCreateFile NtCreatefile = eio_NtCreateFile();
  caml_unix_check_path(v_pathname, "openat");
  pathname = caml_stat_strdup_to_utf16(String_val(v_pathname));
  RtlInitUnicodeString(&relative, pathname);

  // If NULL the filepath has to be absolute
  if (Is_some(v_dirfd)) {
    dir = Handle_val(Field(v_dirfd, 0));
  } else {
    dir = NULL;
  }

  // Initialise object attributes, passing in the root directory FD
  InitializeObjectAttributes(
    &obj_attr,
    &relative,
    OBJ_CASE_INSENSITIVE, // TODO: Double-check what flags need to be passed at this point.
    dir,
    NULL
  );

  // Create the file
  r = NtCreatefile(
    &h,
    (SYNCHRONIZE | DELETE),
    &obj_attr,
    &io_status,
    0, // Allocation size
    FILE_ATTRIBUTE_NORMAL, // TODO: Could check flags to see if we can do READONLY here a la OCaml
    (FILE_SHARE_DELETE),
    FILE_OPEN,
    /* FILE_OPEN_REPARSE_POINT so that unlinking a symlink removes the link
       itself rather than following it and deleting the target (matching the
       POSIX behaviour, which never follows the final symlink). */
    ((Bool_val(v_dir) ? FILE_DIRECTORY_FILE : FILE_NON_DIRECTORY_FILE)
       | FILE_SYNCHRONOUS_IO_NONALERT | FILE_DELETE_ON_CLOSE | FILE_OPEN_REPARSE_POINT),
    NULL, // Extended attribute buffer
    0     // Extended attribute buffer length
  );

  // Free the allocated pathname
  caml_stat_free(pathname);

  // Check [r], not [h]: NtCreateFile needn't write [h] on failure.
  if (!NT_SUCCESS(r)) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    uerror("unlinkat", v_pathname);
  }

  // Closing the handle triggers the delete (FILE_DELETE_ON_CLOSE).
  CloseHandle(h);

  CAMLreturn(Val_unit);
}

/* Open a handle for [pathname] relative to the optional directory [v_dirfd],
   mirroring the [openat] stub above. Returns INVALID_HANDLE_VALUE and sets the
   OCaml error code (via [caml_win32_maperr]) on failure. */
static HANDLE eio_open_handle_at(value v_dirfd, value v_pathname, ACCESS_MASK access,
                                 ULONG share, ULONG disposition, ULONG options,
                                 const char *opname)
{
  HANDLE h, dir;
  OBJECT_ATTRIBUTES obj_attr;
  IO_STATUS_BLOCK io_status;
  wchar_t *pathname;
  UNICODE_STRING relative;
  NTSTATUS r;
  pNtCreateFile NtCreatefile = eio_NtCreateFile();

  caml_unix_check_path(v_pathname, opname);
  pathname = caml_stat_strdup_to_utf16(String_val(v_pathname));
  RtlInitUnicodeString(&relative, pathname);

  dir = Is_some(v_dirfd) ? Handle_val(Field(v_dirfd, 0)) : NULL;

  InitializeObjectAttributes(&obj_attr, &relative, OBJ_CASE_INSENSITIVE, dir, NULL);

  r = NtCreatefile(&h, access, &obj_attr, &io_status,
                   0, FILE_ATTRIBUTE_NORMAL, share, disposition,
                   options | FILE_SYNCHRONOUS_IO_NONALERT | FILE_OPEN_FOR_BACKUP_INTENT,
                   NULL, 0);

  caml_stat_free(pathname);

  if (!NT_SUCCESS(r)) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    return INVALID_HANDLE_VALUE;
  }
  return h;
}

/* Resolve a directory handle to its absolute path and append [leaf_utf8],
   yielding a newly-allocated "<dir>\<leaf>" UTF-16 string (caller frees).
   Returns NULL and sets [*err] on failure. */
static wchar_t *eio_path_at(HANDLE dir, const char *leaf_utf8, DWORD *err)
{
  wchar_t *leaf = caml_stat_strdup_to_utf16(leaf_utf8);
  DWORD dir_len = GetFinalPathNameByHandleW(dir, NULL, 0, FILE_NAME_NORMALIZED);
  if (dir_len == 0) {
    *err = GetLastError();
    caml_stat_free(leaf);
    return NULL;
  }
  /* [dir_len] includes the NUL; add room for a separator and the leaf. */
  size_t leaf_len = wcslen(leaf);
  wchar_t *full = caml_stat_alloc((dir_len + 1 + leaf_len + 1) * sizeof(wchar_t));
  GetFinalPathNameByHandleW(dir, full, dir_len, FILE_NAME_NORMALIZED);
  size_t cur = wcslen(full);
  full[cur++] = L'\\';
  wcscpy(full + cur, leaf);
  caml_stat_free(leaf);
  return full;
}

/* renameat: open the source with an [openat]-style relative open, then rename it
   via [FILE_RENAME_INFO] — relative to the destination directory handle when
   sandboxed (using [NtSetInformationFile]; see below), or to an absolute path
   otherwise. */
CAMLprim value caml_eio_windows_renameat(value v_old_fd, value v_old_path, value v_new_fd, value v_new_path)
{
  CAMLparam4(v_old_fd, v_old_path, v_new_fd, v_new_path);
  HANDLE src;
  HANDLE new_root;
  wchar_t *new_name;
  size_t name_bytes;
  size_t info_size;
  FILE_RENAME_INFO *info;
  BOOL ok;
  DWORD err = 0;

  src = eio_open_handle_at(v_old_fd, v_old_path,
                           DELETE | SYNCHRONIZE,
                           FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                           FILE_OPEN, 0, "renameat");
  if (src == INVALID_HANDLE_VALUE)
    uerror("renameat", v_old_path);

  caml_unix_check_path(v_new_path, "renameat");
  /* When sandboxed, [v_new_path] is a leaf renamed relative to the destination
     directory handle ([RootDirectory]); unconfined, it is the destination path
     and the root is NULL. */
  new_root = Is_some(v_new_fd) ? Handle_val(Field(v_new_fd, 0)) : NULL;
  new_name = caml_stat_strdup_to_utf16(String_val(v_new_path));
  name_bytes = wcslen(new_name) * sizeof(wchar_t);
  info_size = sizeof(FILE_RENAME_INFO) + name_bytes;
  info = caml_stat_alloc(info_size);
  memset(info, 0, sizeof(FILE_RENAME_INFO));
  info->ReplaceIfExists = TRUE;
  info->RootDirectory = new_root;
  info->FileNameLength = (DWORD)name_bytes;
  memcpy(info->FileName, new_name, name_bytes);
  caml_stat_free(new_name);

  if (new_root) {
    /* A handle-relative rename must use the NT call directly:
       [SetFileInformationByHandle] rejects a non-NULL [RootDirectory] with
       ERROR_INVALID_PARAMETER. */
    pNtSetInformationFile NtSetInformationFile_ = eio_NtSetInformationFile();
    IO_STATUS_BLOCK rename_io;
    NTSTATUS st;
    caml_enter_blocking_section();
    st = NtSetInformationFile_(src, &rename_io, info, (ULONG)info_size, Eio_FileRenameInformation);
    CloseHandle(src);
    caml_leave_blocking_section();
    ok = NT_SUCCESS(st);
    if (!ok) err = RtlNtStatusToDosError(st);
  } else {
    caml_enter_blocking_section();
    ok = SetFileInformationByHandle(src, FileRenameInfo, info, (DWORD)info_size);
    if (!ok) err = GetLastError();
    CloseHandle(src);
    caml_leave_blocking_section();
  }

  caml_stat_free_preserving_errno(info);

  if (!ok) {
    caml_win32_maperr(err);
    uerror("renameat", v_new_path);
  }

  CAMLreturn(Val_unit);
}

/* Resolve a directory handle and a leaf name to an absolute path. With no
   directory handle the leaf is already usable and is returned unchanged. */
CAMLprim value caml_eio_windows_path_at(value v_dirfd, value v_leaf)
{
  CAMLparam2(v_dirfd, v_leaf);
  CAMLlocal1(v_result);
  DWORD err = 0;
  wchar_t *full;

  caml_unix_check_path(v_leaf, "path_at");

  if (!Is_some(v_dirfd))
    CAMLreturn(v_leaf);

  full = eio_path_at(Handle_val(Field(v_dirfd, 0)), String_val(v_leaf), &err);
  if (full == NULL) {
    caml_win32_maperr(err);
    uerror("path_at", v_leaf);
  }

  v_result = caml_copy_string_of_utf16(full);
  caml_stat_free(full);
  CAMLreturn(v_result);
}

/* Single-pass directory enumeration returning each entry's name and kind
   together, from a directory HANDLE opened by the [openat] stub.

   [GetFileInformationByHandleEx] with [FileIdExtdDirectoryInfo] yields, per
   entry, the file attributes plus an explicit reparse tag, so a symlink can be
   told apart from other reparse points without a follow-up [stat] on each entry.
   That avoids both the N-per-directory extra syscalls and the TOCTOU race where
   an entry statted after the listing has since been removed (and would then be
   reported [`Unknown]). Working from the handle also keeps the enumeration
   confined to exactly that directory, with no path re-resolution — matching the
   sandbox model.

   Returns a list of [(kind, name)] pairs (order unspecified; the caller sorts
   when it needs to). [.] and [..] are skipped. */
CAMLprim value caml_eio_windows_readdir(value v_fd)
{
  CAMLparam1(v_fd);
  CAMLlocal3(v_list, v_pair, v_name);
  HANDLE h = Handle_val(v_fd);
  /* A generous batch so most directories are read in one or two syscalls; a
     single long name (up to ~32K wchars) still fits comfortably. */
  DWORD bufsize = 65536;
  void *buf = caml_stat_alloc(bufsize);
  BOOL ok;
  DWORD err = 0;
  /* Polymorphic-variant tags are immediate ints, so they need no GC root. */
  value v_dir = caml_hash_variant("Directory");
  value v_reg = caml_hash_variant("Regular_file");
  value v_lnk = caml_hash_variant("Symbolic_link");
  value v_unknown = caml_hash_variant("Unknown");

  v_list = Val_emptylist;

  for (;;) {
    caml_enter_blocking_section();
    ok = GetFileInformationByHandleEx(h, FileIdExtdDirectoryInfo, buf, bufsize);
    if (!ok) err = GetLastError();
    caml_leave_blocking_section();

    if (!ok) {
      /* The kernel signals "end of listing" with ERROR_NO_MORE_FILES. */
      if (err == ERROR_NO_MORE_FILES) break;
      caml_stat_free(buf);
      caml_win32_maperr(err);
      uerror("readdir", Nothing);
    }

    FILE_ID_EXTD_DIR_INFO *info = (FILE_ID_EXTD_DIR_INFO *)buf;
    for (;;) {
      /* [FileNameLength] is in bytes and [FileName] is not NUL-terminated. */
      size_t name_wchars = info->FileNameLength / sizeof(WCHAR);
      int is_dot =
        (name_wchars == 1 && info->FileName[0] == L'.') ||
        (name_wchars == 2 && info->FileName[0] == L'.' && info->FileName[1] == L'.');
      if (!is_dot) {
        value v_kind;
        /* Copy the name out and terminate it before converting: the runtime's
           UTF-16 -> UTF-8 helper (as used by [Unix.readdir]) expects a
           NUL-terminated string, and we must not poke into the shared buffer. */
        wchar_t *name = caml_stat_alloc((name_wchars + 1) * sizeof(wchar_t));
        memcpy(name, info->FileName, name_wchars * sizeof(wchar_t));
        name[name_wchars] = L'\0';
        v_name = caml_copy_string_of_utf16(name);
        caml_stat_free(name);

        if (info->FileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) {
          /* A reparse point is a symlink only for the symlink tag; mount points
             and other reparse kinds are reported as [`Unknown]. */
          v_kind = (info->ReparsePointTag == IO_REPARSE_TAG_SYMLINK) ? v_lnk : v_unknown;
        } else if (info->FileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
          v_kind = v_dir;
        } else {
          v_kind = v_reg;
        }

        v_pair = caml_alloc_tuple(2);
        Store_field(v_pair, 0, v_kind);
        Store_field(v_pair, 1, v_name);

        v_name = caml_alloc(2, Tag_cons); /* reuse [v_name] as the cons cell */
        Store_field(v_name, 0, v_pair);
        Store_field(v_name, 1, v_list);
        v_list = v_name;
      }
      /* [NextEntryOffset] of 0 marks the last entry in this batch. */
      if (info->NextEntryOffset == 0) break;
      info = (FILE_ID_EXTD_DIR_INFO *)((char *)info + info->NextEntryOffset);
    }
  }

  caml_stat_free(buf);
  CAMLreturn(v_list);
}

/* Build a double-NUL-terminated UTF-16 environment block from an array of
   "KEY=VALUE" OCaml strings. The caller frees the result. */
static wchar_t *eio_build_env_block(value v_env)
{
  mlsize_t n = Wosize_val(v_env);
  size_t total = 1; /* trailing block terminator */
  wchar_t **parts = caml_stat_alloc(n * sizeof(wchar_t *));
  for (mlsize_t i = 0; i < n; i++) {
    parts[i] = caml_stat_strdup_to_utf16(String_val(Field(v_env, i)));
    total += wcslen(parts[i]) + 1;
  }
  wchar_t *block = caml_stat_alloc(total * sizeof(wchar_t));
  wchar_t *p = block;
  for (mlsize_t i = 0; i < n; i++) {
    size_t len = wcslen(parts[i]);
    wmemcpy(p, parts[i], len + 1); /* copy including NUL */
    p += len + 1;
    caml_stat_free(parts[i]);
  }
  *p = L'\0';
  caml_stat_free(parts);
  return block;
}

/* Spawn a child process with CreateProcessW. The three standard handles are
   duplicated as inheritable and restricted with PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
   so no other handles leak to the child. Returns [(pid, process_handle)]. */
CAMLprim value caml_eio_windows_spawn(value v_cwd, value v_env, value v_exe,
                                      value v_stdin, value v_stdout, value v_stderr,
                                      value v_cmdline)
{
  CAMLparam5(v_cwd, v_env, v_exe, v_stdin, v_stdout);
  CAMLxparam2(v_stderr, v_cmdline);
  CAMLlocal1(v_result);

  wchar_t *cmdline = NULL, *cwd = NULL, *exe = NULL, *env_block = NULL;
  HANDLE src[3];
  HANDLE dup[3] = { NULL, NULL, NULL };
  STARTUPINFOEXW si;
  PROCESS_INFORMATION pi;
  SIZE_T attr_size = 0;
  BOOL ok = FALSE;
  DWORD err = 0;
  DWORD create_flags = EXTENDED_STARTUPINFO_PRESENT | CREATE_UNICODE_ENVIRONMENT;
  HANDLE cur = GetCurrentProcess();

  memset(&si, 0, sizeof(si));
  memset(&pi, 0, sizeof(pi));

  cmdline = caml_stat_strdup_to_utf16(String_val(v_cmdline));
  if (caml_string_length(v_cwd) > 0) cwd = caml_stat_strdup_to_utf16(String_val(v_cwd));
  if (caml_string_length(v_exe) > 0) exe = caml_stat_strdup_to_utf16(String_val(v_exe));
  if (Wosize_val(v_env) > 0) env_block = eio_build_env_block(v_env);

  src[0] = Handle_val(v_stdin);
  src[1] = Handle_val(v_stdout);
  src[2] = Handle_val(v_stderr);

  /* Duplicate the standard handles as inheritable copies. */
  for (int i = 0; i < 3; i++) {
    if (!DuplicateHandle(cur, src[i], cur, &dup[i], 0, TRUE, DUPLICATE_SAME_ACCESS)) {
      err = GetLastError();
      goto cleanup;
    }
  }

  si.StartupInfo.cb = sizeof(STARTUPINFOEXW);
  si.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  si.StartupInfo.hStdInput  = dup[0];
  si.StartupInfo.hStdOutput = dup[1];
  si.StartupInfo.hStdError  = dup[2];

  if (!eio_has_console()) {
    /* The child may be a console program, but we have no console to share, so
       give it its own hidden one (matching the OCaml runtime's behaviour). */
    create_flags |= CREATE_NEW_CONSOLE;
    si.StartupInfo.dwFlags |= STARTF_USESHOWWINDOW;
    si.StartupInfo.wShowWindow = SW_HIDE;
  }

  /* Restrict inherited handles to exactly the three we duplicated. */
  InitializeProcThreadAttributeList(NULL, 1, 0, &attr_size);
  si.lpAttributeList = caml_stat_alloc(attr_size);
  if (!InitializeProcThreadAttributeList(si.lpAttributeList, 1, 0, &attr_size)) {
    err = GetLastError();
    caml_stat_free(si.lpAttributeList);
    si.lpAttributeList = NULL;
    goto cleanup;
  }
  if (!UpdateProcThreadAttribute(si.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                                 dup, 3 * sizeof(HANDLE), NULL, NULL)) {
    err = GetLastError();
    goto cleanup;
  }

  caml_enter_blocking_section();
  ok = CreateProcessW(exe, cmdline, NULL, NULL, TRUE,
                      create_flags, env_block, cwd, &si.StartupInfo, &pi);
  if (!ok) err = GetLastError();
  caml_leave_blocking_section();

cleanup:
  if (si.lpAttributeList) {
    DeleteProcThreadAttributeList(si.lpAttributeList);
    caml_stat_free(si.lpAttributeList);
  }
  for (int i = 0; i < 3; i++)
    if (dup[i]) CloseHandle(dup[i]);
  caml_stat_free(cmdline);
  if (cwd) caml_stat_free(cwd);
  if (exe) caml_stat_free(exe);
  if (env_block) caml_stat_free(env_block);

  if (!ok) {
    caml_win32_maperr(err);
    uerror("spawn", Nothing);
  }

  CloseHandle(pi.hThread);
  v_result = caml_alloc_tuple(2);
  Store_field(v_result, 0, Val_long(pi.dwProcessId));
  Store_field(v_result, 1, caml_win32_alloc_handle(pi.hProcess));
  CAMLreturn(v_result);
}

CAMLprim value caml_eio_windows_spawn_bytes(value *argv, int argn)
{
  (void)argn;
  return caml_eio_windows_spawn(argv[0], argv[1], argv[2], argv[3],
                                argv[4], argv[5], argv[6]);
}

/* ---- Pseudoterminal support (ConPTY) --------------------------------------

   The Windows Pseudo Console API (Windows 10 1809+) is resolved dynamically so
   the binary stays loadable on older systems: an absent [CreatePseudoConsole]
   surfaces as [EOPNOTSUPP] rather than a load-time failure. An [HPCON] is a heap
   object, not a kernel handle, so it cannot travel through a [Unix.file_descr];
   it lives in a custom block instead. */

/* [HPCON] is [VOID*]; we avoid depending on the header's declaration (which
   requires a recent [_WIN32_WINNT]) and keep our own opaque alias. */
typedef HRESULT (WINAPI *pCreatePseudoConsole)(COORD, HANDLE, HANDLE, DWORD, void **);
typedef HRESULT (WINAPI *pResizePseudoConsole)(void *, COORD);
typedef void    (WINAPI *pClosePseudoConsole)(void *);

#ifndef PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE
#define PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE 0x00020016
#endif

/* kernel32 exports, resolved once (a benign race just re-stores the same
   address). [tried] distinguishes "absent" from "not yet looked up". */
static pCreatePseudoConsole eio_CreatePseudoConsole(void)
{
  static pCreatePseudoConsole fn = NULL;
  static int tried = 0;
  if (!tried) { tried = 1; fn = (pCreatePseudoConsole)GetProcAddress(GetModuleHandleW(L"kernel32.dll"), "CreatePseudoConsole"); }
  return fn;
}

static pResizePseudoConsole eio_ResizePseudoConsole(void)
{
  static pResizePseudoConsole fn = NULL;
  static int tried = 0;
  if (!tried) { tried = 1; fn = (pResizePseudoConsole)GetProcAddress(GetModuleHandleW(L"kernel32.dll"), "ResizePseudoConsole"); }
  return fn;
}

static pClosePseudoConsole eio_ClosePseudoConsole(void)
{
  static pClosePseudoConsole fn = NULL;
  static int tried = 0;
  if (!tried) { tried = 1; fn = (pClosePseudoConsole)GetProcAddress(GetModuleHandleW(L"kernel32.dll"), "ClosePseudoConsole"); }
  return fn;
}

/* Map an [HRESULT] failure to an errno and raise. */
static void eio_hresult_error(HRESULT hr, const char *cmdname)
{
  DWORD err = (HRESULT_FACILITY(hr) == FACILITY_WIN32) ? (DWORD)HRESULT_CODE(hr) : (DWORD)hr;
  caml_win32_maperr(err);
  uerror(cmdname, Nothing);
}

/* A custom block owning an [HPCON]. The finalizer closes the console if it has
   not been closed explicitly; explicit close zeroes the pointer, so the two are
   idempotent (mirroring the runtime's handle pattern). */
#define Conpty_val(v) (*((void **)Data_custom_val(v)))

static void eio_conpty_finalize(value v)
{
  void *h = Conpty_val(v);
  if (h) {
    pClosePseudoConsole close_fn = eio_ClosePseudoConsole();
    if (close_fn) close_fn(h);
    Conpty_val(v) = NULL;
  }
}

/* Only [finalize] is provided; the block is never compared, hashed or
   marshalled, so the remaining operations stay NULL. */
static struct custom_operations eio_conpty_ops = {
  .identifier = "eio.windows.conpty",
  .finalize = eio_conpty_finalize,
  .compare = NULL,
  .hash = NULL,
  .serialize = NULL,
  .deserialize = NULL,
};

static value eio_alloc_conpty(void *h)
{
  value v = caml_alloc_custom(&eio_conpty_ops, sizeof(void *), 0, 1);
  Conpty_val(v) = h;
  return v;
}

/* Create the two pipe pairs that back a pty and return
   [(in_read, in_write, out_read, out_write)]. The child's input flows
   parent -> [in_write] -> [in_read] -> ConPTY; the child's output flows
   ConPTY -> [out_write] -> [out_read] -> parent. All ends are synchronous and
   non-inheritable, so their I/O runs on the systhread pool.

   (The design's single duplex-named-pipe master was tried first but ConPTY only
   ever emitted its init sequence and never rendered child output through a
   handle shared as both hInput and hOutput; two separate pipes — the fallback
   the design anticipated — render correctly.) */
CAMLprim value caml_eio_windows_open_pty_pipes(value v_unit)
{
  CAMLparam1(v_unit);
  CAMLlocal1(v_result);
  HANDLE in_read = NULL, in_write = NULL, out_read = NULL, out_write = NULL;
  DWORD err;
  /* Non-inheritable ends; a generous buffer so the console pump does not stall
     on a slow reader. */
  if (!CreatePipe(&in_read, &in_write, NULL, 65536)) {
    err = GetLastError();
    caml_win32_maperr(err);
    uerror("open_pty", Nothing);
  }
  if (!CreatePipe(&out_read, &out_write, NULL, 65536)) {
    err = GetLastError();
    CloseHandle(in_read); CloseHandle(in_write);
    caml_win32_maperr(err);
    uerror("open_pty", Nothing);
  }

  v_result = caml_alloc_tuple(4);
  Store_field(v_result, 0, caml_win32_alloc_handle(in_read));
  Store_field(v_result, 1, caml_win32_alloc_handle(in_write));
  Store_field(v_result, 2, caml_win32_alloc_handle(out_read));
  Store_field(v_result, 3, caml_win32_alloc_handle(out_write));
  CAMLreturn(v_result);
}

/* [conpty_create winsize in_read out_write] wraps CreatePseudoConsole, reading
   child input from [in_read] and writing child output to [out_write]. ConPTY
   duplicates both handles, so the caller may close its [in_read] copy
   afterwards. [winsize] is the OCaml record { rows; cols; xpixel; ypixel }. */
CAMLprim value caml_eio_windows_conpty_create(value v_size, value v_in_read, value v_out_write)
{
  CAMLparam3(v_size, v_in_read, v_out_write);
  COORD size;
  HANDLE in_read, out_write;
  void *hpc = NULL;
  HRESULT hr;
  pCreatePseudoConsole create_fn = eio_CreatePseudoConsole();
  if (!create_fn)
    caml_unix_error(EOPNOTSUPP, "open_pty", Nothing);

  size.Y = (SHORT)Long_val(Field(v_size, 0)); /* rows */
  size.X = (SHORT)Long_val(Field(v_size, 1)); /* cols */
  in_read = Handle_val(v_in_read);
  out_write = Handle_val(v_out_write);

  caml_enter_blocking_section();
  hr = create_fn(size, in_read, out_write, 0, &hpc);
  caml_leave_blocking_section();
  if (FAILED(hr))
    eio_hresult_error(hr, "open_pty");

  CAMLreturn(eio_alloc_conpty(hpc));
}

/* [conpty_resize conpty winsize] resizes the pseudoconsole. */
CAMLprim value caml_eio_windows_conpty_resize(value v_conpty, value v_size)
{
  CAMLparam2(v_conpty, v_size);
  COORD size;
  HRESULT hr;
  void *h = Conpty_val(v_conpty);
  pResizePseudoConsole resize_fn = eio_ResizePseudoConsole();
  if (!resize_fn)
    caml_unix_error(EOPNOTSUPP, "resize", Nothing);

  size.Y = (SHORT)Long_val(Field(v_size, 0)); /* rows */
  size.X = (SHORT)Long_val(Field(v_size, 1)); /* cols */
  hr = resize_fn(h, size);
  if (FAILED(hr))
    eio_hresult_error(hr, "resize");
  CAMLreturn(Val_unit);
}

/* [conpty_close conpty] closes the pseudoconsole explicitly (idempotent with the
   finalizer). ClosePseudoConsole can block draining the console, so release the
   runtime lock around it. */
CAMLprim value caml_eio_windows_conpty_close(value v_conpty)
{
  CAMLparam1(v_conpty);
  void *h = Conpty_val(v_conpty);
  if (h) {
    pClosePseudoConsole close_fn = eio_ClosePseudoConsole();
    if (close_fn) {
      caml_enter_blocking_section();
      close_fn(h);
      caml_leave_blocking_section();
    }
    Conpty_val(v_conpty) = NULL;
  }
  CAMLreturn(Val_unit);
}

/* Spawn a child attached to a pseudoconsole. Unlike [caml_eio_windows_spawn],
   the child inherits no explicit stdio handles (STARTF_USESTDHANDLES is unset):
   the pseudoconsole supplies stdin/stdout/stderr via the
   PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE attribute. Returns [(pid, process_handle)]. */
CAMLprim value caml_eio_windows_spawn_pty(value v_cwd, value v_env, value v_exe,
                                          value v_conpty, value v_cmdline)
{
  CAMLparam5(v_cwd, v_env, v_exe, v_conpty, v_cmdline);
  CAMLlocal1(v_result);

  wchar_t *cmdline = NULL, *cwd = NULL, *exe = NULL, *env_block = NULL;
  STARTUPINFOEXW si;
  PROCESS_INFORMATION pi;
  SIZE_T attr_size = 0;
  BOOL ok = FALSE;
  DWORD err = 0;
  DWORD create_flags = EXTENDED_STARTUPINFO_PRESENT | CREATE_UNICODE_ENVIRONMENT;
  void *hpc = Conpty_val(v_conpty);

  memset(&si, 0, sizeof(si));
  memset(&pi, 0, sizeof(pi));

  cmdline = caml_stat_strdup_to_utf16(String_val(v_cmdline));
  if (caml_string_length(v_cwd) > 0) cwd = caml_stat_strdup_to_utf16(String_val(v_cwd));
  if (caml_string_length(v_exe) > 0) exe = caml_stat_strdup_to_utf16(String_val(v_exe));
  if (Wosize_val(v_env) > 0) env_block = eio_build_env_block(v_env);

  si.StartupInfo.cb = sizeof(STARTUPINFOEXW);
  /* STARTF_USESTDHANDLES with NULL handles, rather than no std handles at all:
     when the launcher is itself console-less and its standard handles are pipes
     (as under a non-interactive Eio host), CreateProcessW otherwise propagates
     those pipe handles to the child, and the child writes to them instead of the
     pseudoconsole. Presenting NULL std handles suppresses that propagation so the
     pseudoconsole supplies the child's stdio. */
  si.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  si.StartupInfo.hStdInput = NULL;
  si.StartupInfo.hStdOutput = NULL;
  si.StartupInfo.hStdError = NULL;

  /* One attribute: the pseudoconsole to attach. */
  InitializeProcThreadAttributeList(NULL, 1, 0, &attr_size);
  si.lpAttributeList = caml_stat_alloc(attr_size);
  if (!InitializeProcThreadAttributeList(si.lpAttributeList, 1, 0, &attr_size)) {
    err = GetLastError();
    caml_stat_free(si.lpAttributeList);
    si.lpAttributeList = NULL;
    goto cleanup;
  }
  if (!UpdateProcThreadAttribute(si.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
                                 hpc, sizeof(hpc), NULL, NULL)) {
    err = GetLastError();
    goto cleanup;
  }

  caml_enter_blocking_section();
  /* bInheritHandles is FALSE: the pseudoconsole is passed via the attribute, not
     through handle inheritance. */
  ok = CreateProcessW(exe, cmdline, NULL, NULL, FALSE,
                      create_flags, env_block, cwd, &si.StartupInfo, &pi);
  if (!ok) err = GetLastError();
  caml_leave_blocking_section();

cleanup:
  if (si.lpAttributeList) {
    DeleteProcThreadAttributeList(si.lpAttributeList);
    caml_stat_free(si.lpAttributeList);
  }
  caml_stat_free(cmdline);
  if (cwd) caml_stat_free(cwd);
  if (exe) caml_stat_free(exe);
  if (env_block) caml_stat_free(env_block);

  if (!ok) {
    caml_win32_maperr(err);
    uerror("spawn", Nothing);
  }

  CloseHandle(pi.hThread);
  v_result = caml_alloc_tuple(2);
  Store_field(v_result, 0, Val_long(pi.dwProcessId));
  Store_field(v_result, 1, caml_win32_alloc_handle(pi.hProcess));
  CAMLreturn(v_result);
}

/* Block until the process referred to by [v_handle] exits, then return its
   exit code. Intended to be run in a worker systhread. */
CAMLprim value caml_eio_windows_process_wait(value v_handle)
{
  CAMLparam1(v_handle);
  HANDLE h = Handle_val(v_handle);
  DWORD code = 0;
  DWORD wait_res;
  caml_enter_blocking_section();
  wait_res = WaitForSingleObject(h, INFINITE);
  caml_leave_blocking_section();
  if (wait_res == WAIT_FAILED) {
    caml_win32_maperr(GetLastError());
    uerror("process_wait", Nothing);
  }
  if (!GetExitCodeProcess(h, &code)) {
    caml_win32_maperr(GetLastError());
    uerror("process_wait", Nothing);
  }
  /* [code] is an unsigned 32-bit DWORD; widen to [intnat] before [Val_long] so a
     crash exit code >= 0x80000000 (e.g. STATUS_ACCESS_VIOLATION) is reported as a
     non-negative OCaml int rather than sign-extended through a 32-bit [long]. */
  CAMLreturn(Val_long((intnat)(unsigned int)code));
}

/* Best-effort termination. Terminating a process that has already exited fails
   with an error we deliberately ignore (matching [Eio.Process.signal]). */
CAMLprim value caml_eio_windows_process_terminate(value v_handle, value v_code)
{
  CAMLparam2(v_handle, v_code);
  TerminateProcess(Handle_val(v_handle), (UINT)Long_val(v_code));
  CAMLreturn(Val_unit);
}

/* Map a GetAddrInfoW error code to the constructor index of
   Eio.Net.Getaddrinfo_error.t (the numbers must match its declaration order).
   On Windows the EAI_* macros in <ws2tcpip.h> are aliases for the WSA* codes
   that GetAddrInfoW returns directly. EAI_NODATA is itself defined as an alias
   of EAI_NONAME here, so we only ever emit the NONAME index (9), never the
   separate NODATA index (8). */
static int caml_eai_of_win(int eai)
{
  switch (eai) {
    case EAI_AGAIN:    return 2;   /* WSATRY_AGAIN */
    case EAI_BADFLAGS: return 3;   /* WSAEINVAL */
    case EAI_FAIL:     return 5;   /* WSANO_RECOVERY */
    case EAI_FAMILY:   return 6;   /* WSAEAFNOSUPPORT */
    case EAI_MEMORY:   return 7;   /* WSA_NOT_ENOUGH_MEMORY */
    case EAI_NONAME:   return 9;   /* WSAHOST_NOT_FOUND (also EAI_NODATA) */
    case EAI_SERVICE:  return 12;  /* WSATYPE_NOT_FOUND */
    case EAI_SOCKTYPE: return 13;  /* WSAESOCKTNOSUPPORT */
    default:           return 0;   /* UNKNOWN */
  }
}

/* Native getaddrinfo(3) for the Windows backend. Unlike OCaml's
   [Unix.getaddrinfo], this reports structured lookup failures (mapped to
   [Eio.Net.Getaddrinfo_error.t]) rather than returning an unclassified error or
   an empty list.

   Windows does not fill [ai_protocol], and a query with [ai_socktype = 0]
   returns a single result whose [ai_socktype] is also 0, so a lone query cannot
   distinguish TCP from UDP. We therefore issue one query per socket type and tag
   each result from the socket type we asked for. A lookup failure is reported
   only when *both* queries fail, so a service tied to a single protocol still
   yields that protocol's addresses.

   Node and service are converted to UTF-16 and passed to [GetAddrInfoW], for
   consistency with the other NT calls in this file. The C side stays dumb: it
   returns the numeric host as a string and the port as an int, and [net.ml]
   rebuilds the [Unix.sockaddr]. That avoids <caml/socketaddr.h>, which does not
   compile in this tree on Windows. Each result is [(is_stream, host, port)]; the
   list is built in reverse (stream results last-prepended first) and [net.ml]
   restores the original TCP-then-UDP order. */
CAMLprim value caml_eio_windows_getaddrinfo(value v_node, value v_service)
{
  CAMLparam2(v_node, v_service);
  CAMLlocal4(v_result, v_list, v_cons, v_item);
  CAMLlocal1(v_host);
  static const struct { int socktype; int is_stream; } queries[2] = {
    { SOCK_STREAM, 1 },
    { SOCK_DGRAM,  0 },
  };
  wchar_t *node = NULL;
  wchar_t *service = NULL;
  int errors[2] = { 0, 0 };
  int i;

  /* Duplicate the OCaml strings before releasing the runtime lock, since the GC
     may move them while [GetAddrInfoW] runs in the blocking section. */
  if (caml_string_length(v_node) > 0)
    node = caml_stat_strdup_to_utf16(String_val(v_node));
  if (caml_string_length(v_service) > 0)
    service = caml_stat_strdup_to_utf16(String_val(v_service));

  v_list = Val_emptylist;

  for (i = 0; i < 2; i++) {
    ADDRINFOW hints;
    ADDRINFOW *res = NULL, *item;
    int r;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = queries[i].socktype;

    caml_enter_blocking_section();
    r = GetAddrInfoW(node, service, &hints, &res);
    caml_leave_blocking_section();

    errors[i] = r;
    if (r != 0)
      continue;

    for (item = res; item; item = item->ai_next) {
      char host[NI_MAXHOST];
      const void *addr;
      int port;

      switch (item->ai_family) {
        case AF_INET: {
          struct sockaddr_in *ip = (struct sockaddr_in *) item->ai_addr;
          addr = &ip->sin_addr;
          port = ntohs(ip->sin_port);
          break;
        }
        case AF_INET6: {
          struct sockaddr_in6 *ip6 = (struct sockaddr_in6 *) item->ai_addr;
          addr = &ip6->sin6_addr;
          port = ntohs(ip6->sin6_port);
          break;
        }
        default:
          continue;
      }

      if (inet_ntop(item->ai_family, (void *)addr, host, sizeof(host)) == NULL)
        continue;

      v_host = caml_copy_string(host);

      v_item = caml_alloc(3, 0);
      Store_field(v_item, 0, Val_bool(queries[i].is_stream));
      Store_field(v_item, 1, v_host);
      Store_field(v_item, 2, Val_int(port));

      v_cons = caml_alloc(2, Tag_cons);
      Store_field(v_cons, 0, v_item);
      Store_field(v_cons, 1, v_list);
      v_list = v_cons;
    }

    FreeAddrInfoW(res);
  }

  if (node) caml_stat_free(node);
  if (service) caml_stat_free(service);

  /* Report a failure only when neither socket type resolved; otherwise return
     whatever addresses we did find. */
  if (errors[0] != 0 && errors[1] != 0) {
    v_result = caml_alloc(1, 1);        /* Error code */
    Store_field(v_result, 0, Val_int(caml_eai_of_win(errors[0])));
    CAMLreturn(v_result);
  }

  v_result = caml_alloc(1, 0);          /* Ok list */
  Store_field(v_result, 0, v_list);
  CAMLreturn(v_result);
}
