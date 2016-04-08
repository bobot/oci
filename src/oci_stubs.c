/**************************************************************************/
/*                                                                        */
/*  This file is part of OCI.                                             */
/*                                                                        */
/*  Copyright (C) 2015-2016                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/


#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>

/** This function or alloc_process_status should be exported by
    ocaml runtime */
CAMLextern int caml_rev_convert_signal_number(int);

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static value alloc_process_status(int pid, int status, value ru)
{
  CAMLparam1(ru);
  CAMLlocal2(st,res);

  if (WIFEXITED(status)) {
    st = alloc_small(1, TAG_WEXITED);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  }
  else if (WIFSTOPPED(status)) {
    st = alloc_small(1, TAG_WSTOPPED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
  }
  else {
    st = alloc_small(1, TAG_WSIGNALED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WTERMSIG(status)));
  }
  res = alloc_small(3, 0);
  Field(res, 0) = Val_int(pid);
  Field(res, 1) = st;
  Field(res, 2) = ru;
  CAMLreturn(res);
}

static int wait_flag_table[] = {
  WNOHANG, WUNTRACED
};

CAMLprim value oci_wait4(value flags, value pid_req)
{
  CAMLparam0();
  CAMLlocal1(v_usage);
  int pid, status, cv_flags;
  struct rusage ru;

  cv_flags = convert_flag_list(flags, wait_flag_table);
  enter_blocking_section();
  pid = wait4(Int_val(pid_req), &status, cv_flags, &ru);
  leave_blocking_section();
  if (pid == -1) uerror("wait4", pid_req);

  v_usage = caml_alloc(16, 0);
  Store_field(v_usage, 0,
              caml_copy_double((double) ru.ru_utime.tv_sec +
                               (double) ru.ru_utime.tv_usec / 1e6));
  Store_field(v_usage, 1,
              caml_copy_double((double) ru.ru_stime.tv_sec +
                               (double) ru.ru_stime.tv_usec / 1e6));
  Store_field(v_usage, 2, caml_copy_int64(ru.ru_maxrss));
  Store_field(v_usage, 3, caml_copy_int64(ru.ru_ixrss));
  Store_field(v_usage, 4, caml_copy_int64(ru.ru_idrss));
  Store_field(v_usage, 5, caml_copy_int64(ru.ru_isrss));
  Store_field(v_usage, 6, caml_copy_int64(ru.ru_minflt));
  Store_field(v_usage, 7, caml_copy_int64(ru.ru_majflt));
  Store_field(v_usage, 8, caml_copy_int64(ru.ru_nswap));
  Store_field(v_usage, 9, caml_copy_int64(ru.ru_inblock));
  Store_field(v_usage, 10, caml_copy_int64(ru.ru_oublock));
  Store_field(v_usage, 11, caml_copy_int64(ru.ru_msgsnd));
  Store_field(v_usage, 12, caml_copy_int64(ru.ru_msgrcv));
  Store_field(v_usage, 13, caml_copy_int64(ru.ru_nsignals));
  Store_field(v_usage, 14, caml_copy_int64(ru.ru_nvcsw));
  Store_field(v_usage, 15, caml_copy_int64(ru.ru_nivcsw));

  CAMLreturn(alloc_process_status(pid, status,v_usage));
}
