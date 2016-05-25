/* From Janestreet core core_unix */
/* Core_unix support functions written in C. */

#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/wait.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/threads.h>

#define PIPE_READ 0
#define PIPE_WRITE 1

/* Note: the ARG_MAX defined in sys/limits.h can be huge */
#define ML_ARG_MAX (4096 + 1)

#define MAX_ERROR_LEN 4096

extern char **environ;

static void report_error(int fd, const char* str)
{
  char buf[MAX_ERROR_LEN];
  char buf2[MAX_ERROR_LEN];
#ifdef __GLIBC__
  snprintf(buf2, MAX_ERROR_LEN, "%s (%s)\n", str,
           strerror_r(errno, buf, MAX_ERROR_LEN));
#else
  if (strerror_r(errno, buf, MAX_ERROR_LEN) == -1)
    snprintf(buf, MAX_ERROR_LEN, "Unknown error %d", errno);
  snprintf(buf2, MAX_ERROR_LEN, "%s (%s)\n", str, buf);
#endif
  buf2[MAX_ERROR_LEN - 1] = '\0';
  if (write(fd, buf2, strlen(buf2))) {}
  /* The returned value from the above write is ignored.
     This is fine because we are about to exit(254).

     But newer versions of gcc warn, so we enclose the expression with
     `if(..){}'. Note: simply casting to (void) is not sufficient to
     suppress the warning. */
}

/* Close function that handles signals correctly by retrying the close
   after EINTR.

   NOTE: we should never see EIO when closing pipes.  If so, it is
   reasonable to see this as a kernel bug, and it's pretty useless trying
   to catch/work around potential kernel bugs.  We assume that it works.
   An EBADF would be bad when closing successfully opened pipes, too, but
   in that case the pipe should be guaranteed to be closed anyway (unlike
   EIO).  This covers all errors that close could potentially return.
*/
static inline int safe_close(int fd)
{
  int ret;
  while ((ret = close(fd)) == -1 && errno == EINTR) /* empty loop */ ;
  return ret;
}

/* Idempotent version of safe_close: doesn't flag EBADF as an error. */
static inline int safe_close_idem(int fd)
{
  int ret = safe_close(fd);
  return (ret == -1 && errno == EBADF) ? 0 : ret;
}


CAMLprim value oci_ml_create_process(value v_working_dir, value v_prog,
                                 value v_args,
                                 value v_env, value v_search_path)
{
  /* No need to protect the arguments or other values: we never release
     the O'Caml lock, and we never use O'Caml values after other values
     get allocated in the O'Caml heap. */
  typedef enum { READ_END = 0, WRITE_END = 1 } pipe_end_t;
  value v_res;
  int stdin_pfds[2];
  int stdout_pfds[2];
  int stderr_pfds[2];
  int child_pid;
  int my_errno;

  /* It's ok to hold pointers into the O'Caml heap, since the memory
     space gets duplicated upon the fork, during which we keep the
     O'Caml lock. */
  char *prog = String_val(v_prog);
  int search_path = Bool_val(v_search_path);

  /* We use a statically allocated, fixed-size array for performance
     reasons.  It is reasonable to assume that the array can never become
     too big for stack allocations anyway. */
  char *args[ML_ARG_MAX];
  int n_args = Wosize_val(v_args);

  char *working_dir = NULL;

  /* Note that the executable name also counts as an argument, and we
     also have to subtract one more for the terminating NULL! */
  if (n_args >= ML_ARG_MAX - 1)
    caml_failwith("too many arguments for Unix.create_process");

  args[0] = prog;
  args[n_args + 1] = NULL;

  while (n_args) {
    args[n_args] = String_val(Field(v_args, n_args - 1));
    --n_args;
  }

  if (pipe(stdin_pfds) == -1)
    uerror("create_process: parent->stdin pipe creation failed", Nothing);

  if (pipe(stdout_pfds) == -1) {
    my_errno = errno;
    safe_close(stdin_pfds[READ_END]);
    safe_close(stdin_pfds[WRITE_END]);
    unix_error(my_errno,
               "create_process: stdout->parent pipe creation failed", Nothing);
  }

  if (pipe(stderr_pfds) == -1) {
    my_errno = errno;
    safe_close(stdin_pfds[READ_END]);
    safe_close(stdin_pfds[WRITE_END]);
    safe_close(stdout_pfds[READ_END]);
    safe_close(stdout_pfds[WRITE_END]);
    unix_error(my_errno,
               "create_process: stderr->parent pipe creation failed", Nothing);
  }

  /* This function deliberately doesn't release the O'Caml lock (i.e. it
     doesn't call caml_enter_blocking_section) during the fork.  This is
     because we hold pointers into the ML heap across a fork, and
     releasing the lock immediately before the fork could theoretically
     cause the GC to run and move blocks before the fork duplicates the
     memory space.

     If the parent process has threads that turn out to suffer from too
     much latency during this fork, we may want to rewrite this function
     to copy the O'Caml values into the C heap before the fork and release
     the O'Caml lock.  It seems unlikely that forks will ever take so
     long that people care.  In Linux 2.6 forks are practically constant
     time even in the presence of ridiculous amounts of processes, and
     are reported to always have less than 500us latency.  Maybe the
     kernel does not even schedule threads during forks anyway.  */
  if ((child_pid = fork()) == 0) {
    /* Child process. */

    /* Just in case any of the pipes' file descriptors are 0, 1 or 2
       (not inconceivable, especially when running as a daemon),
       duplicate all three descriptors we need in the child to fresh
       descriptors before duplicating them onto stdin, stdout and stderr.

       It is in fact the case that none of [temp_stdin], [temp_stdout] and
       [temp_stderr] will never be 0, 1, or 2.  That this is so follows from
       the following three properties:

       1. The kernel always allocates the lowest-numbered unused fd when
          asked for a new one.

       2. We allocated more than two fds during the calls to [pipe] above.

       3. We have not closed any fds between the calls to [pipe] above and
          this point.  */
    int temp_stdin = dup(stdin_pfds[READ_END]);
    int temp_stdout = dup(stdout_pfds[WRITE_END]);
    int temp_stderr = dup(stderr_pfds[WRITE_END]);
    if (temp_stdin == -1 || temp_stdout == -1 || temp_stderr == -1) {
      /* Errors here and below are sent back to the parent on the
         stderr pipe. */
      report_error(stderr_pfds[WRITE_END],
                   "could not dup fds in child process");
      /* The open fds will be cleaned up by exit(); likewise below.
         We use 254 to avoid any clash with ssh returning 255. */
      exit(254);
    }

    /* We are going to replace stdin, stdout, and stderr for this child
       process so we close the existing descriptors now.  They may be
       closed already so we ignore EBADF from close. */
    /* We don't have to do this ([dup2] closes the newfd if it is open
       before splatting the oldfd on it), but maybe we get a more
       informative error message if there is a problem closing a descriptor
       rather than with the dup operation itself. */
    if (safe_close_idem(STDIN_FILENO) == -1
        || safe_close_idem(STDOUT_FILENO) == -1
        || safe_close_idem(STDERR_FILENO) == -1) {
      report_error(temp_stderr,
                   "could not close standard descriptors in child process");
      exit(254);
    }

    /* All pipe fds propagated from parent to child via fork() must be
       closed, otherwise the reference counts on those fds won't drop
       to zero (and cause the pipe to be broken) when the parent closes
       them. */
    safe_close(stdin_pfds[READ_END]);
    safe_close(stdin_pfds[WRITE_END]);
    safe_close(stdout_pfds[READ_END]);
    safe_close(stdout_pfds[WRITE_END]);
    safe_close(stderr_pfds[READ_END]);
    safe_close(stderr_pfds[WRITE_END]);

    /* We must dup2 after closing the pfds, because the latter might
       have been standard descriptors. */
    if (dup2(temp_stdin, STDIN_FILENO) == -1
        || dup2(temp_stdout, STDOUT_FILENO) == -1
        || dup2(temp_stderr, STDERR_FILENO) == -1) {
      report_error(temp_stderr, "could not dup2 fds in child process");
      exit(254);
    }

    safe_close(temp_stdin);
    safe_close(temp_stdout);
    safe_close(temp_stderr);

    /* We don't bother saving/restoring the environment or freeing the
       new one since we exit the process in case of error. */
    environ = cstringvect(v_env);

    if (Is_block(v_working_dir))
      working_dir = String_val(Field(v_working_dir, 0));
    if (working_dir && chdir(working_dir) == -1) {
      report_error(STDERR_FILENO, "chdir failed in child process");
      exit(254);
    }

    if ((search_path ? execvp : execv)(prog, args) == -1) {
      report_error(STDERR_FILENO, "execvp/execv failed in child process");
      exit(254);
    }
  }

  my_errno = errno;

  /* Parent process. */

  /* Close the ends of the pipes that we [the parent] aren't going to use. */
  safe_close(stdin_pfds[READ_END]);
  safe_close(stdout_pfds[WRITE_END]);
  safe_close(stderr_pfds[WRITE_END]);

  /* Set the ends we are going to use to close on exec, so the next child we
   * fork doesn't get an unwanted inheritance. */
  close_on_exec(stdin_pfds[WRITE_END]);
  close_on_exec(stdout_pfds[READ_END]);
  close_on_exec(stderr_pfds[READ_END]);

  /* If the fork failed, cause the pipes to be destroyed and fail. */
  if (child_pid == -1) {
    safe_close(stdin_pfds[WRITE_END]);
    safe_close(stdout_pfds[READ_END]);
    safe_close(stderr_pfds[READ_END]);
    unix_error(my_errno, "create_process: failed to fork", Nothing);
  }

  /* Must use Field as an lvalue after caml_alloc_small -- not Store_field. */
  v_res = caml_alloc_small(4, 0);
  Field(v_res, 0) = Val_int(child_pid);
  Field(v_res, 1) = Val_int(stdin_pfds[WRITE_END]);
  Field(v_res, 2) = Val_int(stdout_pfds[READ_END]);
  Field(v_res, 3) = Val_int(stderr_pfds[READ_END]);

  return v_res;
}
