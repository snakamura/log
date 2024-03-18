# Using WIFSTOPPED with waitpid

It's common to use `WIFSTOPPED` with `waitpid` when the parent process wait for a child process to stop and then the parent process sends `SIGCONT` to the child process to continue processing.

For this purpose, we sometimes see this kind of code:

    while ($pid != ($res_pid = waitpid($pid, WUNTRACED)) || not WIFSTOPPED($?)) {
      last if -1 == $res_pid && not kill 0 => $pid;
      sleep 1;
    }

But passing `WIFSTOPPED` to `$?` is actually incorrect and it may not work on some environment. You should pass `${^CHILD_ERROR_NATIVE}` instead.

    while ($pid != ($res_pid = waitpid($pid, WUNTRACED)) || not WIFSTOPPED(${^CHILD_ERROR_NATIVE})) {
      ..
    }
