# Changing limits with `prlimit`

You can change limits of a running process using `prlimit`.

For example, you can increase the maximum number of open files by

```
prlimit --pid=12345 --nofile=1048576
```

where `12345` is an id of the process.
