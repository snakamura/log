# Files in /etc/cron.d

According to `man cron`:

> /etc/crontab and the files in /etc/cron.d must be owned by root, and must not be group- or other-writable. In contrast to the spool area, the files under /etc/cron.d or the files under /etc/cron.hourly, /etc/cron.daily, /etc/cron.weekly and /etc/cron.monthly may also be symlinks, provided that both the symlink and the file it points to are owned by root.

So when put a symlink in `/etc/cron.d` and find that it doesn't work, you should check its owner. In this case, you'd find a line like this in `/var/log/syslog`.

    Jun 16 23:20:38 misc cron[5894]: (*system*test) WRONG FILE OWNER (/etc/cron.d/test)
