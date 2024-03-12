# Decreasing the number of generations kept by logrotate won't delete old log files

When you decrease the number of generations that logrotate will keep by setting `rotate` parameter, to reduce the disk space used by logs, for example, you'll have to delete old log files by yourself.

For example, assume you have a configuration file containing

    rotate 10

there will be log files from foo.log, foo.log.1 ... foo.log.10. When you decrease the number to 5 like

    rotate 5

logrotate will remove foo.log.6, but won't remove foo.log.7 ... foo.log.10. Make sure to remove these files manually.
