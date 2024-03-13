# Killing a thread or query of MySQL running on RDS

When you run a MySQL instance on RDS and want to kill a thread or a query for some reason, you'll find you cannot use `KILL` or `mysqladmin kill` because you don't have a permission to do so.

RDS provides the stored procedures named mysql.rds_kill and mysql.rds_kill_query which will kill a thread and a query respectively. To kill a thread, first use `SHOW PROCESSLIST` to get the list of threads and find the id of the thread you want to kill. Assuming the thread id is 53512, then use `CALL mysql.rds_kill(53512)` to kill the thread.
