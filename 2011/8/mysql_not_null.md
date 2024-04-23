# MySQL puts the default value to `NOT NULL` column

When you try to put `NULL` to a column with `NOT NULL`, MySQL puts the default value. Assuming you have a table created by this SQL,

    CREATE TABLE x (a INT NOT NULL);

and update the value with,

    UPDATE x SET a = NULL;

MySQL puts 0 instead of making it an error. It may cause a problem when you use a sub-query. For example,

    UPDATE x SET a = (SELECT id FROM y WHERE name = 'foo');

doesn't emit an error even when there is no row in table `y` whose name is "foo", and set `a` to 0 instead.

To make this an error, you can set sql mode to strict mode by:

    SET sql_mode = 'STRICT_ALL_TABLES';

You can also set this variable in [mysqld] section in my.cnf.

But care must be taken if you use the strict mode, because it affects other features. For example, it emits an error when you put a long string into a short column instead of truncating it. See [5.1.6. Server SQL Modes](http://dev.mysql.com/doc/refman/5.1/en/server-sql-mode.html) for details.
