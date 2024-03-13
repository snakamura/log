# Never call mysql_real_connect more than once without calling mysql_init again

I spent a couple of hours digging into a problem that a program using MySQL C API sometimes used a wrong character set. The code was something like:

    MYSQL mysql;
    mysql_init(&mysql);
    mysql_options(&mysql, MYSQL_READ_DEFAULT_GROUP, "test");
    while (!mysql_real_connect(&mysql, host, user, password, dbname, port, NULL, 0)) {
        sleep(3);
    }

The problem was that when `mysql_real_connect` failed, it called `mysql_close` inside that cleared all options set by `mysql_options`. Since we use `default-character-set` in my.cnf to set the character set to utf-8, it used latin-1 when it didn't read my.cnf.

It almost always worked fine because `mysql_real_connect` succeeded, but it sometimes used a wrong character set when the first attempt to connect to the server failed.

When `mysql_real_connect` fails, you have to repeat from `mysql_init`.
