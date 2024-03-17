# MySQL updates columns from left to right

Assuming you have a MySQL table with two int columns `x` and `y` and try to update with this SQL:

    UPDATE t SET x = 0, y = x;

you will find both `x` and `y` become `0`. However, if you use this SQL:

    UPDATE t SET y = x, x = 0;

you will find `y` has the old value of `x` and `x` becomes `0`.

See [12.2.10. UPDATE Syntax](http://dev.mysql.com/doc/refman/5.1/en/update.html) for details.

On most other RDBs, both SQLs behave in the same manner as the latter.
