# PostgreSQL doesn’t create an index on a column with a foreign key while MySQL does

When you create two tables with a foreign key on MySQL, it creates an index on the column referring the foreign key.

```
CREATE TABLE p (id INT PRIMARY KEY);
CREATE TABLE c (id INT PRIMARY KEY, p INT NOT NULL, FOREIGN KEY (p) REFERENCES p (id));
```

```
SHOW INDEXES FROM c;
+-------+------------+----------+--------------+-------------+-----------+-------------+----------+--------+------+------------+---------+---------------+
| Table | Non_unique | Key_name | Seq_in_index | Column_name | Collation | Cardinality | Sub_part | Packed | Null | Index_type | Comment | Index_comment |
+-------+------------+----------+--------------+-------------+-----------+-------------+----------+--------+------+------------+---------+---------------+
| c     |          0 | PRIMARY  |            1 | id          | A         |           0 |     NULL | NULL   |      | BTREE      |         |               |
| c     |          1 | p        |            1 | p           | A         |           0 |     NULL | NULL   |      | BTREE      |         |               |
+-------+------------+----------+--------------+-------------+-----------+-------------+----------+--------+------+------------+---------+---------------+
2 rows in set (0.00 sec)
```

You can find that the index `p` is created automatically.

But you'll find PostgreSQL doesn't do this by just creating these two tables.

```
\d c;
                 Table "public.c"
 Column |  Type   | Collation | Nullable | Default
--------+---------+-----------+----------+---------
 id     | integer |           | not null |
 p      | integer |           | not null |
Indexes:
    "c_pkey" PRIMARY KEY, btree (id)
Foreign-key constraints:
    "c_p_fkey" FOREIGN KEY (p) REFERENCES p(id)
```

This means that, by default, PostgreSQL scans `c` instead of using an index when you, for example, delete a row from `p` and PostgreSQL updates or deletes rows in `c` cascadingly.

Of course, you can create an index by yourself.

```
CREATE INDEX c_p ON c (p);
```

From https://www.postgresql.org/docs/12/ddl-constraints.html#DDL-CONSTRAINTS-FK
> Since a DELETE of a row from the referenced table or an UPDATE of a referenced column will require a scan of the referencing table for rows matching the old value, it is often a good idea to index the referencing columns too. Because this is not always needed, and there are many choices available on how to index, declaration of a foreign key constraint does not automatically create an index on the referencing columns.
