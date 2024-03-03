# Calculating sum on console

Imagine you have a file containing a number in each line and you want to calculate sum of them. You can do this using `awk`, but you can also do this using `paste` and `bc`.

```
cat test.txt | paste -s -d+ | bc
```

For example, this outputs `55`.

```
$ paste -s -d+ <<END | bc
1
2
3
4
5
6
7
8
9
10
END
55
```
