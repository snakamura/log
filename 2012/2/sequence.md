# Generating a sequence in a shell script

When you want to generate a sequence of numbers, you can use `seq` command. For example,

    for n in `seq 5 10`; do rm foo_$n.txt; done

will remove foo_5.txt, foo_6.txt ... foo_10.txt.

If you're using bash, you can use {n..m} instead. For example,

    rm foo_{5..10}.txt

will do the same operations as the previous example.
