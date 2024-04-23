# Reverse words with `tac`

Imagine you're writing a shell script and try to reverse the order of words in a variable. For example,

    a="abc 123 ABC"
    b=... # Do something here
    echo $b # This should print "ABC 123 abc"

Although there are many ways to do this with lots of tools, I like this one using `tac`.

    b=$(for x in $a; do echo $x; done | tac)

Or

    b=$(echo $a | tac -s ' ')

The latter one is a little tricky because the output of `echo $a | tac -s ' '` is

    ABC
    123 abc

because tac doesn't treat a new line as a separator. Even so, when you put it into a variable, you can get what you want.
