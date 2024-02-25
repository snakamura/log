# zsh supports process substitution using a temporary file

This prints 0,

```
stat -c "%s" <(cat <<END
foo
END
)
```

while this prints 4.

```
stat -c "%s" =(cat <<END
foo
END
)
```

This is handy when you pass something to a command where the command seeks on it.
