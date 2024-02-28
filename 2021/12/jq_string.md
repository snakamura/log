# Convert a text to a JSON string with jq

When you want to read a text from somewhere and make it a JSON string in a shell script, `jq` helps.

```
$ printf 'foo
"bar"
baz\boo
' | jq -R --slurp .
"foo\n\"bar\"\nbaz\boo"
```
