# `Iterator::collect` can collect `Result`s

[Iterator::collect](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect) is often used to iterate values in an iterator and store them into a collection. For example, `print!("{:?}", (1..10).map(|n| n.to_string()).collect::<Vec<_>>())` will print `["1", "2", "3", "4", "5", "6", "7", "8", "9"]`.

You can also use `Iterator::collect` to `Iterator<Result<T, E>>` to `Result<V<T>, E>` where `V<T>` is a collection of `T`.

For example,

```
print!(
    "{:?}",
    (1..10)
        .map(|n| Ok(n))
        .collect::<Result<Vec<_>, std::io::Error>>()
);
```

prints `Ok([1, 2, 3, 4, 5, 6, 7, 8, 9])`.

When an iterator contains an error, it'll be collected to the first error.

```
print!(
    "{:?}",
    [
        Ok(1),
        Ok(2),
        Err(std::io::Error::other("error1")),
        Ok(4),
        Err(std::io::Error::other("error2"))
    ]
    .into_iter()
    .collect::<Result<Vec<_>, std::io::Error>>()
);
```

prints `Err(Custom { kind: Other, error: "error1" })`.

This works with `Option` as well.

```
print!(
    "{:?}",
    ["1", "2", "3", "4"]
        .iter()
        .map(|n| n.parse::<i32>().ok())
        .collect::<Option<Vec<_>>>()
);
```

prints `Some([1, 2, 3, 4])`, while

```
print!(
    "{:?}",
    ["1", "2", "x", "4"]
        .iter()
        .map(|n| n.parse::<i32>().ok())
        .collect::<Option<Vec<_>>>()
);
```
prints `None`.


You can think it as [`sequenceA`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Prelude.html#v:sequenceA) with `Either e` (or `Maybe`) in Haskell. Like `sequenceA (map Right [1..9])` becomes `Right [1,2,3,4,5,6,7,8,9]`, and `sequenceA (map (readMaybe @Int) ["1", "2", "x", "4"])` becomes `Nothing`.

The following snippet reads files specified by command-line arguments, splits content of each file into lines, then concatenates all lines (and prints them).

```
use std::env;
use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let lines = env::args()
        .skip(1)
        .map(|path| {
            Ok(
                fs::read_to_string(&path)?
                    .lines()
                    .map(String::from)
                    .collect::<Vec<_>>(), // Iterator<String> to Vec<String>
            )
        })
        .collect::<io::Result<Vec<_>>>()? // Iterator<io::Result<Vec<String>>> to io::Result<Vec<Vec<String>>>
        .into_iter()
        .flatten()
        .collect::<Vec<_>>(); // Iterator<String> to Vec<String>
    print!("{:?}", lines);
    Ok(())
}
```

As you can see, we use three `collect`s. Two to convert `Iterator<String>` to `Vec<String>`, and one to convert `Iterator<io::Result<Vec<String>>>` to `io::Result<Vec<Vec<String>>>`.

You can write similar in Haskell like this.

```
import Control.Exception
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  Right lines <-
    (fmap concat . sequenceA)
      <$> traverse
        ( \path ->
            fmap lines
              <$> try @IOError (readFile path)
        )
        args
  print lines
```
