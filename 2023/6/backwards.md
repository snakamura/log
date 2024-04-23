# `Control.Applicative.Backwards`

`Control.Applicative` has a type [`Backwards`](https://hackage.haskell.org/package/transformers/docs/Control-Applicative-Backwards.html) which applies actions in the reverse order.

Let's see how it reverses the order. Here is a simple example of printing `a` and `b` using `Applicative`.

```
import Control.Applicative
import Control.Applicative.Backwards
import Data.Foldable

main :: IO ()
main = do
    putStrLn "a" *> putStrLn "b"
```

Let's put it in `Backwards`. Don't forget to apply `forwards` to get `IO ()` from `Backwards IO ()`.

```
forwards $ Backwards (putStrLn "a") *> Backwards (putStrLn "b")
```

First, make it redundant to use `` instead of `*>` without using ``.

```
forwards $ Backwards (pure (\_ _ -> ()))  Backwards (putStrLn "a")  Backwards (putStrLn "b")
```

Then, expand `` in `Backwards` to get two nested `liftA2`s.

```
forwards $ Backwards (putStrLn "a"  pure (\_ _ -> ()))  Backwards (putStrLn "b")
forwards $ Backwards (putStrLn "b"  (putStrLn "a"  pure ((\_ _ -> ()))))
forwards $ Backwards (putStrLn "b"  liftA2 (\a f -> f a) (putStrLn "a") (pure (\_ _ -> ())))
forwards $ Backwards (liftA2 (\a f -> f a) (putStrLn "b") (liftA2 (\a f -> f a) (putStrLn "a") (pure (\_ _ -> ()))))
```

Since the `Applicative` instance of `IO` performs actions from left to right, this prints `b` then `a`.

You can use it something like this to reverse the order of actions.

```
-- Forward
traverse_ print [1 .. 10 :: Int]
-- Backward
forwards $ traverse_ (Backwards . print) [1 .. 10 :: Int]
```
