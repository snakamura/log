# When do you use `Data.Monoid.Endo`?

`Data.Monoid` module exports `Endo` which wraps `a -> a`. What's this?

When it comes to a `Monoid` instance related to a function, you might remember this instance (I merged `Semigroup` into `Monoid`) for simplicity.

```
instance Monoid b => Monoid (a -> b) where
    (f <> g) x = f x <> g x
    mempty = const mempty
```

This instance calls both functions with the same argument and apply `()` to the results.

On the other hand, a `Monoid` instance of `Endo` composes functions.

```
instance Monoid (Endo a) where
    Endo f <> Endo g = Endo $ f . g
    mempty = Endo id
```

For example, `x` is `[1, 3, 2, 3]`, but `y` is `[1, 2, 3]`.

```
x :: [Int]
x = ((1:)  (2:)) [3]

y :: [Int]
y = appEndo (Endo (1:)  Endo (2:)) [3]
```

So when do we use it? One of the use cases `Endo` becomes handy is building a list. Imagine you have a function `squash` like this. This function takes a list of `Monoid` values and concatenates them, then pass it to a function to get a final result.

```
squash :: Monoid a => [a] -> (a -> b) -> b
squash xs f = f (mconcat xs)
```

You can pass `[[1], [2], [3], ....]` to concatenate it like this.

```
slow :: [Int]
slow = squash (map (:[]) [1..10000000]) id
```

But this will run in quadratic time. By using `Endo`, you can avoid this quadratic complexity.

```
fast :: [Int]
fast = squash (map (Endo . (:)) [1..10000000]) (flip appEndo [])
```

This will construct a composed function like `(1:) . (2:) . (3:) . ...`, and passing an empty list to this function will return the final result.
