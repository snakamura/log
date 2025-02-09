# Regular expression to free `Alternative`, part 3

When you look at `matchSeq` in [the previous post](./free_alternative2.html), you'll find that we use `(<>)` of `Semigroup` only at a place. we can make `match` free from `Semigroup` if we can remove it. Let's revisit `matchSeq`.

```
matchSeq :: (Semigroup a) => RSeq a -> String -> [(a, String)]
matchSeq (REmpty a) s = [(a, s)]
matchSeq (RSeq char alt) s = do
  (a1, s1) <- matchChar char s
  (a2, s2) <- matchAlt alt s1
  pure (a1 <> a2, s2)
```

One of the ways to remove `(<>)` is making `a1` or `a2` a function. For example, we can call it like `a1 a2` instead of combining them like `a1 <> a2` if `a1` is a function `a -> a`. Actually, `a1` isn't necessarily `a -> a`, but can be `b -> a` if `a2` is `b`. They can be the opposite. We can call it like `a2 a1` instead of `a1 <> a2` if `a2` is `b -> a` and `a1` is `b`.

Let's take the latter approach and see how `RSeq` will look like.

```
data RSeq a where
  REmpty :: a -> RSeq a
  RSeq :: RChar b -> RAlt (b -> a) -> RSeq a
```

Now, the first part has `b` (`RChar b`), and the second part has `b -> a` (`RAlt (b -> a)`). As I wrote above, our new `matchSeq` will apply `a1` to `a2` instead of concatenating `a1` and `a2` using `(<>)`.

```
matchSeq :: RSeq a -> String -> [(a, String)]
matchSeq (REmpty a) s = [(a, s)]
matchSeq (RSeq char alt) s = do
  (a1, s1) <- matchChar char s
  (a2, s2) <- matchAlt alt s1
  pure (a2 a1, s2)
```

For example, you can write a regular expression `ab` like this.

```
regex :: Regex Int
regex = RAlt [RSeq (RChar 'a' 1) (RAlt [RSeq (RChar 'b' (+ 1)) (RAlt [REmpty id])])]
```

In this example, `match regex` returns `Just 1`.

You can control what it returns by attaching whatever function you like to `RSeq`. For example, `match regex'` returns `Just "There is b!"` by throwing away the value in `RChar 'a' 1`.

```
regex' :: Regex String
regex' = RAlt [RSeq (RChar 'a' 1) (RAlt [RSeq (RChar 'b' (const "There is b!")) (RAlt [REmpty id])])]
```

Again, it's hard to write these expression directly. Let's update the combinator functions. `rNever`, `rEmpty` and `rAlt` are simple.

```
rNever :: RAlt a
rNever = RAlt []

rEmpty :: a -> RAlt a
rEmpty a = RAlt [REmpty a]

rAlt :: RAlt a -> RAlt a -> RAlt a
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

infixl 3 `rAlt`
```

But `rSeq` is a bit complicated. `rSeq` will combine `RAlt (a -> b)` and `rAlt a` instead of `RAlt a` and `RAlt a`.

```
rSeq :: RAlt (a -> b) -> RAlt a -> RAlt b
rSeq (RAlt (seqs :: [RSeq (a -> b)])) (alt :: RAlt a) = RAlt $ concat [rSeq' seq alt | seq <- seqs]
  where
    rSeq' :: RSeq (a -> b) -> RAlt a -> [RSeq b]
    rSeq' (REmpty (f :: a -> b)) (RAlt (seqs' :: [RSeq a])) = map (f <$>) seqs'
    rSeq' (RSeq (c :: RChar c) (alt1 :: RAlt (c -> a -> b))) (alt2 :: RAlt a) =
      [ RSeq
          (c :: RChar c)
          ((((flip <$> (alt1 :: RAlt (c -> a -> b))) :: RAlt (a -> c -> b)) `rSeq` (alt2 :: RAlt a)) :: RAlt (c -> b))
      ]

infixl 4 `rSeq`
```

I added type annotations as much as possible to get the idea. After all, the first parameter is `a -> b` which consists of `c` and `c -> a -> b`, and the second parameter is `a`. We need to build a new structure whose first part is `c` and the second part is `c -> b`. It means that we need to flip `c -> a -> b` to get `a -> c -> b`, then apply `a` to get `c -> b`.

Also, we can build `Regex` for a character with `rChar`.

```
rChar :: Char -> RAlt Int
rChar c = RAlt [RSeq (RChar c 1) (rEmpty id)]
```

Can we write `regex` as ``rChar 'a' `rSeq` rChar 'b'`` again now? Unfortunately, no. `rSeq` combines `RAlt (a -> b)` and `RAlt a`, but both `rChar 'a'` and `rChar 'b'` is `RAlt Int`. We need to lift `rChar 'a'` to return `(+ 1)` instead of `1` by fmapping `(+)`.

```
regex :: Regex Int
regex = (+) <$> rChar 'a' `rSeq` rChar 'b'
```

You can also write it with `rEmpty`.

```
regex :: Regex Int
regex = rEmpty (+) `rSeq` rChar 'a' `rSeq` rChar 'b'
```

There is one more change. `rMany` no longer returns `RAlt a`, but returns `RAlt [a]` instead. You can apply any function to it by fmapping the function.

```
rMany :: RAlt a -> RAlt [a]
rMany r =
  let RAlt seqs = rSeq ((:) <$> r) (rMany r)
   in RAlt (REmpty [] : seqs)
```

Now, you can write `ab|(cd)*e` like this.

```
rChar' :: Char -> RAlt String
rChar' c = RAlt [RSeq (RChar c [c]) (rEmpty id)]

regex :: Regex String
regex = (<>) <$> rChar' 'a' `rSeq` rChar' 'b' `rAlt` ((<>) <$> concat <$> rMany ((<>) <$> rChar' 'c' `rSeq` rChar' 'd')) `rSeq` rChar' 'e'
```

`match regex "ab"` returns `Just "ab"`, and `match regex "cdcdcde"` returns `Just "cdcdcde"`.

Also, we can get a string that matched `(cd)*`.

```
regex' :: Regex String
regex' = const (const "") <$> rChar' 'a' `rSeq` rChar' 'b' `rAlt` (const <$> concat <$> rMany ((<>) <$> rChar' 'c' `rSeq` rChar' 'd')) `rSeq` rChar' 'e'
```

Now, `match regex' "ab"` returns `Just ""`, and `match regex' "cdcdcde"` returns `Just "cdcdcd"`.

You might've already found this, but `RAlt` can be an instance of [`Applicative`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Applicative.html#t:Applicative) and [`Alternative`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Applicative.html#t:Alternative) where `pure` is `rEmpty`, `(<*>)` is `rSeq`, `empty` is `rNever`, and `(<|>)` is `rAlt`.

```
data RChar a = RChar Char a deriving (Functor)

data RSeq a where
  REmpty :: a -> RSeq a
  RSeq :: RChar b -> RAlt (b -> a) -> RSeq a

deriving instance Functor RSeq

newtype RAlt a = RAlt [RSeq a] deriving (Functor)

instance Applicative RAlt where
  pure = rEmpty
  (<*>) = rSeq

instance Alternative RAlt where
  empty = rNever
  (<|>) = rAlt

type Regex = RAlt
```

Also, you can use [`many`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Applicative.html#v:many) instead of `rMany`. It'll become a bit shorter to write `ab|(cd)*e` with these operators.

```
regex :: Regex String
regex = (<>) <$> rChar' 'a' <*> rChar' 'b' <|> ((<>) <$> concat <$> many ((<>) <$> rChar' 'c' <*> rChar' 'd')) <*> rChar' 'e'
```

We'll make it a bit more generic in the next post.
