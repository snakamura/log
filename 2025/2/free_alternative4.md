# Regular expression to free `Alternative`, part 4

Before making `RSeq` and `RAlt` in the previous post more generic, let's revisit our match functions.

```
match :: Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f (a, "") = Just a
    f _ = Nothing

matchAlt :: RAlt a -> String -> [(a, String)]
matchAlt (RAlt seqs) s = concat [matchSeq seq s | seq <- seqs]

matchSeq :: RSeq a -> String -> [(a, String)]
matchSeq (REmpty a) s = [(a, s)]
matchSeq (RSeq char alt) s = do
  (a1, s1) <- matchChar char s
  (a2, s2) <- matchAlt alt s1
  pure (a2 a1, s2)

matchChar :: RChar a -> String -> [(a, String)]
matchChar (RChar rc a) (c : cs)
  | rc == c = [(a, cs)]
  | otherwise = []
matchChar _ [] = []
```

As you can see, types of `matchAlt`, `matchSeq` and `matchChar` are `f a -> String -> [(a, String)]` for some type `f`. They take a state (`String`), and return a list of a pair of a value (`a`) and a new state (`String`). Yes, it's a state monad on a list monad. It means that we can write them as `f a -> StateT String [] a`. Let's rewrite them.

`matchChar` is the simplest. You can use [`get`](https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Monad-Trans-State-Lazy.html#v:get) and [`put`](https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Monad-Trans-State-Lazy.html#v:put) to work with a state.

```
matchChar :: RChar a -> StateT String [] a
matchChar (RChar rc a) = do
  c : cs <- get
  guard (rc == c)
  put cs
  pure a
```

`matchSeq` has already used a list monad, so let's make it handle state implicitly.

```
matchSeq :: RSeq a -> StateT String [] a
matchSeq (REmpty a) = pure a
matchSeq (RSeq char alt) = do
  a1 <- matchChar char
  a2 <- matchAlt alt
  pure (a2 a1)
```

You can write it in Applicative-style, too.

```
matchSeq :: RSeq a -> StateT String [] a
matchSeq (REmpty a) = pure a
matchSeq (RSeq char alt) = (&) <$> matchChar char <*> matchAlt alt
```

The last one is `matchAlt`. You'll run each state in `RAlt` and concatenate them.

```
matchAlt :: RAlt a -> StateT String [] a
matchAlt (RAlt seqs) = StateT $ \s -> concatMap (\seq -> runStateT (matchSeq seq) s) seqs
```

Or, you can take advantage of `Alternative` instance of `StateT`.

```
matchAlt :: RAlt a -> StateT String [] a
matchAlt (RAlt seqs) = foldr (\seq states -> matchSeq seq <|> states) empty seqs
```

`match` runs a state monad with a given `String` and get a result.

```
match :: Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ runStateT (matchAlt r) s
  where
    f (a, "") = Just a
    f _ = Nothing
```

Writing these functions using `StateT` itself doesn't mean anything, but this will take on the meaning later.

Now, let's revisit `RSeq` and `RAlt`. Even though they work only with `RChar` now, but they don't depend on `RChar` when we build them. I mean, `pure` (`rEmpty`), `(<*>)` (`rSeq`), `empty` (`rNever`), and `(<|>)` (`rAlt`) don't use properties of `RChar`. This means that we can add one more type parameter of kind `Type -> Type` to `RSeq` and `RAlt` and use it instead of `RChar`.

```
data RSeq f a where
  REmpty :: a -> RSeq f a
  RSeq :: f b -> RAlt f (b -> a) -> RSeq f a

deriving instance Functor (RSeq f)

newtype RAlt f a = RAlt [RSeq f a] deriving (Functor)
```

What changes should we need to make `matchAlt` and `matchSeq` work with any `f`? These functions cannot call `matchChar` directly now because `matchChar` works with `RChar`. Instead, we can pass `matchChar` as a parameter to these functions.

```
matchAlt m (RAlt seqs) = foldr (\seq alt -> matchSeq m seq <|> alt) empty seqs

matchSeq _ (REmpty a) = pure a
matchSeq m (RSeq fa alt) = (&) <$> m fa <*> matchAlt m alt
```

`match` will pass `matchChar` to them.

```
match :: Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ runStateT (matchAlt matchChar r) s
  where
    f (a, "") = Just a
    f _ = Nothing
```

The question is, what type `matchAlt` and `matchSeq` will be. The concrete type of `matchChar` was `RChar a -> StateT String [] a`. Are these types generic enough?

```
matchAlt :: (f a -> StateT String [] a) -> RAlt f a -> StateT String [] a
matchSeq :: (f a -> StateT String [] a) -> RSeq f a -> StateT String[] a
```

Unfortunately, these types don't work because we need `m` to be polymorphic so that we can apply it to the first part of `RSeq` whose type is `b`, and the second part whose type is `b -> a`. Let's make it polymorphic.

```
matchAlt :: (forall x. f x -> StateT String [] x) -> RAlt f a -> StateT String [] a
matchSeq :: (forall x. f x -> StateT String [] x) -> RSeq f a -> StateT String[] a
```

But when you look at the implementations of `matchAlt` and `matchSeq`, you'll find that they only use methods of `Applicative` and `Alternative`. They should work not only with `StateT String []`, but with any `Alternative`. Their types will be these types finally.

```
matchAlt :: (Alternative g) => (forall x. f x -> g x) -> RAlt f a -> g a
matchSeq :: (Alternative g) => (forall x. f x -> g x) -> RSeq f a -> g a
```

This means that `matchAlt` can turn `RAlt f a` to any `g a` where `g` is an instance of `Alternative`.

For example, let's define a function `listChar` of type `RChar a -> [a]`. This type matches `(Alternative g) => f x -> g x`.

```
listChar :: RChar a -> [a]
listChar (RChar _ a) = [a]
```

With a regular expression `regex` which expresses `(a|b)c|d(e|f)g`,

```
regex :: Regex String
regex = (<>) <$> (rChar 'a' <|> rChar 'b') <*> rChar 'c' <|> (\a b c -> a <> b <> c) <$> rChar 'd' <*> (rChar 'e' <|> rChar 'f') <*> rChar 'g'
```

`matchAlt listChar regex` returns `["ac","bc","deg","dfg"]`. This is a list which will match `regex`. We can use `regex` to match a regular expression as well as list strings that match it. Unfortunately, `matchAlt listChar regex` doesn't work when `regex` contains `many` because it produces an infinite number of strings.

We'll put everything together in [the next post](./free_alternative5.html).
