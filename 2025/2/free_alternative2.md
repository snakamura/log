# Regular expression to free `Alternative`, part 2

We defined `Regex` type to express a regular expression in [part 1](../1/free_alternative1.html). Now, how can we get more information from it? For example, how can I get a length of a matched string? The simplest way is defining a new matcher.

```
match :: Regex -> String -> Maybe Int
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f (n, "") = Just n
    f _ = Nothing

matchAlt :: RAlt -> String -> [(Int, String)]
matchAlt (RAlt seqs) s = concat [matchSeq seq s | seq <- seqs]

matchSeq :: RSeq -> String -> [(Int, String)]
matchSeq REmpty s = [(0, s)]
matchSeq (RSeq char alt) s = do
  (n1, s1) <- matchChar char s
  (n2, s2) <- matchAlt alt s1
  pure (n1 + n2, s2)

matchChar :: RChar -> String -> [(Int, String)]
matchChar (RChar rc) (c : cs)
  | rc == c = [(1, cs)]
  | otherwise = []
matchChar _ [] = []
```

As you can see, these functions now return a list of a pair of `Int` and `String` instead of a list of `String`. `Int` represents a length of a matched string while `String` represents a rest of the string.

This works, but we need to define another set of functions to get another information, for example, a matched string itself.

To avoid that, let's embed information we need into our `Regex` itself. We'll add a parameter of a generic type `a` to `RChar` to hold this information.

```
data RChar a = RChar Char a

data RSeq a = REmpty a | RSeq (RChar a) (RAlt a)

newtype RAlt a = RAlt [RSeq a]

type Regex = RAlt
```

We also update a set of functions to build `Regex`.

```
rNever :: RAlt a
rNever = RAlt []

rEmpty :: (Monoid a) => RAlt a
rEmpty = RAlt [REmpty mempty]

rSeq :: RAlt a -> RAlt a -> RAlt a
rSeq (RAlt seqs) alt = RAlt $ concat [rSeq' seq alt | seq <- seqs]
  where
    rSeq' :: RSeq a -> RAlt a -> [RSeq a]
    rSeq' (REmpty _) (RAlt seqs') = seqs'
    rSeq' (RSeq c1 (RAlt seqs')) alt' = [RSeq c1 (RAlt (rSeq' seq' alt')) | seq' <- seqs']

rAlt :: RAlt a -> RAlt a -> RAlt a
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

rMany :: (Monoid a) => RAlt a -> RAlt a
rMany r =
  let RAlt seqs = rSeq r (rMany r)
   in RAlt (REmpty mempty : seqs)
```

These functions are almost identical to the ones in the previous post, but have additional type parameter `a`. As you can see, I added `Monoid a` constraint to `rEmpty` and `rMany` so that we can build `REmpty` by passing `mempty`. We'll also use `Semigroup a` constraint to match `Regex` to concatenate a value stored in `RChar` in `matchSeq`.

```
match :: (Semigroup a) => Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f (a, "") = Just a
    f _ = Nothing

matchAlt :: (Semigroup a) => RAlt a -> String -> [(a, String)]
matchAlt (RAlt seqs) s = concat [matchSeq seq s | seq <- seqs]

matchSeq :: (Semigroup a) => RSeq a -> String -> [(a, String)]
matchSeq (REmpty a) s = [(a, s)]
matchSeq (RSeq char alt) s = do
  (a1, s1) <- matchChar char s
  (a2, s2) <- matchAlt alt s1
  pure (a1 <> a2, s2)

matchChar :: RChar a -> String -> [(a, String)]
matchChar (RChar rc a) (c : cs)
  | rc == c = [(a, cs)]
  | otherwise = []
matchChar _ [] = []
```

Finally, we define `rChar` variants to build `RChar` with a desired value. For example, you can use this `rChar` to get length of a matched string. We always bind `1` to `RChar` because `RChar` always matches a single character.

```
rChar :: Char -> Regex (Sum Int)
rChar c = RAlt [RSeq (RChar c 1) rEmpty]

regex :: Regex
regex = (rChar 'a' `rSeq` rChar 'b') `rAlt` (rMany (rChar 'c' `rSeq` rChar 'd') `rSeq` rChar 'e')
```

You can find that `getSum <$> match regex "cdcdcde"` returns `Just 7`.

With `rChar'` which binds a string containing the character itself to `RChar`, you can get a matched string itself. This time, we bind `[c]` to it because `RChar` matches this string.

```
rChar' :: Char -> Regex String
rChar' c = RAlt [RSeq (RChar c [c]) rEmpty]

regex' :: Regex
regex' = (rChar' 'a' `rSeq` rChar' 'b') `rAlt` (rMany (rChar' 'c' `rSeq` rChar' 'd') `rSeq` rChar' 'e')
```

This time, `match regex' "cdcdcde" returns `Just "cdcdcde".

Now we can get information from `Regex` using the same match function if the information is `Monoid`. But this means that we always concatenate information in `RChar`. How can we get information with more flexibilities? For example, how can we get a string that matched a part of a regular expression? Let's see what we can do in the next post.
