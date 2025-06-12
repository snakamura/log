# Regular expression to free `Alternative`, part 1

When you express a regular expression in Haskell, the first option would be a recursive datatype with a function to build `*`.

```
data Regex
  = Never
  | Empty
  | Char Char
  | Seq Regex Regex
  | Alt Regex Regex
  deriving (Show)

many :: Regex -> Regex
many r = Empty `Alt` (r `Seq` many r)
```

`ab|(cd)*e` can be expressed like this with them, for example.

```
regex :: Regex
regex = (Char 'a' `Seq` Char 'b') `Alt` (many (Char 'c' `Seq` Char 'd') `Seq` Char 'e')
```

You'd usually convert it to NFA/DFA to write a matcher, but this time, we'll write a matcher directly.

```
match :: Regex -> String -> Bool
match r s = elem "" $ match' r s

match' :: Regex -> String -> [String]
match' Never _ = []
match' Empty s = [s]
match' (Char rc) (c : cs)
  | rc == c = [cs]
  | otherwise = []
match' (Char _) "" = []
match' (Seq r1 r2) s = match' r1 s >>= match' r2
match' (Alt r1 r2) s = match' r1 s <> match' r2 s
```

As you can see, `match` returns `True` only when a string matches a regular expression completely. `match'` consumes a string if it matches a regular expression and returns possible inputs that will be fed to the rest of the regular expression.

This is pretty straightforward, but this representation allows you to write the same regular expression in different ways. For example, `a|b|c` can be ``(Char 'a' `Alt` Char 'b') `Alt` Char 'c'`` or ``Char 'a' `Alt` (Char 'b' `Alt` Char 'c')``.

How can we express it in a normalized way? You can split this datatype into several pieces and combine them to build a type for a regular expression.

```
newtype RChar = RChar Char deriving (Show)

data RSeq = REmpty | RSeq RChar RAlt deriving (Show)

newtype RAlt = RAlt [RSeq] deriving (Show)

type Regex = RAlt
```

A regular expression will be a list of `RSeq` which is either empty or a character followed by a regular expression. `a|b|c` will be `RAlt [RSeq (RChar 'a') (RAlt [REmpty]), RSeq (RChar 'b') (RAlt [REmpty]), RSeq (RChar 'c') (RAlt [REmpty])]`.

It's good, but it's hard to write it directly. Let's introduce some functions to build it more easily.

```
rNever :: Regex
rNever = RAlt []

rEmpty :: Regex
rEmpty = RAlt [REmpty]

rChar :: Char -> Regex
rChar c = RAlt [RSeq (RChar c) rEmpty]

rSeq :: Regex -> Regex -> Regex
rSeq (RAlt seqs) alt = RAlt $ concat [rSeq' seq alt | seq <- seqs]
  where
    rSeq' :: RSeq -> RAlt -> [RSeq]
    rSeq' REmpty (RAlt seqs') = seqs'
    rSeq' (RSeq c1 (RAlt seqs')) alt' = [RSeq c1 (RAlt (rSeq' seq' alt')) | seq' <- seqs']

rAlt :: Regex -> Regex -> Regex
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

rMany :: Regex -> Regex
rMany r =
  let RAlt seqs = rSeq r (rMany r)
   in RAlt (REmpty : seqs)
```

Each of these functions (except `rMany`) corresponds to a constructor of the first version of `Regex`. You can now write `a|b|c` as ``rChar 'a' `rAlt` rChar 'b' `rAlt` rChar 'c'``. Note that both ``(rChar 'a' `rAlt` rChar 'b') `rAlt` rChar 'c'`` and ``rChar 'a' `rAlt` (rChar 'b' `rAlt` rChar 'c')`` produce the same expression.

Now, `ab|(cd)*e` can be expressed like this with them.

```
regex :: Regex
regex = (rChar 'a' `rSeq` rChar 'b') `rAlt` (rMany (rChar 'c' `rSeq` rChar 'd') `rSeq` rChar 'e')
```

A matcher is similar to the previous one except that we need a function for `RChar`, `RSeq` and `RAlt` separately instead of having multiple branches in a function.

```
match :: Regex -> String -> Bool
match r s = elem "" $ matchAlt r s

matchAlt :: RAlt -> String -> [String]
matchAlt (RAlt seqs) s = concat [matchSeq seq s | seq <- seqs]

matchSeq :: RSeq -> String -> [String]
matchSeq REmpty s = [s]
matchSeq (RSeq char alt) s = matchChar char s >>= matchAlt alt

matchChar :: RChar -> String -> [String]
matchChar (RChar rc) (c : cs)
  | rc == c = [cs]
  | otherwise = []
matchChar _ [] = []
```

In [the next post](../2/free_alternative2.html), we'll see how we can get more information when matching a regular expression.
