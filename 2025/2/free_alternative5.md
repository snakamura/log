# Regular expression to free `Alternative`, part 5

To put them together, we got these types and functions. Note that I renamed `matchAlt` and `matchSeq` to `runAlt` and `runSeq` now that we can use them not only for matching.

```
import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Control.Monad.State (MonadState (..), StateT (..))
import Data.Function ((&))
import Data.Maybe (listToMaybe, mapMaybe)
import Prelude hiding (seq)

data RSeq f a where
  REmpty :: a -> RSeq f a
  RSeq :: f b -> RAlt f (b -> a) -> RSeq f a

deriving instance Functor (RSeq f)

newtype RAlt f a = RAlt [RSeq f a] deriving (Functor)

instance Applicative (RAlt f) where
  pure = rEmpty
  (<*>) = rSeq

instance Alternative (RAlt f) where
  empty = rNever
  (<|>) = rAlt

rNever :: RAlt f a
rNever = RAlt []

rEmpty :: a -> RAlt f a
rEmpty a = RAlt [REmpty a]

rSeq :: RAlt f (a -> b) -> RAlt f a -> RAlt f b
rSeq (RAlt (seqs :: [RSeq f (a -> b)])) (alt :: RAlt f a) = RAlt $ concat [rSeq' seq alt | seq <- seqs]
  where
    rSeq' :: RSeq f (a -> b) -> RAlt f a -> [RSeq f b]
    rSeq' (REmpty (f :: a -> b)) (RAlt (seqs' :: [RSeq f a])) = map (f <$>) seqs'
    rSeq' (RSeq (c :: f c) (alt1 :: RAlt f (c -> a -> b))) (alt2 :: RAlt f a) =
      [ RSeq
          (c :: f c)
          ((((flip <$> (alt1 :: RAlt f (c -> a -> b))) :: RAlt f (a -> c -> b)) `rSeq` (alt2 :: RAlt f a)) :: RAlt f (c -> b))
      ]

rAlt :: RAlt f a -> RAlt f a -> RAlt f a
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

runAlt :: (Alternative g) => (forall x. f x -> g x) -> RAlt f a -> g a
runAlt m (RAlt seqs) = foldr (\seq alt -> runSeq m seq <|> alt) empty seqs

runSeq :: (Alternative g) => (forall x. f x -> g x) -> RSeq f a -> g a
runSeq _ (REmpty a) = pure a
runSeq m (RSeq fa alt) = (&) <$> m fa <*> runAlt m alt
```

You can build `Regex` with your functor `RChar`,

```
data RChar a = RChar Char a deriving (Functor)

type Regex = RAlt RChar

rChar :: Char -> Regex String
rChar c = RAlt [RSeq (RChar c [c]) (rEmpty id)]
```

and match it,

```
match :: Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ runStateT (runAlt matchChar r) s
  where
    f (a, "") = Just a
    f _ = Nothing

matchChar :: RChar a -> StateT String [] a
matchChar (RChar rc a) = do
  c : cs <- get
  guard (rc == c)
  put cs
  pure a
```

or list it.

```
list :: Regex a -> [a]
list = runAlt listChar

listChar :: RChar a -> [a]
listChar (RChar _ a) = [a]
```

`RAlt` is called a free alternative because it makes any functor an instance of `Alternative`. You have `RChar` which is an instance of `Functor`, and you've got `RAlt RChar` which is an instance of `Alternative`.

This is similar to a list being called a free monoid. `[a]` will be an instance of `Monoid` no matter what `a` is. You can decide how you concatenate values later with a free monoid. For instance, when you have `1`, `2`, `3`, `4`, and `5`, and you haven't decided whether your going to add them or multiply them. You can put them in a list now, and decide what you'll do with it later.

You've got a sum with [`Sum`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Monoid.html#t:Sum), and got a product with [`Product`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Monoid.html#t:Product).

```
getSum $ mconcat [1, 2, 3, 4, 5] -- 15
getProduct $ mconcat [1, 2, 3, 4, 5] -- 120
```

The same goes for a free alternative. You can build `RAlt RChar a` based on a regular expression before deciding whether you're going to match it or get a list of strings that match it. You can later use `StateT String []` for matching, and use `[]` for listing.

You can say that a free structure allows you to delay your decisions.

Note that a free alternative is in [`free`](https://hackage.haskell.org/package/free) package as [`Control.Alternative.Free`](https://hackage.haskell.org/package/free-5.2/docs/Control-Alternative-Free.html) module where `RAlt` is called `Alt`, and `RSeq` is called `AltF`.
