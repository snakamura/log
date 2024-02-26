# Coproduct to product monoidal functor

In the previous two posts, we've seen monoidal functors that preserve product and coproduct. Then, what does it look like if we define a functor that preserves monidal structure in different forms?

Let's say we have coproduct monoidal structure in a source category, and product monoidal structure in a target category. To make a functor from the source category to the target category [a monoidal functor](https://ncatlab.org/nlab/show/monoidal+functor), we need a morphism `epsilon` converting a unit in the target category to a mapped unit in the source category, and a natural transformation `mu` converting a monoidal product of mapped objects in the target category to a mapped monoidal product in the source category.

```
{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables,
             UndecidableInstances
#-}

import Control.Applicative (Alternative(..))
import Data.Void (Void, absurd)

class Functor f => Monoidal f where
    mu :: (f a, f b) -> f (Either a b)
    epsilon :: () -> f Void
```

For example, you can implement this type class for `Maybe`.

```
instance Monoidal Maybe where
    mu :: (Maybe a, Maybe b) -> Maybe (Either a b)
    mu (Just a, _) = Just (Left a)
    mu (_, Just b) = Just (Right b)
    mu (_, _) = Nothing

    epsilon :: () -> Maybe Void
    epsilon () = Nothing
```

It turns out that this monoidal functor is equivalent to `Alternative` functor. So you can implement `Alternative` in terms of `Monoidal`.

```
instance (Functor f, Monoidal f, Applicative f) => Alternative f where
instance (Functor f, Monoidal f, Applicative f) => Alternative f where
    (<|>) :: f a -> f a -> f a
    x <|> y = either id id <$> mu (x, y)

    empty :: f a
    empty = absurd <$> epsilon ()
```
