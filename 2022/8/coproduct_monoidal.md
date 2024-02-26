# Coproduct monoidal functor

In [the previous post](https://snak.tumblr.com/post/691339270827491328/monoidal-functor), we defined a monoidal functor that preserves a product of types. In the same way, we can define a monoidal structure using coproduct with `Void` as an identity. It satisfies `Either (Either a b) c ≡ Either a (Either b c)`, `Either a Void ≡ a`, and `Either Void a ≡ a`.

Just like `Monoidal` in the previous post, we can define `Monoidal` for this structure.

```
{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables,
             UndecidableInstances
#-}

import Data.Void (Void)

class Functor f => Monoidal f where
    mu :: Either (f a) (f b) -> f (Either a b)
    epsilon :: Void -> f Void

instance Monoidal Maybe where
    mu :: Either (Maybe a) (Maybe b) -> Maybe (Either a b)
    mu (Left (Just a)) = Just (Left a)
    mu (Right (Just b)) = Just (Right b)
    mu _ = Nothing

    epsilon :: Void -> Maybe Void
    epsilon _ = Nothing
```

Unfortunately, there seems no interesting correspondent functor like `Applicative`, though.
