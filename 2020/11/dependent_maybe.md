# Implementing `Maybe` using dependent types

Let's try implementing `Maybe` using dependent types. This is useless in practice, but it'll give us some intuitions about how to use them.

```
{-# LANGUAGE DataKinds,
             DeriveFunctor,
             EmptyCase,
             GADTs,
             InstanceSigs,
             KindSignatures,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind (Type)
import Data.Singletons ( SLambda(SLambda))
import Data.Singletons.Prelude
    ( FlipSym1
    , Id
    , IdSym0
    , sId
    )
import Data.Singletons.Sigma
    ( Sigma((:&:))
    , mapSigma
    , projSigma2
    )
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
```

First, we need a kind that tells us whether it has a value or not. We call it `O` (for `Optional`) which has types `S` (for `Some`) and `N` (for `None`).

```
singletons [d|
    data O = S | N deriving (Show, Eq)
  |]
```

Then we'll have a type indexed by kind `O`.

```
data Optional (o :: O) (a :: Type) where
    Some :: a -> Optional 'S a
    None :: Optional 'N a

deriving instance Show a => Show (Optional o a)
```

We'll use `Optional 'S a` for `Just`, and `Optional 'N a` for `Nothing`. Note that we have two distinct types instead of one.

Okay, we're going to implement some functions using `Optional`. First, let's define a function converting a text to an integer. It returns `Optional 'S Int` when the text can be converted to an integer, and returns `Optional 'N Int` otherwise.

```
textToInt :: Text -> Sigma O (FlipSym1 (TyCon Optional) @@ Int)
textToInt t | Just n <- readMaybe $ T.unpack t = SS :&: Some n
            | otherwise = SN :&: None
```

The return type is a bit complicated. It returns `Sigma` with a type indexed by `O` which is `Optional`. So technically, it’s `Sigma O (Optional o Int)`. But we need to flip type parameters of `Optional`, and convert it to defunctionalized symbols.

Using this function, we can write a function that adds a text to an integer. This function returns `Optional 'S Int` if the second parameter can be converted to an integer, and returns `Optional 'N Int` otherwise.

```
addText :: Int -> Text -> Sigma O (FlipSym1 (TyCon Optional) @@ Int)
addText n t = projSigma2 f (textToInt t)
  where
    f :: Optional o Int -> Sigma O (FlipSym1 (TyCon Optional) @@ Int)
    f (Some m) = SS :&: Some (n + m)
    f None = SN :&: None
```

We get `Optional` from `Sigma` returned from `textToInt` using `projSigma2`, then adding the first parameter if it's `Some`.

Are there anything we can do to make this simpler? First, let's define `Opt` to make the type signature simpler.

```
type Opt a = Sigma O (FlipSym1 (TyCon Optional) @@ a)
```

Since `addText` maps a `Sigma` returned by `textToInt` to another `Sigma`, we should be able to write it using `mapSigma`. As I wrote in [the previous post](../10/sigma2.html), we need a type function `G`, its singleton version `sG` and a value function `f` to use `mapSigma`.

```
singletons [d|
    g :: O -> O
    g S = S
    g N = N
  |]

addText' :: Int -> Text -> Opt Int
addText' n t = mapSigma (SLambda sG :: SLambda GSym0) f (textToInt t)
  where
    f :: Optional o Int -> Optional (G o) Int
    f (Some m) = Some $ n + m
    f None = None
```

As you can see, `G` is an identity type function. So we should be able to use `Id` in `Data.Singleton.Prelude`.

```
addText'' :: Int -> Text -> Opt Int
addText'' n t = mapSigma (SLambda sId :: SLambda (IdSym0 :: O ~> O)) f (textToInt t)
  where
    f :: Optional o Int -> Optional (Id o) Int
    f (Some m) = Some $ n + m
    f None = None
```

Also, you can find `f` is just `fmap` on `Optional`.  So once we get a `Functor` instance for `Optional`, we can use it.

```
deriving instance Functor (Optional o)

addText''' :: Int -> Text -> Opt Int
addText''' n t = mapSigma (SLambda sId :: SLambda (IdSym0 :: O ~> O))
                          (fmap (+ n) :: Optional o Int -> Optional (Id o) Int)
                          (textToInt t)
```

It's annoying that we need to add some type annotations, but unfortunately, we cannot omit them.
