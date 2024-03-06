# Functions taking some types, part 1

Imagine you have type X indexed by a phantom type S.

```
{-# LANGUAGE DataKinds,
             EmptyCase,
             FlexibleContexts,
             GADTs,
             InstanceSigs,
             MultiParamTypeClasses,
             PolyKinds,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             UndecidableInstances
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind
    ( Constraint
    , Type
    )
import Data.Singletons.Prelude
    ( Elem
    , If
    )
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as T

singletons [d|
    data S = S1 | S2 | S3 | S4 deriving (Show, Eq)
  |]

data X :: S -> Type where
    X1 :: X 'S1
    X2 :: Int -> X 'S2
    X3 :: Text -> X 'S3
    X4 :: Float -> X 'S4

deriving instance Show (X s)
```

You want to define a function taking `X` with some of `S`. For example, that function takes `X 'S2` and `X 'S3`, but doesn't take `X 'S1` nor `X 'S4`. How do you write this function?

The simplest approach would be to use `Either`. Your function will take `Either (X 'S2) (X 'S3)`.

```
f1 :: Either (X 'S2) (X 'S3) -> Text
f1 (Left (X2 n)) = T.pack $ show n
f1 (Right (X3 t)) = t
```

You can call it by wrapping your `X` by `Left` or `Right`.

```
c1 :: Text
c1 = f1 $ Left $ X2 2
```

This is pretty straightforward, but you need to nest `Either` when you make this function take `X 'S4` too, which is a bit annoying.

How about using a constraint? You can define a type function (type family) of kind `S -> Constraint` and use it as a constraint of the function.

```
type family F2 s :: Constraint where
    F2 'S2 = ()
    F2 'S3 = ()
    F2 _ = ('True ~ 'False)

f2 :: F2 s => X s -> Text
f2 (X2 n) = T.pack $ show n
f2 (X3 t) = t

c2 :: Text
c2 = f2 $ X2 2
```

As you know, applying a type constraint to a function is the same thing as passing a table explicitly. You'll define a GADT of kind `S -> Type` (instead of defining a type constraint of kind `S -> Constraint`) to do this.

```
data F3 :: S -> Type where
    F32 :: F3 'S2
    F33 :: F3 'S3

f3 :: F3 s -> X s -> Text
f3 F32 (X2 n) = T.pack $ show n
f3 F33 (X3 t) = t

c3 :: Text
c3 = f3 F32 $ X2 2
```

In this approach, you need to pass `F3` explicitly to `f3` whereas it was implicitly passed to `f2`. Can we make it implicit? Yes, first, let's define `Proved` type class.

```
class Proved p a where
    auto :: p a
```

Then, make `F3` instances of this class.

```
instance Proved F3 'S2 where
    auto = F32

instance Proved F3 'S3 where
    auto = F33
```

Then, you can always pass `auto` as its first parameter and the complier will find a proper instance.

```
f3' :: Proved F3 s => X s -> Text
f3' = f3 auto

c3' :: Text
c3' = f3' $ X2 2
```

`f3'` is a bit generic version of `f2` but they do the same thing.

If you think it's cumbersome to write `F2` or `F3`, you could make a type of the function take a list of types directly by writing a helper constraint `OneOf`.

```
type family OneOf t l :: Constraint where
    OneOf t l = If (Elem t l) (() :: Constraint) ('True ~ 'False)

f4 :: OneOf s '[ 'S2, 'S3 ] => X s -> Text
f4 (X2 n) = T.pack $ show n
f4 (X3 t) = t

c4 :: Text
c4 = f4 $ X2 2
```
