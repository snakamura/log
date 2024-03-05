# Playing with Sigma, part 2

So we have `Sigma` now. What can we do with it?

```
{-# LANGUAGE DataKinds,
             GADTs,
             EmptyCase,
             InstanceSigs,
             OverloadedStrings,
             PolyKinds,
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
import Data.Singletons.Sigma
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as T

singletons [d|
    data S = S1 | S2 | S3 | S4 deriving (Show, Eq)
    data T = T1 | T2 | T3 | T4 deriving (Show, Eq)
  |]

data X (s :: S) where
    X1 :: X 'S1
    X2 :: Int -> X 'S2
    X3 :: Text -> X 'S3
    X4 :: Float -> X 'S4

deriving instance Show (X s)

data Y (t :: T) = Y Text deriving Show
```

First, you can wrap `X` in it, of course.

```
x1, x2, x3, x4 :: Sigma S (TyCon X)
x1 = SS1 :&: X1
x2 = SS2 :&: X2 2
x3 = SS3 :&: X3 "3"
x4 = SS4 :&: X4 4
```

Then, you can project the type and the value.

```
proj1 :: Sigma S t -> Text
proj1 = projSigma1 f
  where
    f :: SS s -> Text
    f SS1 = "S1"
    f SS2 = "S2"
    f SS3 = "S3"
    f SS4 = "S4"

proj2 :: Sigma S (TyCon X) -> Text
proj2 = projSigma2 f
  where
    f :: X s -> Text
    f X1 = "X1"
    f (X2 n) = "X2 "  T.pack (show n)
    f (X3 t) = "X3 "  t
    f (X4 f) = "X4 "  T.pack (show f)
```

And you can convert it to another `Sigma` using `mapSigma`. To use `mapSigma`, you need three functions; a type function, a function on singletons and a function on values. Let's try converting `Sigma S (TyCon X)` to `Sigma T (TyCon Y)`.

First, you need a type function converting `S` to `T`.

```
type family Mf (s :: S ) :: T where
    Mf S1 = T1
    Mf S2 = T2
    Mf S3 = T3
    Mf S4 = T4
```

Also, you need defunctionalization symbols for it.

```
data MfSym0 :: S ~> T
type instance Apply MfSym0 s = Mf s
```

Second, you need a function on singletons.

```
sMf :: SS s -> ST (Mf s)
sMf SS1 = ST1
sMf SS2 = ST2
sMf SS3 = ST3
sMf SS4 = ST4
```

Instead of writing all of them by hand, you can use singletons to generate them.

```
singletons [d|
    mf :: S -> T
    mf S1 = T1
    mf S2 = T2
    mf S3 = T3
    mf S4 = T4
  |]
```

Finally, you need a function on values.

```
mg :: X s -> Y t
mg X1 = Y "X1"
mg (X2 n) = Y $ "X2 "  T.pack (show n)
mg (X3 t) = Y $ "X3 "  t
mg (X4 f) = Y $ "X4 "  T.pack (show f)
```

By combining them together, you can have a function converting `Sigma S (TyCon X)` to `Sigma T (TyCon Y)` using `mapSigma`.

```
m :: Sigma S (TyCon X) -> Sigma T (TyCon Y)
m = mapSigma (SLambda sMf :: SLambda MfSym0) mg
```
